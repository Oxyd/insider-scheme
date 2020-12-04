#include "vm.hpp"

#include "action.hpp"
#include "io.hpp"
#include "numeric.hpp"

#include <cassert>
#include <cstdint>
#include <stdexcept>
#include <vector>

namespace insider {

#ifndef NDEBUG
static constexpr std::size_t root_stack_initial_size = 0;
#else
static constexpr std::size_t root_stack_initial_size = 4096;
#endif

root_stack::root_stack()
  : data_{std::make_unique<object*[]>(root_stack_initial_size)}
  , alloc_{root_stack_initial_size}
{
  std::fill(&data_[0], &data_[alloc_], nullptr);
}

void
root_stack::change_allocation(std::size_t new_alloc) {
  if (new_alloc > alloc_) {
    alloc_ = new_alloc;

    auto old_data = std::move(data_);
    data_ = std::make_unique<object*[]>(alloc_);
    std::copy(&old_data[0], &old_data[0] + size_, &data_[0]);
  }
}

void
root_stack::grow(std::size_t n) {
  assert(size_ + n <= alloc_);

  std::fill(&data_[size_], &data_[size_ + n], nullptr);
  size_ += n;
}

void
root_stack::shrink(std::size_t n) {
  size_ -= n;
}

void
root_stack::resize(std::size_t new_size) {
  if (new_size > size_)
    grow(new_size - size_);
  else
    shrink(size_ - new_size);
}

void
root_stack::erase(std::size_t begin, std::size_t end) {
  std::copy(&data_[0] + end, &data_[0] + size_, &data_[0] + begin);
  size_ -= end - begin;
}

void
root_stack::trace(tracing_context& tc) const {
  for (std::size_t i = 0; i < size_; ++i)
    tc.trace(data_[i]);
}

void
root_stack::update_references() {
  for (std::size_t i = 0; i < size_; ++i)
    update_reference(data_[i]);
}

static constexpr std::size_t call_stack_growth_constant = 1024;

call_stack::call_stack()
  : frames_{std::make_unique<frame[]>(call_stack_growth_constant)}
  , alloc_{call_stack_growth_constant}
{ }

auto
call_stack::push(insider::procedure* proc, std::size_t stack_top) -> frame& {
  if (size_ >= alloc_)
    grow(alloc_ + call_stack_growth_constant);

  assert(size_ < alloc_);
  return frames_[size_++] = frame{proc->entry_pc, proc, stack_top, operand{}, nullptr};
}

auto
call_stack::push_native(named_native_procedure* proc) -> frame& {
  if (size_ >= alloc_)
    grow(alloc_ + call_stack_growth_constant);

  return frames_[size_++] = frame{0, nullptr, 0, operand{}, proc};
}

void
call_stack::trace(tracing_context& tc) const {
  for (std::size_t i = 0; i < size_; ++i)
    tc.trace(frames_[i].procedure);
}

void
call_stack::update_references() {
  for (std::size_t i = 0; i < size_; ++i)
    update_reference(frames_[i].procedure);
}

void
call_stack::grow(std::size_t new_alloc) {
  auto old_frames = std::move(frames_);

  frames_ = std::make_unique<frame[]>(new_alloc);
  std::copy(&old_frames[0], &old_frames[0] + size_, &frames_[0]);

  alloc_ = new_alloc;
}

execution_state::execution_state(context& ctx)
  : ctx{ctx}
  , value_stack{make_tracked<root_stack>(ctx)}
  , call_stack{make_tracked<insider::call_stack>(ctx)}
{ }

static std::vector<object*>
collect_closure(closure* cls) {
  std::vector<object*> result;
  result.reserve(cls->size());
  for (std::size_t i = 0; i < cls->size(); ++i)
    result.push_back(cls->ref(i));

  return result;
}

static void
pop_call_frame(execution_state& state) {
  std::size_t num_locals = state.call_stack->current_frame().procedure->locals_size;
  state.call_stack->pop();
  state.value_stack->shrink(num_locals);
}

object*
call_frame_local(execution_state& state, operand local) {
  return state.value_stack->ref(state.call_stack->current_frame().stack_top + local);
}

template <int Arity, std::size_t... Is>
object*
do_call(execution_state& state, native_procedure<Arity>* proc, std::size_t num_args, std::index_sequence<Is...>) {
  call_stack::frame& frame = state.call_stack->current_frame();
  bytecode const& bc = state.ctx.program;

  // Consume operands even if call is invalid.
  [[maybe_unused]] std::array<operand, Arity> arg_operands{((void) Is, read_operand(bc, frame.pc))...};

  if (Arity != num_args)
    throw error{"{}: Wrong number of arguments, expected {}, got {}", proc->name, Arity, num_args};

  [[maybe_unused]] std::size_t stack_top = frame.stack_top;

  state.call_stack->push_native(proc);
  object* result = proc->target(state.ctx, state.value_stack->ref(stack_top + arg_operands[Is])...);
  state.call_stack->pop();
  return result;
}

static object*
do_call_generic(execution_state& state, object* proc, std::size_t num_args) {
  call_stack::frame& frame = state.call_stack->current_frame();
  bytecode const& bc = state.ctx.program;
  auto native_proc = assume<native_procedure<-1>>(proc);

  std::vector<object*> args;
  args.reserve(num_args);
  for (std::size_t i = 0; i < num_args; ++i)
    args.push_back(state.value_stack->ref(frame.stack_top + read_operand(bc, frame.pc)));

  state.call_stack->push_native(native_proc);
  object* result = native_proc->target(state.ctx, args);
  state.call_stack->pop();
  return result;
}

template <int Arity>
object*
call_native_helper(execution_state& state, object* proc, std::size_t num_args) {
  if (num_args == Arity) {
    if (is<native_procedure<Arity>>(proc))
      return do_call<Arity>(state, assume<native_procedure<Arity>>(proc), num_args, std::make_index_sequence<Arity>{});
    else
      return do_call_generic(state, proc, num_args);
  } else
    return call_native_helper<Arity - 1>(state, proc, num_args);
}

template <>
object*
call_native_helper<-1>(execution_state& state, object* proc, std::size_t num_args) {
  return do_call_generic(state, proc, num_args);
}

static object*
call_native(execution_state& state, object* proc, std::size_t num_args) {
  if (num_args <= max_specialised_arity)
    return call_native_helper<max_specialised_arity>(state, proc, num_args);
  else
    return do_call_generic(state, proc, num_args);
}

template <int Arity, std::size_t... Is>
object*
do_call_vector(context& ctx, native_procedure<Arity>* proc, std::vector<object*> const& args,
               std::index_sequence<Is...>) {
  if (Arity != args.size())
    throw error{"{}: Wrong number of arguments, expected {}, got {}", proc->name, Arity, args.size()};

  return proc->target(ctx, args[Is]...);
}


template <int Arity>
object*
call_native_vector_helper(context& ctx, object* proc, std::vector<object*> const& args) {
  if (is<native_procedure<Arity>>(proc))
    return do_call_vector<Arity>(ctx, assume<native_procedure<Arity>>(proc), args, std::make_index_sequence<Arity>{});
  else
    return call_native_vector_helper<Arity - 1>(ctx, proc, args);
}

template <>
object*
call_native_vector_helper<-1>(context& ctx, object* proc, std::vector<object*> const& args) {
  auto native_proc = assume<native_procedure<-1>>(proc);
  return native_proc->target(ctx, args);
}

static object*
call_native_vector(context& ctx, object* proc, std::vector<object*> const& args) {
  return call_native_vector_helper<max_specialised_arity>(ctx, proc, args);
}

execution_state
make_state(context& ctx, procedure* global,
           std::vector<object*> const& closure, std::vector<object*> const& arguments) {
  execution_state result{ctx};
  result.call_stack->push(global, 0);

  result.value_stack->change_allocation(global->locals_size);
  result.value_stack->grow(global->locals_size);

  std::size_t closure_size = closure.size();
  for (std::size_t i = 0; i < closure_size; ++i)
    result.value_stack->set(i, closure[i]);

  for (std::size_t i = 0; i < arguments.size(); ++i)
    result.value_stack->set(closure_size + i, arguments[i]);

  return result;
}

namespace {
  class execution_action : public action<execution_action> {
  public:
    execution_action(execution_state& state)
      : action{state.ctx}
      , state_{state}
    { }

    ~execution_action() { check(); }

    std::string
    format() const {
      std::string result;
      for (std::size_t i = state_.call_stack->size(); i > 0; --i) {
        if (i != state_.call_stack->size())
          result += '\n';

        auto const& frame = state_.call_stack->get(i - 1);
        if (frame.procedure) {
          std::optional<std::string> name = frame.procedure->name;
          result += fmt::format("in {}", name ? *name : "<lambda>");
        } else {
          assert(frame.native_procedure);
          result += fmt::format("in native procedure {}", frame.native_procedure->name);
        }
      }

      return result;
    }

  private:
    execution_state& state_;
  };
}

generic_tracked_ptr
run(execution_state& state) {
  execution_action a(state);

  while (true) {
    gc_disabler no_gc{state.ctx.store};

    call_stack::frame& frame = state.call_stack->current_frame();
    root_stack& values = *state.value_stack;
    bytecode const& bc = state.ctx.program;

    auto local = [&] (operand l) { return values.ref(frame.stack_top + l); };
    auto set_local = [&] (operand l, object* value) { values.set(frame.stack_top + l, value); };

    opcode op = read_opcode(bc, frame.pc);
    switch (op) {
    case opcode::no_operation:
      break;

    case opcode::load_static: {
      operand static_num = read_operand(bc, frame.pc);
      operand dest = read_operand(bc, frame.pc);
      set_local(dest, state.ctx.get_static(static_num));
      break;
    }

    case opcode::load_top_level: {
      operand global_num = read_operand(bc, frame.pc);
      operand dest = read_operand(bc, frame.pc);
      set_local(dest, state.ctx.get_top_level(global_num));
      break;
    }

    case opcode::store_top_level: {
      operand reg = read_operand(bc, frame.pc);
      operand global_num = read_operand(bc, frame.pc);
      state.ctx.set_top_level(global_num, local(reg));
      break;
    }

    case opcode::add:
    case opcode::subtract:
    case opcode::multiply:
    case opcode::divide: {
      object* lhs = local(read_operand(bc, frame.pc));
      object* rhs = local(read_operand(bc, frame.pc));
      operand dest = read_operand(bc, frame.pc);

      switch (op) {
      case opcode::add:
        set_local(dest, add(state.ctx, lhs, rhs));
        break;
      case opcode::subtract:
        set_local(dest, subtract(state.ctx, lhs, rhs));
        break;
      case opcode::multiply:
        set_local(dest, multiply(state.ctx, lhs, rhs));
        break;
      case opcode::divide:
        set_local(dest, truncate_quotient(state.ctx, lhs, rhs));
        break;
      default:
        assert(!"Cannot get here");
      }

      break;
    }

    case opcode::arith_equal:
    case opcode::less_than:
    case opcode::greater_than: {
      object* lhs = local(read_operand(bc, frame.pc));
      object* rhs = local(read_operand(bc, frame.pc));
      operand dest = read_operand(bc, frame.pc);

      switch (op) {
      case opcode::arith_equal:
        set_local(dest, arith_equal(state.ctx, lhs, rhs));
        break;
      case opcode::less_than:
        set_local(dest, less(state.ctx, lhs, rhs));
        break;
      case opcode::greater_than:
        set_local(dest, greater(state.ctx, lhs, rhs));
        break;
      default:
        assert(!"Cannot get here");
      }
      break;
    }

    case opcode::set: {
      operand src = read_operand(bc, frame.pc);
      operand dst = read_operand(bc, frame.pc);
      set_local(dst, local(src));
      break;
    }

    case opcode::tail_call:
    case opcode::call:
    case opcode::call_top_level:
    case opcode::call_static:
    case opcode::tail_call_top_level:
    case opcode::tail_call_static: {
      bool is_tail = op == opcode::tail_call
        || op == opcode::tail_call_top_level
        || op == opcode::tail_call_static;

      object* callee;
      switch (op) {
      case opcode::call:
      case opcode::tail_call:
        callee = local(read_operand(bc, frame.pc));
        break;

      case opcode::call_top_level:
      case opcode::tail_call_top_level:
        callee = state.ctx.get_top_level(read_operand(bc, frame.pc));
        break;

      case opcode::call_static:
      case opcode::tail_call_static:
        callee = state.ctx.get_static(read_operand(bc, frame.pc));
        break;

      default:
        assert(false);
      }

      if (!is_tail)
        frame.dest_register = read_operand(bc, frame.pc);

      operand num_args = read_operand(bc, frame.pc);
      object* call_target = callee;
      insider::closure* closure = nullptr;

      if (auto cls = match<insider::closure>(call_target)) {
        call_target = cls->procedure();
        closure = cls;
      }

      if (auto scheme_proc = match<procedure>(call_target)) {
        if (num_args < scheme_proc->min_args)
          throw error{"{}: Wrong number of arguments, expected {}{}, got {}",
              scheme_proc->name ? *scheme_proc->name : "<lambda>",
              scheme_proc->has_rest ? "at least " : "",
              scheme_proc->min_args, num_args};

        std::size_t closure_size = 0;
        if (closure)
          closure_size = closure->size();

        std::size_t args_size = scheme_proc->min_args;
        if (scheme_proc->has_rest)
          ++args_size;

        std::size_t new_base = state.value_stack->size();
        state.value_stack->change_allocation(new_base + scheme_proc->locals_size);

        for (std::size_t i = 0; i < closure_size; ++i)
          state.value_stack->push(closure->ref(i));
        for (std::size_t i = 0; i < scheme_proc->min_args; ++i)
          state.value_stack->push(local(read_operand(bc, frame.pc)));

        if (scheme_proc->has_rest) {
          std::size_t num_rest = num_args - scheme_proc->min_args;
          state.value_stack->change_allocation(state.value_stack->size() + num_rest + 1);

          for (std::size_t i = 0; i < num_rest; ++i)
            state.value_stack->push(local(read_operand(bc, frame.pc)));

          state.value_stack->push(state.ctx.constants->null.get());

          for (std::size_t i = 0; i < num_rest; ++i) {
            object* tail = state.value_stack->pop();
            object* head = state.value_stack->pop();
            state.value_stack->push(cons(state.ctx, head, tail));
          }
        }

        if (is_tail) {
          std::size_t parent_base = frame.stack_top;

          for (std::size_t i = 0; i < closure_size + args_size; ++i)
            state.value_stack->set(parent_base + i, state.value_stack->ref(new_base + i));

          frame.pc = scheme_proc->entry_pc;
          frame.procedure = scheme_proc;

          state.value_stack->resize(parent_base + scheme_proc->locals_size);
        } else {
          state.call_stack->push(scheme_proc, new_base);
          state.value_stack->resize(new_base + scheme_proc->locals_size);
        }
      } else if (is_native_procedure(call_target)) {
        assert(!closure);
        object* result = call_native(state, call_target, num_args);

        if (!is_tail)
          state.value_stack->set(frame.stack_top + frame.dest_register, result);
        else {
          // tail_call. For Scheme procedures, the callee would perform a ret and
          // go back to this frame's caller. Since this is a native procedure, we
          // have to simulate the ret ourselves.

          if (state.call_stack->size() == 1)
            // Same as in ret below: We're returning from the global procedure.
            return track(state.ctx, result);

          pop_call_frame(state);
          state.value_stack->set(state.call_stack->current_frame().stack_top + state.call_stack->current_frame().dest_register,
                                 result);
        }
      } else
        throw error{"Application: Not a procedure: {}", datum_to_string(state.ctx, call_target)};
      break;
    }

    case opcode::ret: {
      operand return_reg = read_operand(bc, frame.pc);
      object* result = local(return_reg);

      if (state.call_stack->size() == 1)
        // We are returning from the global procedure, so we return back to the
        // calling C++ code. We won't pop the final stack so that the caller may
        // inspect it (used in tests).
        return track(state.ctx, result);

      pop_call_frame(state);
      state.value_stack->set(state.call_stack->current_frame().stack_top + state.call_stack->current_frame().dest_register,
                             result);
      break;
    }

    case opcode::jump:
    case opcode::jump_back:
    case opcode::jump_unless:
    case opcode::jump_back_unless: {
      operand off;
      operand condition_reg{};

      if (op == opcode::jump || op == opcode::jump_back)
        off = read_operand(bc, frame.pc);
      else {
        condition_reg = read_operand(bc, frame.pc);
        off = read_operand(bc, frame.pc);
      }

      int offset = (op == opcode::jump_back || op == opcode::jump_back_unless) ? -off : off;
      if (op == opcode::jump_unless || op == opcode::jump_back_unless) {
        object* test_value = local(condition_reg);

        // The only false value in Scheme is #f. So we only jump if the test_value
        // is exactly #f.

        if (test_value != state.ctx.constants->f.get())
          break;
      }

      frame.pc += offset;
      break;
    }

    case opcode::make_closure: {
      procedure* proc = assume<procedure>(local(read_operand(bc, frame.pc)));
      operand dest = read_operand(bc, frame.pc);
      operand num_captures = read_operand(bc, frame.pc);

      auto result = make<closure>(state.ctx, proc, num_captures);
      for (std::size_t i = 0; i < num_captures; ++i)
        result->set(state.ctx.store, i, local(read_operand(bc, frame.pc)));

      set_local(dest, result);
      break;
    }

    case opcode::box: {
      object* value = local(read_operand(bc, frame.pc));
      set_local(read_operand(bc, frame.pc), state.ctx.store.make<box>(value));
      break;
    }

    case opcode::unbox: {
      auto box = expect<insider::box>(local(read_operand(bc, frame.pc)));
      set_local(read_operand(bc, frame.pc), box->get());
      break;
    }

    case opcode::box_set: {
      auto box = expect<insider::box>(local(read_operand(bc, frame.pc)));
      box->set(state.ctx.store, local(read_operand(bc, frame.pc)));
      break;
    }

    case opcode::cons: {
      object* car = local(read_operand(bc, frame.pc));
      object* cdr = local(read_operand(bc, frame.pc));
      set_local(read_operand(bc, frame.pc), make<pair>(state.ctx, car, cdr));
      break;
    }

    case opcode::make_vector: {
      operand dest = read_operand(bc, frame.pc);
      operand num_elems = read_operand(bc, frame.pc);

      auto result = make<vector>(state.ctx, state.ctx, num_elems);
      for (std::size_t i = 0; i < num_elems; ++i)
        result->set(state.ctx.store, i, local(read_operand(bc, frame.pc)));

      set_local(dest, result);
      break;
    }
    } // end switch

    no_gc.enable();
    state.ctx.store.update();
  }

  assert(false); // The only way the loop above will exit is via return.
  return {};
}

generic_tracked_ptr
call(context& ctx, object* callable, std::vector<object*> const& arguments) {
  std::vector<object*> closure;
  if (auto cls = match<insider::closure>(callable)) {
    callable = cls->procedure();
    closure = collect_closure(cls);
  }

  if (auto scheme_proc = match<procedure>(callable)) {
    if (scheme_proc->min_args != arguments.size())
      throw std::runtime_error{"Wrong number of arguments in function call"};

    execution_state state = make_state(ctx, scheme_proc, closure, arguments);
    return run(state);
  } else if (is_native_procedure(callable)) {
    assert(closure.empty());
    return track(ctx, call_native_vector(ctx, callable, arguments));
  } else
    throw std::runtime_error{"Expected a callable"};
}

} // namespace insider
