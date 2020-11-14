#include "vm.hpp"

#include "action.hpp"
#include "io.hpp"
#include "numeric.hpp"

#include <cassert>
#include <cstdint>
#include <stdexcept>
#include <vector>

namespace insider {

void
root_stack::erase(std::size_t begin, std::size_t end) {
  data_.erase(data_.begin() + begin, data_.begin() + end);
}

void
root_stack::trace(tracing_context& tc) {
  for (object* o : data_)
    tc.trace(o);
}

void
root_stack::update_references() {
  for (object*& o : data_)
    update_reference(o);
}

auto
call_stack::push(insider::procedure* proc, std::size_t stack_top) -> frame& {
  return frames_.emplace_back(frame{0, proc, stack_top, operand{}});
}

void
call_stack::trace(tracing_context& tc) {
  for (frame const& f : frames_)
    tc.trace(f.procedure);
}

void
call_stack::update_references() {
  for (frame& f : frames_)
    update_reference(f.procedure);
}

execution_state::execution_state(context& ctx)
  : ctx{ctx}
  , value_stack{make_tracked<root_stack>(ctx)}
  , call_stack{make_tracked<insider::call_stack>(ctx)}
{
  value_stack->reserve(4096);
  call_stack->reserve(1024);
}

static std::vector<object*>
collect_closure(closure* cls) {
  std::vector<object*> result;
  result.reserve(cls->size());
  for (std::size_t i = 0; i < cls->size(); ++i)
    result.push_back(cls->ref(i));

  return result;
}

// Make a call frame and copy closure and call arguments into it, and push the
// new frame to both value_stack and call_stack.
static std::size_t
push_call_frame(execution_state& state, procedure* proc,
                closure* cls, std::size_t num_args) {
  call_stack::frame& result = state.call_stack->push(proc, state.value_stack->size());
  state.value_stack->grow(proc->locals_size);

  assert(state.call_stack->size() >= 2);
  call_stack::frame& caller = state.call_stack->parent_frame();
  bytecode const& bc = caller.procedure->bytecode;

  std::size_t closure_size = 0;
  if (cls) {
    closure_size = cls->size();
    for (std::size_t i = 0; i < closure_size; ++i)
      stack_set(state.value_stack, result.stack_top + i, cls->ref(i));
  }

  for (std::size_t i = 0; i < num_args; ++i) {
    stack_set(state.value_stack, result.stack_top + closure_size + i,
              state.value_stack->ref(caller.stack_top + read_operand(bc, caller.pc)));
  }

  return closure_size + num_args;
}

static void
pop_call_frame(execution_state& state) {
  std::size_t num_locals = state.call_stack->current_frame().procedure->locals_size;
  state.call_stack->pop();
  state.value_stack->shrink(num_locals);
}

static void
erase_parent_frame(execution_state& state) {
  assert(state.call_stack->size() >= 2);

  call_stack::frame const& parent = state.call_stack->parent_frame();
  std::size_t values_begin = parent.stack_top;
  std::size_t values_end = values_begin + parent.procedure->locals_size;

  state.call_stack->pop_parent();
  state.value_stack->erase(values_begin, values_end);

  state.call_stack->current_frame().stack_top = values_begin;
}

static void
call_frame_set_local(execution_state& state, operand local, object* value) {
  assert(value);
  stack_set(state.value_stack, state.call_stack->current_frame().stack_top + local, value);
}

object*
call_frame_local(execution_state& state, operand local) {
  return state.value_stack->ref(state.call_stack->current_frame().stack_top + local);
}

template <int Arity, std::size_t... Is>
object*
do_call(execution_state& state, native_procedure<Arity>* proc, std::size_t num_args, std::index_sequence<Is...>) {
  call_stack::frame& frame = state.call_stack->current_frame();
  bytecode const& bc = frame.procedure->bytecode;

  // Consume operands even if call is invalid.
  std::array<operand, Arity> arg_operands{((void) Is, read_operand(bc, frame.pc))...};
  (void) arg_operands; // Unused when Arity == 0.

  if (Arity != num_args)
    throw error{"{}: Wrong number of arguments, expected {}, got {}", proc->name, Arity, num_args};

  return proc->target(state.ctx, state.value_stack->ref(frame.stack_top + arg_operands[Is])...);
}

template <int Arity>
object*
call_native_helper(execution_state& state, object* proc, std::size_t num_args) {
  if (is<native_procedure<Arity>>(proc))
    return do_call<Arity>(state, assume<native_procedure<Arity>>(proc), num_args, std::make_index_sequence<Arity>{});
  else
    return call_native_helper<Arity - 1>(state, proc, num_args);
}

template <>
object*
call_native_helper<-1>(execution_state& state, object* proc, std::size_t num_args) {
  call_stack::frame& frame = state.call_stack->current_frame();
  bytecode const& bc = frame.procedure->bytecode;
  auto native_proc = assume<native_procedure<-1>>(proc);

  std::vector<object*> args;
  args.reserve(num_args);
  for (std::size_t i = 0; i < num_args; ++i)
    args.push_back(state.value_stack->ref(frame.stack_top + read_operand(bc, frame.pc)));

  return native_proc->target(state.ctx, args);
}

static object*
call_native(execution_state& state, object* proc, std::size_t num_args) {
  return call_native_helper<max_specialised_arity>(state, proc, num_args);
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

// Execute the current instruction in the current call frame.
static void
execute_one(execution_state& state) {
  call_stack::frame& frame = state.call_stack->current_frame();
  bytecode const& bc = frame.procedure->bytecode;

  opcode op = read_opcode(bc, frame.pc);
  switch (op) {
  case opcode::no_operation:
    break;

  case opcode::load_static: {
    operand static_num = read_operand(bc, frame.pc);
    operand dest = read_operand(bc, frame.pc);
    call_frame_set_local(state, dest, state.ctx.get_static(static_num));
    break;
  }

  case opcode::load_top_level: {
    operand global_num = read_operand(bc, frame.pc);
    operand dest = read_operand(bc, frame.pc);
    call_frame_set_local(state, dest, state.ctx.get_top_level(global_num));
    break;
  }

  case opcode::store_top_level: {
    operand reg = read_operand(bc, frame.pc);
    operand global_num = read_operand(bc, frame.pc);
    state.ctx.set_top_level(global_num, call_frame_local(state, reg));
    break;
  }

  case opcode::add:
  case opcode::subtract:
  case opcode::multiply:
  case opcode::divide: {
    object* lhs = call_frame_local(state, read_operand(bc, frame.pc));
    object* rhs = call_frame_local(state, read_operand(bc, frame.pc));
    operand dest = read_operand(bc, frame.pc);

    switch (op) {
    case opcode::add:
      call_frame_set_local(state, dest, add(state.ctx, lhs, rhs));
      break;
    case opcode::subtract:
      call_frame_set_local(state, dest, subtract(state.ctx, lhs, rhs));
      break;
    case opcode::multiply:
      call_frame_set_local(state, dest, multiply(state.ctx, lhs, rhs));
      break;
    case opcode::divide:
      call_frame_set_local(state, dest, truncate_quotient(state.ctx, lhs, rhs));
      break;
    default:
      assert(!"Cannot get here");
    }

    break;
  }

  case opcode::arith_equal:
  case opcode::less_than:
  case opcode::greater_than: {
    object* lhs = call_frame_local(state, read_operand(bc, frame.pc));
    object* rhs = call_frame_local(state, read_operand(bc, frame.pc));
    operand dest = read_operand(bc, frame.pc);

    switch (op) {
    case opcode::arith_equal:
      call_frame_set_local(state, dest, arith_equal(state.ctx, lhs, rhs));
      break;
    case opcode::less_than:
      call_frame_set_local(state, dest, less(state.ctx, lhs, rhs));
      break;
    case opcode::greater_than:
      call_frame_set_local(state, dest, greater(state.ctx, lhs, rhs));
      break;
    default:
      assert(!"Cannot get here");
    }
    break;
  }

  case opcode::set: {
    operand src = read_operand(bc, frame.pc);
    operand dst = read_operand(bc, frame.pc);
    call_frame_set_local(state, dst, call_frame_local(state, src));
    break;
  }

  case opcode::call:
  case opcode::call_top_level:
  case opcode::call_static:
  case opcode::tail_call:
  case opcode::tail_call_top_level:
  case opcode::tail_call_static: {
    bool is_tail = op == opcode::tail_call
                   || op == opcode::tail_call_top_level
                   || op == opcode::tail_call_static;

    object* callee;
    switch (op) {
    case opcode::call:
    case opcode::tail_call:
      callee = call_frame_local(state, read_operand(bc, frame.pc));
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

      std::size_t values_used = push_call_frame(state, scheme_proc, closure, scheme_proc->min_args);
      call_stack::frame& parent_frame = state.call_stack->parent_frame();

      if (scheme_proc->has_rest) {
        // We have to pass exactly min_args + 1 arguments. The last one is a
        // list with all the arguments after the min_args'th.

        std::size_t num_rest = num_args - scheme_proc->min_args;
        std::vector<object*> rest_args;
        rest_args.reserve(num_rest);

        call_stack::frame const& caller = state.call_stack->parent_frame();
        for (std::size_t i = 0; i < num_rest; ++i)
          rest_args.push_back(state.value_stack->ref(caller.stack_top + read_operand(parent_frame.procedure->bytecode,
                                                                                     parent_frame.pc)));

        object* rest_list = make_list_from_vector(state.ctx, rest_args);
        stack_set(state.value_stack, state.call_stack->current_frame().stack_top + values_used, rest_list);
      }

      if (is_tail)
        erase_parent_frame(state);
    } else if (is_native_procedure(call_target)) {
      simple_action<std::string const&> a{state.ctx, "in {}", native_procedure_name(call_target)};

      assert(!closure);

      object* result = call_native(state, call_target, num_args);

      if (!is_tail)
        stack_set(state.value_stack, frame.stack_top + frame.dest_register, result);
      else {
        // tail_call. For Scheme procedures, the callee would perform a ret and
        // go back to this frame's caller. Since this is a native procedure, we
        // have to simulate the ret ourselves.

        if (state.call_stack->size() == 1) {
          // Same as in ret below: We're returning from the global procedure.

          state.global_return = track(state.ctx, result);
          frame.pc = frame.procedure->bytecode.size();
          return;
        }

        pop_call_frame(state);
        stack_set(state.value_stack, state.call_stack->current_frame().stack_top + state.call_stack->current_frame().dest_register,
                  result);
      }
    } else
      throw error{"Application: Not a procedure: {}", datum_to_string(state.ctx, call_target)};
    break;
  }

  case opcode::ret: {
    operand return_reg = read_operand(bc, frame.pc);
    object* result = call_frame_local(state, return_reg);

    if (state.call_stack->size() == 1) {
      // We are returning from the global procedure, so we have nowhere to
      // return to. We will set the global_return value and *not* pop the
      // stack. We'll then set pc past all the bytecode so we finish execution.

      state.global_return = track(state.ctx, result);
      frame.pc = frame.procedure->bytecode.size();
      return;
    }

    pop_call_frame(state);
    stack_set(state.value_stack, state.call_stack->current_frame().stack_top + state.call_stack->current_frame().dest_register,
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
      object* test_value = call_frame_local(state, condition_reg);

      // The only false value in Scheme is #f. So we only jump if the test_value
      // is exactly #f.

      if (test_value != state.ctx.constants->f.get())
        break;
    }

    frame.pc += offset;
    break;
  }

  case opcode::make_closure: {
    procedure* proc = assume<procedure>(call_frame_local(state, read_operand(bc, frame.pc)));
    operand dest = read_operand(bc, frame.pc);
    operand num_captures = read_operand(bc, frame.pc);

    auto result = make<closure>(state.ctx, proc, num_captures);
    for (std::size_t i = 0; i < num_captures; ++i)
      result->set(state.ctx.store, i, call_frame_local(state, read_operand(bc, frame.pc)));

    call_frame_set_local(state, dest, result);
    break;
  }

  case opcode::box: {
    object* value = call_frame_local(state, read_operand(bc, frame.pc));
    call_frame_set_local(state, read_operand(bc, frame.pc), state.ctx.store.make<box>(value));
    break;
  }

  case opcode::unbox: {
    auto box = expect<insider::box>(call_frame_local(state, read_operand(bc, frame.pc)));
    call_frame_set_local(state, read_operand(bc, frame.pc), box->get());
    break;
  }

  case opcode::box_set: {
    auto box = expect<insider::box>(call_frame_local(state, read_operand(bc, frame.pc)));
    box->set(state.ctx.store, call_frame_local(state, read_operand(bc, frame.pc)));
    break;
  }

  case opcode::cons: {
    object* car = call_frame_local(state, read_operand(bc, frame.pc));
    object* cdr = call_frame_local(state, read_operand(bc, frame.pc));
    call_frame_set_local(state, read_operand(bc, frame.pc), make<pair>(state.ctx, car, cdr));
    break;
  }

  case opcode::make_vector: {
    operand dest = read_operand(bc, frame.pc);
    operand num_elems = read_operand(bc, frame.pc);

    auto result = make<vector>(state.ctx, state.ctx, num_elems);
    for (std::size_t i = 0; i < num_elems; ++i)
      result->set(state.ctx.store, i, call_frame_local(state, read_operand(bc, frame.pc)));

    call_frame_set_local(state, dest, result);
    break;
  }
  } // end switch
}

execution_state
make_state(context& ctx, procedure* global,
           std::vector<object*> const& closure, std::vector<object*> const& arguments) {
  execution_state result{ctx};
  result.call_stack->push(global, 0);
  result.value_stack->grow(global->locals_size);

  std::size_t closure_size = closure.size();
  for (std::size_t i = 0; i < closure_size; ++i)
    stack_set(result.value_stack, i, closure[i]);

  for (std::size_t i = 0; i < arguments.size(); ++i)
    stack_set(result.value_stack, closure_size + i, arguments[i]);

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
      for (auto frame = state_.call_stack->rbegin(); frame != state_.call_stack->rend(); ++frame) {
        std::optional<std::string> name = frame->procedure->name;

        if (frame != state_.call_stack->rbegin())
          result += '\n';
        result += fmt::format("in {}", name ? *name : "<lambda>");
      }

      return result;
    }

  private:
    execution_state& state_;
  };
}

object*
run(execution_state& state) {
  execution_action a(state);

  while (state.call_stack->current_frame().pc != state.call_stack->current_frame().procedure->bytecode.size()) {
    execute_one(state);
    state.ctx.store.update();
  }

  assert(state.call_stack->size() == 1); // Non-global procedures must finish by executing ret.

  return state.global_return.get();
}

object*
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
    return call_native_vector(ctx, callable, arguments);
  } else
    throw std::runtime_error{"Expected a callable"};
}

} // namespace insider
