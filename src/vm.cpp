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

static constexpr std::size_t frame_preamble_size = 3;

namespace {
  // Dynamic-sized stack to store local variables.
  class root_stack : public composite_root_object<root_stack> {
  public:
    static constexpr char const* scheme_name = "insider::root_stack";

    root_stack();

    object*
    ref(std::size_t i) { assert(i < size_); assert(data_[i]); return data_[i]; }

    object_span
    span(std::size_t begin, std::size_t size) { return {data_.get() + begin, size}; }

    void
    set(std::size_t i, object* value) {
      assert(i < size_);
      assert(is_valid(value));
      data_[i] = value;
    }

    void
    push(object* value) {
      assert(is_valid(value));
      assert(size_ + 1 <= alloc_);

      data_[size_++] = value;
    }

    object*
    pop() {
      return data_[--size_];
    }

    void
    change_allocation(std::size_t alloc);

    void
    grow(std::size_t n);

    void
    shrink(std::size_t n);

    void
    resize(std::size_t new_size);

    integer::value_type
    size() const { return size_; }

    void
    trace(tracing_context&) const;

    void
    update_references();

    std::size_t
    hash() const { return 0; }

  private:
    std::unique_ptr<object*[]> data_;
    std::size_t size_ = 0;
    std::size_t alloc_;
  };
} // anonymous namespace

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
root_stack::trace(tracing_context& tc) const {
  for (std::size_t i = 0; i < size_; ++i)
    tc.trace(data_[i]);
}

void
root_stack::update_references() {
  for (std::size_t i = 0; i < size_; ++i)
    update_reference(data_[i]);
}

namespace {
  struct execution_state {
    context&                         ctx;
    tracked_ptr<root_stack>          value_stack;
    integer::value_type              pc;
    integer::value_type              frame_base = 0;

    execution_state(context& ctx);
  };
}

execution_state::execution_state(context& ctx)
  : ctx{ctx}
  , value_stack{make_tracked<root_stack>(ctx)}
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
  state.value_stack->resize(state.frame_base);
  state.value_stack->pop(); // Procedure pointer.
  state.frame_base = ptr_to_integer(state.value_stack->pop()).value();
  state.pc = ptr_to_integer(state.value_stack->pop()).value();
}

static operand
get_destination_register(execution_state& state) {
  bytecode const& bc = state.ctx.program;
  integer::value_type& pc = state.pc;

  basic_instruction bi = read_basic_instruction(bc, pc);
  assert(bi.opcode == opcode::call || bi.opcode == opcode::call_top_level || bi.opcode == opcode::call_static);

  for_each_extra_operand(bc, pc, bi.operands[2], [] (operand) { }); // Skip over extra operands.

  return bi.operands[1];
}

static execution_state
make_state(context& ctx, procedure* global,
           std::vector<object*> const& closure, std::vector<object*> const& arguments) {
  execution_state result{ctx};

  result.value_stack->change_allocation(global->locals_size + frame_preamble_size);

  result.value_stack->push(integer_to_ptr(-1)); // Previous PC.
  result.value_stack->push(integer_to_ptr(-1)); // Previous frame base.
  result.value_stack->push(global);

  integer::value_type frame_base = frame_preamble_size;

  result.value_stack->grow(global->locals_size);
  result.pc = global->entry_pc;
  result.frame_base = frame_base;

  std::size_t closure_size = closure.size();
  for (std::size_t i = 0; i < closure_size; ++i)
    result.value_stack->set(frame_base + i, closure[i]);

  for (std::size_t i = 0; i < arguments.size(); ++i)
    result.value_stack->set(frame_base + closure_size + i, arguments[i]);

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

      bool first = true;
      integer::value_type frame = state_.frame_base;

      while (frame != -1) {
        if (!first)
          result += '\n';

        integer::value_type previous_frame = ptr_to_integer(state_.value_stack->ref(frame - 2)).value();
        object* proc = state_.value_stack->ref(frame - 1);

        if (auto scheme_proc = match<procedure>(proc)) {
          auto name = scheme_proc->name;
          result += fmt::format("in {}", name ? *name : "<lambda>");
        } else {
          assert(is<native_procedure>(proc));
          result += fmt::format("in native procedure {}", assume<native_procedure>(proc)->name);
        }

        frame = previous_frame;
        first = false;
      }

      return result;
    }

  private:
    execution_state& state_;
  };
}

static generic_tracked_ptr
run(execution_state& state) {
  execution_action a(state);
  gc_disabler no_gc{state.ctx.store};

  integer::value_type& frame_base = state.frame_base;
  integer::value_type& pc = state.pc;

  while (true) {
    integer::value_type previous_pc = pc;

    root_stack& values = *state.value_stack;
    bytecode const& bc = state.ctx.program;

    basic_instruction instr = read_basic_instruction(bc, pc);

#ifdef INSIDER_VM_PROFILER
    ++state.ctx.instruction_counts[static_cast<std::size_t>(instr.opcode)];

    struct time_tracker {
      opcode   oc;
      context& ctx;
      std::chrono::high_resolution_clock::time_point begin = std::chrono::high_resolution_clock::now();

      ~time_tracker() {
        auto end = std::chrono::high_resolution_clock::now();
        ctx.instruction_times[static_cast<std::size_t>(oc)] += end - begin;
      }
    } tracker{instr.opcode, state.ctx};
#endif

    switch (instr.opcode) {
    case opcode::no_operation:
      break;

    case opcode::load_static: {
      operand static_num = instr.operands[0];
      operand dest = instr.operands[1];
      values.set(frame_base + dest, state.ctx.get_static(static_num));
      break;
    }

    case opcode::load_top_level: {
      operand global_num = instr.operands[0];
      operand dest = instr.operands[1];
      values.set(frame_base + dest, state.ctx.get_top_level(global_num));
      break;
    }

    case opcode::store_top_level: {
      operand reg = instr.operands[0];
      operand global_num = instr.operands[1];
      state.ctx.set_top_level(global_num, values.ref(frame_base + reg));
      break;
    }

    case opcode::add:
    case opcode::subtract:
    case opcode::multiply:
    case opcode::divide: {
      object* lhs = values.ref(frame_base + instr.operands[0]);
      object* rhs = values.ref(frame_base + instr.operands[1]);
      operand dest = instr.operands[2];

      if (is<integer>(lhs) && is<integer>(rhs) && instr.opcode != opcode::divide) {
        switch (instr.opcode) {
        case opcode::add:
          if (object* result = add_fixnums(assume<integer>(lhs).value(), assume<integer>(rhs).value()))
            values.set(frame_base + dest, result);
          else
            values.set(frame_base + dest, add(state.ctx, lhs, rhs));
          break;

        case opcode::subtract:
          if (object* result = subtract_fixnums(assume<integer>(lhs).value(), assume<integer>(rhs).value()))
            values.set(frame_base + dest, result);
          else
            values.set(frame_base + dest, subtract(state.ctx, lhs, rhs));
          break;

        case opcode::multiply:
          if (object* result = multiply_fixnums(assume<integer>(lhs).value(), assume<integer>(rhs).value()))
            values.set(frame_base + dest, result);
          else
            values.set(frame_base + dest, multiply(state.ctx, lhs, rhs));
          break;

        default:
          assert(false);
          break;
        }

        break;
      }

      switch (instr.opcode) {
      case opcode::add:
        values.set(frame_base + dest, add(state.ctx, lhs, rhs));
        break;
      case opcode::subtract:
        values.set(frame_base + dest, subtract(state.ctx, lhs, rhs));
        break;
      case opcode::multiply:
        values.set(frame_base + dest, multiply(state.ctx, lhs, rhs));
        break;
      case opcode::divide:
        values.set(frame_base + dest, truncate_quotient(state.ctx, lhs, rhs));
        break;
      default:
        assert(!"Cannot get here");
      }

      break;
    }

    case opcode::arith_equal:
    case opcode::less:
    case opcode::greater:
    case opcode::less_or_equal:
    case opcode::greater_or_equal: {
      object* lhs = values.ref(frame_base + instr.operands[0]);
      object* rhs = values.ref(frame_base + instr.operands[1]);
      operand dest = instr.operands[2];

      if (is<integer>(lhs) && is<integer>(rhs)) {
        integer::value_type x = assume<integer>(lhs).value();
        integer::value_type y = assume<integer>(rhs).value();
        object* t = state.ctx.constants->t.get();
        object* f = state.ctx.constants->f.get();

        switch (instr.opcode) {
        case opcode::arith_equal:
          values.set(frame_base + dest, x == y ? t : f);
          break;

        case opcode::less:
          values.set(frame_base + dest, x < y ? t : f);
          break;

        case opcode::greater:
          values.set(frame_base + dest, x > y ? t : f);
          break;

        case opcode::less_or_equal:
          values.set(frame_base + dest, x <= y ? t : f);
          break;

        case opcode::greater_or_equal:
          values.set(frame_base + dest, x >= y ? t : f);
          break;

        default:
          assert(false);
        }
      }

      switch (instr.opcode) {
      case opcode::arith_equal:
        values.set(frame_base + dest, arith_equal(state.ctx, lhs, rhs));
        break;
      case opcode::less:
        values.set(frame_base + dest, less(state.ctx, lhs, rhs));
        break;
      case opcode::greater:
        values.set(frame_base + dest, greater(state.ctx, lhs, rhs));
        break;
      case opcode::less_or_equal:
        values.set(frame_base + dest, less_or_equal(state.ctx, lhs, rhs));
        break;
      case opcode::greater_or_equal:
        values.set(frame_base + dest, greater_or_equal(state.ctx, lhs, rhs));
        break;
      default:
        assert(!"Cannot get here");
      }
      break;
    }

    case opcode::set: {
      operand src = instr.operands[0];
      operand dst = instr.operands[1];
      values.set(frame_base + dst, values.ref(frame_base + src));
      break;
    }

    case opcode::tail_call:
    case opcode::call:
    case opcode::call_top_level:
    case opcode::call_static:
    case opcode::tail_call_top_level:
    case opcode::tail_call_static: {
      bool is_tail = instr.opcode == opcode::tail_call
        || instr.opcode == opcode::tail_call_top_level
        || instr.opcode == opcode::tail_call_static;

      object* callee;
      switch (instr.opcode) {
      case opcode::call:
      case opcode::tail_call:
        callee = values.ref(frame_base + instr.operands[0]);
        break;

      case opcode::call_top_level:
      case opcode::tail_call_top_level:
        callee = state.ctx.get_top_level(instr.operands[0]);
        break;

      case opcode::call_static:
      case opcode::tail_call_static:
        callee = state.ctx.get_static(instr.operands[0]);
        break;

      default:
        assert(false);
      }

      operand dest_register{};
      operand num_args;

      if (is_tail)
        num_args = instr.operands[1];
      else {
        dest_register = instr.operands[1];
        num_args = instr.operands[2];
      }

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

        std::size_t new_base = state.value_stack->size() + frame_preamble_size;
        state.value_stack->change_allocation(new_base + scheme_proc->locals_size);

        state.value_stack->push(integer_to_ptr(previous_pc));
        state.value_stack->push(integer_to_ptr(frame_base));
        state.value_stack->push(scheme_proc);

        for (std::size_t i = 0; i < closure_size; ++i)
          state.value_stack->push(closure->ref(i));

        std::size_t num_rest = 0;
        if (scheme_proc->has_rest) {
          num_rest = num_args - scheme_proc->min_args;
          state.value_stack->change_allocation(state.value_stack->size() + num_rest + 1);
        }

        for_each_extra_operand(bc, pc, num_args,
                               [&] (operand op) {
                                 state.value_stack->push(values.ref(frame_base + op));
                               });

        if (scheme_proc->has_rest) {
          state.value_stack->push(state.ctx.constants->null.get());

          for (std::size_t i = 0; i < num_rest; ++i) {
            object* tail = state.value_stack->pop();
            object* head = state.value_stack->pop();
            state.value_stack->push(cons(state.ctx, head, tail));
          }
        }

        if (is_tail) {
          for (std::size_t i = 0; i < closure_size + args_size; ++i)
            state.value_stack->set(frame_base + i, state.value_stack->ref(new_base + i));

          pc = scheme_proc->entry_pc;
          state.value_stack->resize(frame_base + scheme_proc->locals_size);
        } else {
          values.resize(new_base + scheme_proc->locals_size);

          pc = scheme_proc->entry_pc;
          frame_base = new_base;
        }
      } else if (auto native_proc = match<native_procedure>(call_target)) {
        assert(!closure);

        values.change_allocation(state.value_stack->size() + num_args + frame_preamble_size);
        values.push(integer_to_ptr(previous_pc));
        values.push(integer_to_ptr(frame_base));
        values.push(call_target);

        std::size_t new_base = values.size();
        for_each_extra_operand(bc, pc, num_args,
                               [&] (operand op) { values.push(values.ref(frame_base + op)); });

        object* result = native_proc->target(state.ctx, values.span(new_base, num_args));

        values.shrink(num_args + frame_preamble_size);

        if (!is_tail)
          state.value_stack->set(frame_base + dest_register, result);
        else {
          // tail_call. For Scheme procedures, the callee would perform a ret and
          // go back to this frame's caller. Since this is a native procedure, we
          // have to simulate the ret ourselves.

          pop_call_frame(state);

          if (frame_base == -1)
            // Same as in ret below: We're returning from the global procedure.
            return track(state.ctx, result);

          state.value_stack->set(frame_base + get_destination_register(state), result);
        }

        no_gc.force_update();
      } else
        throw error{"Application: Not a procedure: {}", datum_to_string(state.ctx, call_target)};
      break;
    }

    case opcode::ret: {
      operand return_reg = instr.operands[0];
      object* result = values.ref(frame_base + return_reg);

      pop_call_frame(state);

      if (frame_base == -1)
        // We are returning from the global procedure, so we return back to the
        // calling C++ code. We won't pop the final stack so that the caller may
        // inspect it (used in tests).
        return track(state.ctx, result);

      state.value_stack->set(frame_base + get_destination_register(state), result);

      no_gc.force_update();
      break;
    }

    case opcode::jump:
    case opcode::jump_back:
    case opcode::jump_unless:
    case opcode::jump_back_unless: {
      operand off;
      operand condition_reg{};

      if (instr.opcode == opcode::jump || instr.opcode == opcode::jump_back)
        off = instr.operands[0];
      else {
        condition_reg = instr.operands[0];
        off = instr.operands[1];
      }

      int offset = (instr.opcode == opcode::jump_back || instr.opcode == opcode::jump_back_unless) ? -off : off;
      if (instr.opcode == opcode::jump_unless || instr.opcode == opcode::jump_back_unless) {
        object* test_value = values.ref(frame_base + condition_reg);

        // The only false value in Scheme is #f. So we only jump if the test_value
        // is exactly #f.

        if (test_value != state.ctx.constants->f.get())
          break;
      }

      pc += offset;
      break;
    }

    case opcode::make_closure: {
      procedure* proc = assume<procedure>(values.ref(frame_base + instr.operands[0]));
      operand dest = instr.operands[1];
      operand num_captures = instr.operands[2];

      auto result = make<closure>(state.ctx, proc, num_captures);
      for_each_extra_operand(bc, pc, num_captures,
                             [&, i = 0] (operand op) mutable {
                               result->set(state.ctx.store, i, values.ref(frame_base + op));
                               ++i;
                             });

      values.set(frame_base + dest, result);
      break;
    }

    case opcode::box: {
      object* value = values.ref(frame_base + instr.operands[0]);
      values.set(frame_base + instr.operands[1], state.ctx.store.make<box>(value));
      break;
    }

    case opcode::unbox: {
      auto box = expect<insider::box>(values.ref(frame_base + instr.operands[0]));
      values.set(frame_base + instr.operands[1], box->get());
      break;
    }

    case opcode::box_set: {
      auto box = expect<insider::box>(values.ref(frame_base + instr.operands[0]));
      box->set(state.ctx.store, values.ref(frame_base + instr.operands[1]));
      break;
    }

    case opcode::cons: {
      object* car = values.ref(frame_base + instr.operands[0]);
      object* cdr = values.ref(frame_base + instr.operands[1]);
      values.set(frame_base + instr.operands[2], make<pair>(state.ctx, car, cdr));
      break;
    }

    case opcode::make_vector: {
      operand dest = instr.operands[0];
      operand num_elems = instr.operands[1];

      auto result = make<vector>(state.ctx, state.ctx, num_elems);
      for_each_extra_operand(bc, pc, num_elems,
                             [&, i = 0] (operand op) mutable {
                               result->set(state.ctx.store, i, values.ref(frame_base + op));
                               ++i;
                             });

      values.set(frame_base + dest, result);
      break;
    }
    } // end switch
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
  } else if (auto native_proc = match<native_procedure>(callable)) {
    assert(closure.empty());
    return track(ctx, native_proc->target(ctx, object_span(arguments)));
  } else
    throw std::runtime_error{"Expected a callable"};
}

} // namespace insider
