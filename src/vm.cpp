#include "vm.hpp"

#include "action.hpp"
#include "converters.hpp"
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
[[maybe_unused]] static constexpr std::size_t frame_procedure_offset = 1;
[[maybe_unused]] static constexpr std::size_t frame_base_offset = 2;

// Dynamic-sized stack to store local variables.
class root_stack : public composite_root_object<root_stack> {
public:
  static constexpr char const* scheme_name = "insider::root_stack";

  root_stack();

  ptr<>
  ref(std::size_t i) { assert(i < size_); return data_[i]; }

  object_span
  span(std::size_t begin, std::size_t size) { return {data_.get() + begin, size}; }

  void
  set(std::size_t i, ptr<> value) {
    assert(i < size_);
    assert(!value || is_valid(value));
    data_[i] = value;
  }

  void
  push(ptr<> value) {
    assert(is_valid(value));
    assert(size_ + 1 <= alloc_);

    data_[size_++] = value;
  }

  ptr<>
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

  void
  visit_members(member_visitor const& f);

  integer::value_type
  size() const { return size_; }

  std::size_t
  hash() const { return 0; }

private:
  std::unique_ptr<ptr<>[]> data_;
  std::size_t size_ = 0;
  std::size_t alloc_;
};

root_stack::root_stack()
  : data_{std::make_unique<ptr<>[]>(root_stack_initial_size)}
  , alloc_{root_stack_initial_size}
{
  std::fill(&data_[0], &data_[alloc_], nullptr);
}

void
root_stack::change_allocation(std::size_t new_alloc) {
  if (new_alloc > alloc_) {
    alloc_ = new_alloc;

    auto old_data = std::move(data_);
    data_ = std::make_unique<ptr<>[]>(alloc_);
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
root_stack::visit_members(member_visitor const& f) {
  for (std::size_t i = 0; i < size_; ++i)
    f(data_[i]);
}

execution_state::execution_state(context& ctx)
  : ctx{ctx}
  , value_stack{make_tracked<root_stack>(ctx)}
{ }

static bool
state_done(execution_state& state) {
  return state.frame_base == -1;
}

static std::vector<ptr<>>
collect_closure(ptr<closure> cls) {
  std::vector<ptr<>> result;
  result.reserve(cls->size());
  for (std::size_t i = 0; i < cls->size(); ++i)
    result.push_back(cls->ref(i));

  return result;
}

static std::size_t
push_frame(execution_state& state, ptr<procedure> proc, integer::value_type previous_pc) {
  root_stack& values = *state.value_stack;
  std::size_t new_base = values.size() + frame_preamble_size;

  values.change_allocation(new_base + proc->locals_size);
  values.push(integer_to_ptr(previous_pc));
  values.push(integer_to_ptr(state.frame_base));
  values.push(proc);

  return new_base;
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

  [[maybe_unused]] opcode opcode = read_opcode(bc, pc);
  assert(opcode == opcode::call || opcode == opcode::call_top_level || opcode == opcode::call_static);

  read_operand(bc, pc); // Call target.
  operand dest = read_operand(bc, pc);

  operand num_args = read_operand(bc, pc);
  for (std::size_t i = 0; i < num_args; ++i)
    read_operand(bc, pc); // Skip over arguments.

  return dest;
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
        ptr<> proc = state_.value_stack->ref(frame - 1);

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

static void
throw_if_wrong_number_of_args(ptr<procedure> proc, std::size_t num_args) {
  if (num_args < proc->min_args || (!proc->has_rest && num_args > proc->min_args))
    throw error{"{}: Wrong number of arguments, expected {}{}, got {}",
                proc->name ? *proc->name : "<lambda>",
                proc->has_rest ? "at least " : "",
                proc->min_args, num_args};
}

static generic_tracked_ptr
run(execution_state& state) {
  std::optional<execution_action> a;
  gc_disabler no_gc{state.ctx.store};

  integer::value_type& frame_base = state.frame_base;
  integer::value_type& pc = state.pc;

  if (frame_base == frame_preamble_size)
    // Only create an execution_action if this is the top-level execution.
    a.emplace(state);

  while (true) {
    integer::value_type previous_pc = pc;

    root_stack& values = *state.value_stack;
    bytecode const& bc = state.ctx.program;

    opcode opcode = read_opcode(bc, pc);

#ifdef INSIDER_VM_PROFILER
    ++state.ctx.instruction_counts[static_cast<std::size_t>(instr.opcode)];

    struct time_tracker {
      insider::opcode oc;
      context&        ctx;
      std::chrono::high_resolution_clock::time_point begin = std::chrono::high_resolution_clock::now();

      ~time_tracker() {
        auto end = std::chrono::high_resolution_clock::now();
        ctx.instruction_times[static_cast<std::size_t>(oc)] += end - begin;
      }
    } tracker{opcode, state.ctx};
#endif

    switch (opcode) {
    case opcode::no_operation:
      break;

    case opcode::load_static: {
      operand static_num = read_operand(bc, pc);
      operand dest = read_operand(bc, pc);
      values.set(frame_base + dest, state.ctx.get_static(static_num));
      break;
    }

    case opcode::load_top_level: {
      operand global_num = read_operand(bc, pc);
      operand dest = read_operand(bc, pc);
      values.set(frame_base + dest, state.ctx.get_top_level(global_num));
      break;
    }

    case opcode::store_top_level: {
      operand reg = read_operand(bc, pc);
      operand global_num = read_operand(bc, pc);
      state.ctx.set_top_level(global_num, values.ref(frame_base + reg));
      break;
    }

    case opcode::add:
    case opcode::subtract:
    case opcode::multiply:
    case opcode::divide: {
      ptr<> lhs = values.ref(frame_base + read_operand(bc, pc));
      ptr<> rhs = values.ref(frame_base + read_operand(bc, pc));
      operand dest = read_operand(bc, pc);

      if (is<integer>(lhs) && is<integer>(rhs) && opcode != opcode::divide) {
        switch (opcode) {
        case opcode::add:
          if (ptr<> result = add_fixnums(assume<integer>(lhs).value(), assume<integer>(rhs).value()))
            values.set(frame_base + dest, result);
          else
            values.set(frame_base + dest, add(state.ctx, lhs, rhs));
          break;

        case opcode::subtract:
          if (ptr<> result = subtract_fixnums(assume<integer>(lhs).value(), assume<integer>(rhs).value()))
            values.set(frame_base + dest, result);
          else
            values.set(frame_base + dest, subtract(state.ctx, lhs, rhs));
          break;

        case opcode::multiply:
          if (ptr<> result = multiply_fixnums(assume<integer>(lhs).value(), assume<integer>(rhs).value()))
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

      switch (opcode) {
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
      ptr<> lhs = values.ref(frame_base + read_operand(bc, pc));
      ptr<> rhs = values.ref(frame_base + read_operand(bc, pc));
      operand dest = read_operand(bc, pc);

      if (is<integer>(lhs) && is<integer>(rhs)) {
        integer::value_type x = assume<integer>(lhs).value();
        integer::value_type y = assume<integer>(rhs).value();
        ptr<> t = state.ctx.constants->t.get();
        ptr<> f = state.ctx.constants->f.get();

        switch (opcode) {
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

        break;
      }

      switch (opcode) {
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
      operand src = read_operand(bc, pc);
      operand dst = read_operand(bc, pc);
      values.set(frame_base + dst, values.ref(frame_base + src));
      break;
    }

    case opcode::tail_call:
    case opcode::call:
    case opcode::call_top_level:
    case opcode::call_static:
    case opcode::tail_call_top_level:
    case opcode::tail_call_static: {
      bool is_tail = opcode == opcode::tail_call
        || opcode == opcode::tail_call_top_level
        || opcode == opcode::tail_call_static;

      ptr<> callee;
      switch (opcode) {
      case opcode::call:
      case opcode::tail_call:
        callee = values.ref(frame_base + read_operand(bc, pc));
        break;

      case opcode::call_top_level:
      case opcode::tail_call_top_level:
        callee = state.ctx.get_top_level(read_operand(bc, pc));
        break;

      case opcode::call_static:
      case opcode::tail_call_static:
        callee = state.ctx.get_static(read_operand(bc, pc));
        break;

      default:
        assert(false);
      }

      operand dest_register{};
      operand num_args;

      if (is_tail)
        num_args = read_operand(bc, pc);
      else {
        dest_register = read_operand(bc, pc);
        num_args = read_operand(bc, pc);
      }

      ptr<> call_target = callee;
      ptr<insider::closure> closure = nullptr;

      if (auto cls = match<insider::closure>(call_target)) {
        call_target = cls->procedure();
        closure = cls;
      }

      if (auto scheme_proc = match<procedure>(call_target)) {
        throw_if_wrong_number_of_args(scheme_proc, num_args);

        std::size_t closure_size = 0;
        if (closure)
          closure_size = closure->size();

        std::size_t args_size = scheme_proc->min_args;
        if (scheme_proc->has_rest)
          ++args_size;

        std::size_t new_base = push_frame(state, scheme_proc, previous_pc);

        for (std::size_t i = 0; i < closure_size; ++i)
          values.push(closure->ref(i));

        std::size_t num_rest = 0;
        if (scheme_proc->has_rest) {
          num_rest = num_args - scheme_proc->min_args;
          values.change_allocation(values.size() + num_rest + 1);
        }

        for (std::size_t i = 0; i < num_args; ++i)
          values.push(values.ref(frame_base + read_operand(bc, pc)));

        if (scheme_proc->has_rest) {
          values.push(state.ctx.constants->null.get());

          for (std::size_t i = 0; i < num_rest; ++i) {
            ptr<> tail = values.pop();
            ptr<> head = values.pop();
            values.push(cons(state.ctx, head, tail));
          }
        }

        if (is_tail) {
          for (std::size_t i = 0; i < closure_size + args_size; ++i)
            values.set(frame_base + i, values.ref(new_base + i));

          values.set(frame_base - frame_procedure_offset, scheme_proc);
          pc = scheme_proc->entry_pc;
          values.resize(frame_base + scheme_proc->locals_size);
        } else {
          values.resize(new_base + scheme_proc->locals_size);

          pc = scheme_proc->entry_pc;
          frame_base = new_base;
        }
      } else if (auto native_proc = match<native_procedure>(call_target)) {
        assert(!closure);

        values.change_allocation(values.size() + num_args + frame_preamble_size);
        values.push(integer_to_ptr(previous_pc));
        values.push(integer_to_ptr(frame_base));
        values.push(call_target);

        integer::value_type old_base = frame_base;
        frame_base = values.size();

        for (std::size_t i = 0; i < num_args; ++i)
          values.push(values.ref(old_base + read_operand(bc, pc)));

        std::size_t saved_pc = pc;
        ptr<> result;
        ptr<> next_call;
        do {
          result = native_proc->target(state.ctx, values.span(frame_base, num_args));
          next_call = values.ref(frame_base - frame_procedure_offset);
          if (auto np = match<native_procedure>(next_call)) {
            native_proc = np;
            num_args = values.size() - frame_base;
          }
        } while (is<native_procedure>(next_call) && result == state.ctx.constants->tail_call_tag.get());

        if (is<native_procedure>(next_call)) {
          values.shrink(num_args + frame_preamble_size);
          frame_base = old_base;
          pc = saved_pc;

          if (!is_tail)
            values.set(frame_base + dest_register, result);
          else {
            // tail_call. For Scheme procedures, the callee would perform a ret and
            // go back to this frame's caller. Since this is a native procedure, we
            // have to simulate the ret ourselves.

            pop_call_frame(state);

            if (frame_base == -1)
              // Same as in ret below: We're returning from the global procedure.
              return track(state.ctx, result);

            values.set(frame_base + get_destination_register(state), result);
          }
        } else if (is_tail) {
          // This was a tail call and through native tail calls we ended up
          // calling a Scheme procedure. We now need to pop the parent frame
          // (which executed the original tail call) and shift the current frame
          // back.

          std::size_t this_frame_size = values.size() - frame_base;
          for (std::size_t i = 0; i < this_frame_size; ++i)
            values.set(old_base + i, values.ref(frame_base + i));

          values.set(old_base - frame_procedure_offset, values.ref(frame_base - frame_procedure_offset));
          values.resize(old_base + this_frame_size);
          frame_base = old_base;
        }

        no_gc.force_update();
      } else
        throw error{"Application: Not a procedure: {}", datum_to_string(state.ctx, call_target)};
      break;
    }

    case opcode::ret: {
      operand return_reg = read_operand(bc, pc);
      ptr<> result = values.ref(frame_base + return_reg);

      pop_call_frame(state);

      if (frame_base == -1)
        // We are returning from the global procedure, so we return back to the
        // calling C++ code.
        return track(state.ctx, result);

      if (is<native_procedure>(values.ref(frame_base - 1)))
        // Return to a native procedure. We'll abandon run() immediately; the
        // native procedure will then return back to a previous run() call.
        return track(state.ctx, result);

      values.set(frame_base + get_destination_register(state), result);

      no_gc.force_update();
      break;
    }

    case opcode::jump:
    case opcode::jump_back:
    case opcode::jump_unless:
    case opcode::jump_back_unless: {
      operand off;
      operand condition_reg{};

      if (opcode == opcode::jump || opcode == opcode::jump_back)
        off = read_operand(bc, pc);
      else {
        condition_reg = read_operand(bc, pc);
        off = read_operand(bc, pc);
      }

      int offset = (opcode == opcode::jump_back || opcode == opcode::jump_back_unless) ? -off : off;
      if (opcode == opcode::jump_unless || opcode == opcode::jump_back_unless) {
        ptr<> test_value = values.ref(frame_base + condition_reg);

        // The only false value in Scheme is #f. So we only jump if the test_value
        // is exactly #f.

        if (test_value != state.ctx.constants->f.get())
          break;
      }

      pc += offset;
      break;
    }

    case opcode::make_closure: {
      ptr<procedure> proc = assume<procedure>(values.ref(frame_base + read_operand(bc, pc)));
      operand dest = read_operand(bc, pc);
      operand num_captures = read_operand(bc, pc);

      auto result = make<closure>(state.ctx, proc, num_captures);
      for (std::size_t i = 0; i < num_captures; ++i)
        result->set(state.ctx.store, i, values.ref(frame_base + read_operand(bc, pc)));

      values.set(frame_base + dest, result);
      break;
    }

    case opcode::box: {
      ptr<> value = values.ref(frame_base + read_operand(bc, pc));
      values.set(frame_base + read_operand(bc, pc), state.ctx.store.make<box>(value));
      break;
    }

    case opcode::unbox: {
      auto box = expect<insider::box>(values.ref(frame_base + read_operand(bc, pc)));
      values.set(frame_base + read_operand(bc, pc), box->get());
      break;
    }

    case opcode::box_set: {
      auto box = expect<insider::box>(values.ref(frame_base + read_operand(bc, pc)));
      box->set(state.ctx.store, values.ref(frame_base + read_operand(bc,pc)));
      break;
    }

    case opcode::cons: {
      ptr<> car = values.ref(frame_base + read_operand(bc, pc));
      ptr<> cdr = values.ref(frame_base + read_operand(bc, pc));
      values.set(frame_base + read_operand(bc, pc), make<pair>(state.ctx, car, cdr));
      break;
    }

    case opcode::make_vector: {
      operand dest = read_operand(bc, pc);
      operand num_elems = read_operand(bc, pc);

      auto result = make<vector>(state.ctx, state.ctx, num_elems);
      for (std::size_t i = 0; i < num_elems; ++i)
        result->set(state.ctx.store, i, values.ref(frame_base + read_operand(bc, pc)));

      values.set(frame_base + dest, result);
      break;
    }

    case opcode::vector_set: {
      ptr<vector> v = expect<vector>(values.ref(frame_base + read_operand(bc, pc)));
      integer::value_type i = expect<integer>(values.ref(frame_base + read_operand(bc, pc))).value();
      ptr<> o = values.ref(frame_base + read_operand(bc, pc));

      if (i < 0)
        throw std::runtime_error{"vector-set!: Negative index"};

      v->set(state.ctx.store, static_cast<std::size_t>(i), o);
      break;
    }

    case opcode::vector_ref: {
      ptr<vector> v = expect<vector>(values.ref(frame_base + read_operand(bc, pc)));
      integer::value_type i = expect<integer>(values.ref(frame_base + read_operand(bc, pc))).value();

      if (i < 0)
        throw std::runtime_error{"vector-ref: Negative index"};

      values.set(frame_base + read_operand(bc, pc), v->ref(i));
      break;
    }

    default:
      assert(false); // Invalid opcode
    } // end switch
  }

  assert(false); // The only way the loop above will exit is via return.
  return {};
}

static void
push_args(context& ctx, root_stack& values, ptr<procedure> proc, std::vector<ptr<>> const& arguments) {
  std::size_t num_rest = 0;
  if (proc->has_rest) {
    num_rest = arguments.size() - proc->min_args;
    values.change_allocation(values.size() + num_rest + 1);
  }

  for (ptr<> a : arguments)
    values.push(a);

  if (proc->has_rest) {
    values.push(ctx.constants->null.get());

    for (std::size_t i = 0; i < num_rest; ++i) {
      ptr<> tail = values.pop();
      ptr<> head = values.pop();
      values.push(cons(ctx, head, tail));
    }
  }
}

static void
push_scheme_procedure(execution_state& state, ptr<> callable, std::vector<ptr<>> const& arguments) {
  ptr<insider::closure> closure;
  if (auto cls = match<insider::closure>(callable)) {
    closure = cls;
    callable = cls->procedure();
  }

  ptr<procedure> proc = assume<procedure>(callable);

  throw_if_wrong_number_of_args(proc, arguments.size());

  root_stack& values = *state.value_stack;
  std::size_t new_base = push_frame(state, proc, -1);

  state.pc = proc->entry_pc;
  state.frame_base = new_base;

  if (closure)
    for (ptr<> c : collect_closure(closure))
      values.push(c);

  push_args(state.ctx, values, proc, arguments);

  values.resize(new_base + proc->locals_size);
}

static void
push_native_procedure(execution_state& state, ptr<native_procedure> proc, std::vector<ptr<>> const& arguments) {
  root_stack& values = *state.value_stack;
  values.change_allocation(values.size() + arguments.size() + frame_preamble_size);
  values.push(integer_to_ptr(-1)); // Previous PC
  values.push(integer_to_ptr(state.frame_base));
  values.push(proc);
}

generic_tracked_ptr
call(context& ctx, ptr<> callable, std::vector<ptr<>> const& arguments) {
  if (!is_callable(callable))
    throw std::runtime_error{"Expected a callable"};

  if (!ctx.current_execution)
    ctx.current_execution = std::make_unique<execution_state>(ctx);

  generic_tracked_ptr result;

  if (auto native_proc = match<native_procedure>(callable)) {
    push_native_procedure(*ctx.current_execution, native_proc, arguments);
    result = track(ctx, native_proc->target(ctx, object_span(arguments)));
    pop_call_frame(*ctx.current_execution);
  } else {
    push_scheme_procedure(*ctx.current_execution, callable, arguments);
    result = run(*ctx.current_execution);
  }

  if (state_done(*ctx.current_execution))
    ctx.current_execution.reset();

  return result;
}

tracked_ptr<tail_call_tag_type>
tail_call(context& ctx, ptr<> callable, std::vector<ptr<>> const& arguments) {
  if (!is_callable(callable))
    throw std::runtime_error{"Expected a callable"};

  assert(ctx.current_execution);

  ptr<insider::closure> closure;
  if (auto cls = match<insider::closure>(callable)) {
    closure = cls;
    callable = cls->procedure();
  }

  if (auto scheme_proc = match<procedure>(callable))
    throw_if_wrong_number_of_args(scheme_proc, arguments.size());

  root_stack& values = *ctx.current_execution->value_stack;
  std::size_t frame_base = ctx.current_execution->frame_base;
  values.set(frame_base - frame_procedure_offset, callable);
  values.resize(frame_base);

  if (auto scheme_proc = match<procedure>(callable)) {
    values.change_allocation(frame_base + scheme_proc->locals_size);

    if (closure)
      for (std::size_t i = 0; i < closure->size(); ++i)
        values.push(closure->ref(i));

    push_args(ctx, values, scheme_proc, arguments);
    values.resize(frame_base + scheme_proc->locals_size);

    ctx.current_execution->pc = scheme_proc->entry_pc;
  } else {
    assert(!closure);
    assert(is<native_procedure>(callable));

    values.change_allocation(frame_base + arguments.size());
    for (ptr<> a : arguments)
      values.push(a);
  }

  return ctx.constants->tail_call_tag;
}

} // namespace insider
