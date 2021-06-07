#include "vm.hpp"

#include "action.hpp"
#include "converters.hpp"
#include "free_store.hpp"
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
{ }

static bool
state_done(execution_state& state) {
  return !state.current_frame;
}

static std::vector<ptr<>>
collect_closure(ptr<closure> cls) {
  std::vector<ptr<>> result;
  result.reserve(cls->size());
  for (std::size_t i = 0; i < cls->size(); ++i)
    result.push_back(cls->ref(i));

  return result;
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
      ptr<stack_frame> frame = state_.current_frame.get();

      while (frame) {
        if (!first)
          result += '\n';

        ptr<stack_frame> previous_frame = frame->parent;
        ptr<> proc = frame->callable;

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

inline void
throw_if_wrong_number_of_args(ptr<> callable, std::size_t num_args) {
  if (auto cls = match<closure>(callable))
    return throw_if_wrong_number_of_args(cls->procedure(), num_args);
  else {
    auto proc = assume<procedure>(callable);
    if (num_args < proc->min_args || (!proc->has_rest && num_args > proc->min_args))
      throw error{"{}: Wrong number of arguments, expected {}{}, got {}",
                  proc->name ? *proc->name : "<lambda>",
                  proc->has_rest ? "at least " : "",
                  proc->min_args, num_args};
  }
}

static ptr<stack_frame>
make_tail_call(stack_cache& stack, ptr<stack_frame> new_frame) {
  assert(object_generation(new_frame) == generation::stack);
  assert(new_frame->parent);

  ptr<stack_frame> parent = new_frame->parent;

  if (object_generation(new_frame->parent) == generation::stack) {
    parent->callable = new_frame->callable;
    parent->resize(new_frame->size());

    // parent and new_frame may now be overlapping in the stack. We'll copy all
    // new_frame's elements from left to right into parent, possibly overwriting
    // new_frame. That is okay because stack_frame is trivially
    // destructible. We'll then resize the stack afterward to shorten it so that
    // it ends right after parent.

    for (std::size_t i = 0; i < parent->size(); ++i)
      parent->init(i, new_frame->ref(i));

    stack.shorten(reinterpret_cast<std::byte*>(parent.value()) + sizeof(stack_frame) + parent->size() * sizeof(ptr<>));
    return parent;
  } else {
    new_frame->parent = parent->parent;
    new_frame->previous_pc = parent->previous_pc;
    new_frame->parameters = parent->parameters;
    return new_frame;
  }
}

namespace {
  class bytecode_reader {
  public:
    explicit
    bytecode_reader(execution_state& state)
      : bc_{state.ctx.program}
      , pc_{state.pc}
      , previous_pc_{state.pc}
    { }

    opcode
    read_opcode() {
      previous_pc_ = pc_;
      return insider::read_opcode(bc_, pc_);
    }

    operand
    read_operand() { return insider::read_operand(bc_, pc_); }

    integer::value_type
    previous_pc() const { return previous_pc_; }

    integer::value_type
    current_pc() const { return pc_; }

    void
    jump_to(integer::value_type new_pc) { pc_ = new_pc; }

    void
    jump_offset(integer::value_type offset) { pc_ += offset; }

  private:
    bytecode const&      bc_;
    integer::value_type& pc_;
    integer::value_type  previous_pc_;
  };
}

static generic_tracked_ptr
run(execution_state& state) {
  std::optional<execution_action> a;
  gc_disabler no_gc{state.ctx.store};

  if (!state.current_frame->parent)
    // Only create an execution_action if this is the top-level execution.
    a.emplace(state);

  while (true) {
    bytecode_reader reader{state};
    assert(reader.current_pc() >= 0);

    ptr<stack_frame> frame = state.current_frame.get();
    opcode opcode = reader.read_opcode();

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
      operand static_num = reader.read_operand();
      operand dest = reader.read_operand();
      frame->set(dest, state.ctx.get_static(static_num));
      break;
    }

    case opcode::load_top_level: {
      operand global_num = reader.read_operand();
      operand dest = reader.read_operand();
      frame->set(dest, state.ctx.get_top_level(global_num));
      break;
    }

    case opcode::store_top_level: {
      operand reg = reader.read_operand();
      operand global_num = reader.read_operand();
      state.ctx.set_top_level(global_num, frame->ref(reg));
      break;
    }

    case opcode::add:
    case opcode::subtract:
    case opcode::multiply:
    case opcode::divide: {
      ptr<> lhs = frame->ref(reader.read_operand());
      ptr<> rhs = frame->ref(reader.read_operand());
      operand dest = reader.read_operand();

      if (is<integer>(lhs) && is<integer>(rhs) && opcode != opcode::divide) {
        switch (opcode) {
        case opcode::add:
          if (ptr<> result = add_fixnums(assume<integer>(lhs).value(), assume<integer>(rhs).value()))
            frame->set(dest, result);
          else
            frame->set(dest, add(state.ctx, lhs, rhs));
          break;

        case opcode::subtract:
          if (ptr<> result = subtract_fixnums(assume<integer>(lhs).value(), assume<integer>(rhs).value()))
            frame->set(dest, result);
          else
            frame->set(dest, subtract(state.ctx, lhs, rhs));
          break;

        case opcode::multiply:
          if (ptr<> result = multiply_fixnums(assume<integer>(lhs).value(), assume<integer>(rhs).value()))
            frame->set(dest, result);
          else
            frame->set(dest, multiply(state.ctx, lhs, rhs));
          break;

        default:
          assert(false);
          break;
        }

        break;
      }

      switch (opcode) {
      case opcode::add:
        frame->set(dest, add(state.ctx, lhs, rhs));
        break;
      case opcode::subtract:
        frame->set(dest, subtract(state.ctx, lhs, rhs));
        break;
      case opcode::multiply:
        frame->set(dest, multiply(state.ctx, lhs, rhs));
        break;
      case opcode::divide:
        frame->set(dest, truncate_quotient(state.ctx, lhs, rhs));
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
      ptr<> lhs = frame->ref(reader.read_operand());
      ptr<> rhs = frame->ref(reader.read_operand());
      operand dest = reader.read_operand();

      if (is<integer>(lhs) && is<integer>(rhs)) {
        integer::value_type x = assume<integer>(lhs).value();
        integer::value_type y = assume<integer>(rhs).value();
        ptr<> t = state.ctx.constants->t.get();
        ptr<> f = state.ctx.constants->f.get();

        switch (opcode) {
        case opcode::arith_equal:
          frame->set(dest, x == y ? t : f);
          break;

        case opcode::less:
          frame->set(dest, x < y ? t : f);
          break;

        case opcode::greater:
          frame->set(dest, x > y ? t : f);
          break;

        case opcode::less_or_equal:
          frame->set(dest, x <= y ? t : f);
          break;

        case opcode::greater_or_equal:
          frame->set(dest, x >= y ? t : f);
          break;

        default:
          assert(false);
        }

        break;
      }

      switch (opcode) {
      case opcode::arith_equal:
        frame->set(dest, arith_equal(state.ctx, lhs, rhs));
        break;
      case opcode::less:
        frame->set(dest, less(state.ctx, lhs, rhs));
        break;
      case opcode::greater:
        frame->set(dest, greater(state.ctx, lhs, rhs));
        break;
      case opcode::less_or_equal:
        frame->set(dest, less_or_equal(state.ctx, lhs, rhs));
        break;
      case opcode::greater_or_equal:
        frame->set(dest, greater_or_equal(state.ctx, lhs, rhs));
        break;
      default:
        assert(!"Cannot get here");
      }
      break;
    }

    case opcode::set: {
      operand src = reader.read_operand();
      operand dst = reader.read_operand();
      frame->set(dst, frame->ref(src));
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
        callee = frame->ref(reader.read_operand());
        break;

      case opcode::call_top_level:
      case opcode::tail_call_top_level:
        callee = state.ctx.get_top_level(reader.read_operand());
        break;

      case opcode::call_static:
      case opcode::tail_call_static:
        callee = state.ctx.get_static(reader.read_operand());
        break;

      default:
        assert(false);
      }

      operand num_args;
      if (is_tail)
        num_args = reader.read_operand();
      else {
        reader.read_operand(); // Destination register
        num_args = reader.read_operand();
      }

      stack_cache& stack = state.ctx.store.stack();
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

        auto new_frame = stack.make(scheme_proc->locals_size, scheme_proc, frame, reader.previous_pc());

        std::size_t frame_top = 0;
        for (std::size_t j = 0; j < closure_size; ++j)
          new_frame->set(frame_top++, closure->ref(j));

        std::size_t num_rest = 0;
        if (scheme_proc->has_rest)
          num_rest = num_args - scheme_proc->min_args;

        for (std::size_t j = 0; j < scheme_proc->min_args; ++j)
          new_frame->set(frame_top++, frame->ref(reader.read_operand()));

        new_frame->set_rest_to_null(frame_top);

        if (scheme_proc->has_rest) {
          if (num_rest > 0) {
            ptr<pair> head = cons(state.ctx, frame->ref(reader.read_operand()), state.ctx.constants->null.get());
            ptr<pair> last = head;

            for (std::size_t i = 1; i < num_rest; ++i) {
              ptr<pair> new_tail = cons(state.ctx, frame->ref(reader.read_operand()), state.ctx.constants->null.get());
              last->set_cdr(state.ctx.store, new_tail);
              last = new_tail;
            }

            new_frame->set(frame_top++, head);
          } else
            new_frame->set(frame_top++, state.ctx.constants->null.get());
        }

        if (is_tail)
          new_frame = make_tail_call(stack, new_frame);

        state.current_frame = track(state.ctx, new_frame);
        reader.jump_to(scheme_proc->entry_pc);
      } else if (auto native_proc = match<native_procedure>(call_target)) {
        assert(!closure);

        auto new_frame = stack.make(num_args, native_proc, frame, reader.previous_pc());
        for (std::size_t i = 0; i < num_args; ++i)
          new_frame->set(i, frame->ref(reader.read_operand()));

        if (is_tail)
          new_frame = make_tail_call(stack, new_frame);

        state.current_frame = track(state.ctx, new_frame);

        ptr<> result;
        do
        {
          result = native_proc->target(state.ctx, new_frame->span(0, new_frame->size()));
          new_frame = state.current_frame.get();
          if (auto np = match<native_procedure>(new_frame->callable))
            native_proc = np;
        } while (is<native_procedure>(new_frame->callable) && result == state.ctx.constants->tail_call_tag.get());

        if (is<native_procedure>(new_frame->callable)) {
          // Return from a native call (potentially a different native call than
          // what we started with, due to native tail-calls).

          state.pc = new_frame->previous_pc;
          state.current_frame = track(state.ctx, new_frame->parent);
          frame = state.current_frame.get();
          state.ctx.store.stack().deallocate(new_frame);

          if (frame)
            frame->set(get_destination_register(state), result);
          else
            return track(state.ctx, result);
        }

        no_gc.force_update();
      } else
        throw error{"Application: Not a procedure: {}", datum_to_string(state.ctx, call_target)};
      break;
    }

    case opcode::ret: {
      operand return_reg = reader.read_operand();
      ptr<> result = frame->ref(return_reg);

      ptr<stack_frame> old_frame = frame;

      state.pc = frame->previous_pc;
      frame = frame->parent;
      state.current_frame = track(state.ctx, frame);

      state.ctx.store.stack().deallocate(old_frame);

      if (!frame)
        // We are returning from the global procedure, so we return back to the
        // calling C++ code.
        return track(state.ctx, result);

      if (is<native_procedure>(frame->callable))
        // Return to a native procedure. We'll abandon run() immediately; the
        // native procedure will then return back to a previous run() call.
        return track(state.ctx, result);

      frame->set(get_destination_register(state), result);

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
        off = reader.read_operand();
      else {
        condition_reg = reader.read_operand();
        off = reader.read_operand();
      }

      int offset = (opcode == opcode::jump_back || opcode == opcode::jump_back_unless) ? -off : off;
      if (opcode == opcode::jump_unless || opcode == opcode::jump_back_unless) {
        ptr<> test_value = frame->ref(condition_reg);

        // The only false value in Scheme is #f. So we only jump if the test_value
        // is exactly #f.

        if (test_value != state.ctx.constants->f.get())
          break;
      }

      reader.jump_offset(offset);
      break;
    }

    case opcode::make_closure: {
      ptr<procedure> proc = assume<procedure>(frame->ref(reader.read_operand()));
      operand dest = reader.read_operand();
      operand num_captures = reader.read_operand();

      auto result = make<closure>(state.ctx, proc, num_captures);
      for (std::size_t i = 0; i < num_captures; ++i)
        result->set(state.ctx.store, i, frame->ref(reader.read_operand()));

      frame->set(dest, result);
      break;
    }

    case opcode::box: {
      ptr<> value = frame->ref(reader.read_operand());
      frame->set(reader.read_operand(), state.ctx.store.make<box>(value));
      break;
    }

    case opcode::unbox: {
      auto box = expect<insider::box>(frame->ref(reader.read_operand()));
      frame->set(reader.read_operand(), box->get());
      break;
    }

    case opcode::box_set: {
      auto box = expect<insider::box>(frame->ref(reader.read_operand()));
      box->set(state.ctx.store, frame->ref(reader.read_operand()));
      break;
    }

    case opcode::cons: {
      ptr<> car = frame->ref(reader.read_operand());
      ptr<> cdr = frame->ref(reader.read_operand());
      frame->set(reader.read_operand(), make<pair>(state.ctx, car, cdr));
      break;
    }

    case opcode::make_vector: {
      operand dest = reader.read_operand();
      operand num_elems = reader.read_operand();

      auto result = make<vector>(state.ctx, state.ctx, num_elems);
      for (std::size_t i = 0; i < num_elems; ++i)
        result->set(state.ctx.store, i, frame->ref(reader.read_operand()));

      frame->set(dest, result);
      break;
    }

    case opcode::vector_set: {
      ptr<vector> v = expect<vector>(frame->ref(reader.read_operand()));
      integer::value_type i = expect<integer>(frame->ref(reader.read_operand())).value();
      ptr<> o = frame->ref(reader.read_operand());

      if (i < 0)
        throw std::runtime_error{"vector-set!: Negative index"};

      v->set(state.ctx.store, static_cast<std::size_t>(i), o);
      break;
    }

    case opcode::vector_ref: {
      ptr<vector> v = expect<vector>(frame->ref(reader.read_operand()));
      integer::value_type i = expect<integer>(frame->ref(reader.read_operand())).value();

      if (i < 0)
        throw std::runtime_error{"vector-ref: Negative index"};

      frame->set(reader.read_operand(), v->ref(i));
      break;
    }

    default:
      assert(false); // Invalid opcode
    } // end switch
  }

  assert(false); // The only way the loop above will exit is via return.
  return {};
}

static ptr<stack_frame>
make_scheme_frame(execution_state& state, ptr<> callable, std::vector<ptr<>> const& arguments) {
  ptr<insider::closure> closure;
  if (auto cls = match<insider::closure>(callable)) {
    closure = cls;
    callable = cls->procedure();
  }

  ptr<procedure> proc = assume<procedure>(callable);

  throw_if_wrong_number_of_args(proc, arguments.size());

  auto new_frame = state.ctx.store.stack().make(proc->locals_size, callable, state.current_frame.get(), state.pc);

  std::size_t new_frame_top = 0;
  if (closure)
    for (ptr<> c : collect_closure(closure))
      new_frame->set(new_frame_top++, c);

  for (ptr<> a : arguments)
    new_frame->set(new_frame_top++, a);

  new_frame->set_rest_to_null(new_frame_top);

  state.current_frame = track(state.ctx, new_frame);
  state.pc = proc->entry_pc;

  return new_frame;
}

generic_tracked_ptr
call(context& ctx, ptr<> callable, std::vector<ptr<>> const& arguments) {
  if (!is_callable(callable))
    throw std::runtime_error{"Expected a callable"};

  if (!ctx.current_execution)
    ctx.current_execution = std::make_unique<execution_state>(ctx);

  generic_tracked_ptr result;

  if (auto native_proc = match<native_procedure>(callable)) {
    auto new_frame = ctx.store.stack().make(0, native_proc,
                                            ctx.current_execution->current_frame.get(), ctx.current_execution->pc);
    ctx.current_execution->current_frame = track(ctx, new_frame);
    result = track(ctx, native_proc->target(ctx, object_span(arguments)));
    ctx.current_execution->current_frame = track(ctx, new_frame->parent);
    ctx.current_execution->pc = new_frame->previous_pc;
  } else {
    assert(is_callable(callable));

    auto frame = make_scheme_frame(*ctx.current_execution, callable, arguments);
    frame->previous_pc = -1;
    result = run(*ctx.current_execution);
  }

  if (state_done(*ctx.current_execution))
    ctx.current_execution.reset();

  return result;
}

integer::value_type
find_entry_pc(ptr<> callable) {
  if (auto cls = match<closure>(callable))
    return assume<procedure>(cls->procedure())->entry_pc;
  else
    return expect<procedure>(callable)->entry_pc;
}

static ptr<stack_frame>
make_tail_call_frame(context& ctx, ptr<> callable, std::vector<ptr<>> const& arguments) {
  if (!is_callable(callable))
    throw std::runtime_error{"Expected a callable"};

  assert(ctx.current_execution);

  auto current_frame = ctx.current_execution->current_frame.get();

  if (auto native_proc = match<native_procedure>(callable)) {
    auto new_frame = ctx.store.stack().make(arguments.size(), callable, current_frame, current_frame->previous_pc);
    for (std::size_t i = 0; i < arguments.size(); ++i)
      new_frame->set(i, arguments[i]);

    ctx.current_execution->current_frame = track(ctx, make_tail_call(ctx.store.stack(), new_frame));
  } else {
    auto new_frame = make_scheme_frame(*ctx.current_execution, callable, arguments);
    new_frame = make_tail_call(ctx.store.stack(), new_frame);
    ctx.current_execution->pc = find_entry_pc(callable);
    ctx.current_execution->current_frame = track(ctx, new_frame);
  }

  return ctx.current_execution->current_frame.get();
}

tracked_ptr<tail_call_tag_type>
tail_call(context& ctx, ptr<> callable, std::vector<ptr<>> const& arguments) {
  make_tail_call_frame(ctx, callable, arguments);
  return ctx.constants->tail_call_tag;
}

static tracked_ptr<tail_call_tag_type>
capture_stack(context& ctx, ptr<> receiver) {
  ctx.store.stack().transfer_to_nursery();
  auto cont = make<continuation>(ctx, ctx.current_execution->current_frame.get());
  return tail_call(ctx, receiver, {cont});
}

static ptr<>
replace_stack(context& ctx, ptr<continuation> cont, ptr<> value) {
  ctx.store.stack().clear(); // Anything that hasn't been captured will be forever inaccessible anyway.
  ctx.current_execution->current_frame = track(ctx, cont->frame);
  return value;
}

static ptr<parameter_tag>
create_parameter_tag(context& ctx, ptr<> initial_value) {
  auto tag = make<parameter_tag>(ctx);
  ctx.parameters->add_value(tag, initial_value);
  return tag;
}

static ptr<>*
find_parameter_in_frame(ptr<stack_frame> frame, ptr<parameter_tag> tag) {
  if (frame->parameters)
    if (auto value = frame->parameters->find_value(tag))
      return value;
  return nullptr;
}

static ptr<>*
find_parameter_in_stack(context& ctx, ptr<parameter_tag> tag) {
  ptr<stack_frame> current_frame = ctx.current_execution->current_frame.get();

  while (current_frame) {
    if (auto value = find_parameter_in_frame(current_frame, tag))
      return value;

    current_frame = current_frame->parent;
  }

  return nullptr;
}

static ptr<>&
find_parameter(context& ctx, ptr<parameter_tag> tag) {
  if (auto value = find_parameter_in_stack(ctx, tag))
    return *value;

  auto value = ctx.parameters->find_value(tag);
  assert(value);
  return *value;
}

static void
set_parameter(context& ctx, ptr<parameter_tag> tag, ptr<> value) {
  find_parameter(ctx, tag) = value;
}

static void
add_parameter_value(context& ctx, ptr<stack_frame> frame, ptr<parameter_tag> tag, ptr<> value) {
  if (!frame->parameters)
    frame->parameters = make<parameter_map>(ctx);
  frame->parameters->add_value(tag, value);
}

static tracked_ptr<tail_call_tag_type>
call_parameterized(context& ctx, ptr<parameter_tag> tag, ptr<> value, ptr<> callable) {
  ptr<stack_frame> frame = make_tail_call_frame(ctx, callable, {});
  add_parameter_value(ctx, frame, tag, value);
  return ctx.constants->tail_call_tag;
}

void
export_vm(context& ctx, module& result) {
  define_procedure(ctx, "capture-stack", result, true, capture_stack);
  define_procedure(ctx, "replace-stack!", result, true, replace_stack);
  define_procedure(ctx, "create-parameter-tag", result, true, create_parameter_tag);
  define_procedure(ctx, "find-parameter-value", result, true, find_parameter);
  define_procedure(ctx, "set-parameter-value!", result, true, set_parameter);
  define_procedure(ctx, "call-parameterized", result, true, call_parameterized);
}

} // namespace insider
