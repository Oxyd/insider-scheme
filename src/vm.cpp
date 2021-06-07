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

  private:
    bytecode const&      bc_;
    integer::value_type& pc_;
    integer::value_type  previous_pc_;
  };

  struct instruction_state {
    insider::execution_state& execution_state;
    bytecode_reader           reader;

    explicit
    instruction_state(insider::execution_state& es)
      : execution_state{es}
      , reader{es}
    { }

    ptr<stack_frame>
    frame() const { return execution_state.current_frame.get(); }

    insider::context&
    context() { return execution_state.ctx; }
  };
}

static void
load_static(instruction_state& istate) {
  operand static_num = istate.reader.read_operand();
  operand dest = istate.reader.read_operand();
  istate.frame()->set(dest, istate.context().get_static(static_num));
}

static void
load_top_level(instruction_state& istate) {
  operand global_num = istate.reader.read_operand();
  operand dest = istate.reader.read_operand();
  istate.frame()->set(dest, istate.context().get_top_level(global_num));
}

static void
store_top_level(instruction_state& istate) {
  operand reg = istate.reader.read_operand();
  operand global_num = istate.reader.read_operand();
  istate.context().set_top_level(global_num, istate.frame()->ref(reg));
}

static void
arithmetic(opcode opcode, instruction_state& istate) {
  ptr<> lhs = istate.frame()->ref(istate.reader.read_operand());
  ptr<> rhs = istate.frame()->ref(istate.reader.read_operand());
  operand dest = istate.reader.read_operand();

  if (is<integer>(lhs) && is<integer>(rhs) && opcode != opcode::divide) {
    switch (opcode) {
    case opcode::add:
      if (ptr<> result = add_fixnums(assume<integer>(lhs).value(), assume<integer>(rhs).value()))
        istate.frame()->set(dest, result);
      else
        istate.frame()->set(dest, add(istate.context(), lhs, rhs));
      break;

    case opcode::subtract:
      if (ptr<> result = subtract_fixnums(assume<integer>(lhs).value(), assume<integer>(rhs).value()))
        istate.frame()->set(dest, result);
      else
        istate.frame()->set(dest, subtract(istate.context(), lhs, rhs));
      break;

    case opcode::multiply:
      if (ptr<> result = multiply_fixnums(assume<integer>(lhs).value(), assume<integer>(rhs).value()))
        istate.frame()->set(dest, result);
      else
        istate.frame()->set(dest, multiply(istate.context(), lhs, rhs));
      break;

    default:
      assert(false);
      break;
    }

    return;
  }

  switch (opcode) {
  case opcode::add:
    istate.frame()->set(dest, add(istate.context(), lhs, rhs));
    break;
  case opcode::subtract:
    istate.frame()->set(dest, subtract(istate.context(), lhs, rhs));
    break;
  case opcode::multiply:
    istate.frame()->set(dest, multiply(istate.context(), lhs, rhs));
    break;
  case opcode::divide:
    istate.frame()->set(dest, truncate_quotient(istate.context(), lhs, rhs));
    break;
  default:
    assert(!"Cannot get here");
  }
}

static void
relational(opcode opcode, instruction_state& istate) {
  ptr<> lhs = istate.frame()->ref(istate.reader.read_operand());
  ptr<> rhs = istate.frame()->ref(istate.reader.read_operand());
  operand dest = istate.reader.read_operand();

  if (is<integer>(lhs) && is<integer>(rhs)) {
    integer::value_type x = assume<integer>(lhs).value();
    integer::value_type y = assume<integer>(rhs).value();
    ptr<> t = istate.context().constants->t.get();
    ptr<> f = istate.context().constants->f.get();

    switch (opcode) {
    case opcode::arith_equal:
      istate.frame()->set(dest, x == y ? t : f);
      break;

    case opcode::less:
      istate.frame()->set(dest, x < y ? t : f);
      break;

    case opcode::greater:
      istate.frame()->set(dest, x > y ? t : f);
      break;

    case opcode::less_or_equal:
      istate.frame()->set(dest, x <= y ? t : f);
      break;

    case opcode::greater_or_equal:
      istate.frame()->set(dest, x >= y ? t : f);
      break;

    default:
      assert(false);
    }

    return;
  }

  switch (opcode) {
  case opcode::arith_equal:
    istate.frame()->set(dest, arith_equal(istate.context(), lhs, rhs));
    break;
  case opcode::less:
    istate.frame()->set(dest, less(istate.context(), lhs, rhs));
    break;
  case opcode::greater:
    istate.frame()->set(dest, greater(istate.context(), lhs, rhs));
    break;
  case opcode::less_or_equal:
    istate.frame()->set(dest, less_or_equal(istate.context(), lhs, rhs));
    break;
  case opcode::greater_or_equal:
    istate.frame()->set(dest, greater_or_equal(istate.context(), lhs, rhs));
    break;
  default:
    assert(!"Cannot get here");
  }
}

static ptr<>
find_callee(opcode opcode, instruction_state& istate) {
  switch (opcode) {
  case opcode::call:
  case opcode::tail_call:
    return istate.frame()->ref(istate.reader.read_operand());

  case opcode::call_top_level:
  case opcode::tail_call_top_level:
    return istate.context().get_top_level(istate.reader.read_operand());

  case opcode::call_static:
  case opcode::tail_call_static:
    return istate.context().get_static(istate.reader.read_operand());

  default:
    assert(false);
  }

  return {};
}

static std::tuple<ptr<>, ptr<closure>>
read_callee_and_closure(opcode opcode, instruction_state& istate) {
  ptr<> callee = find_callee(opcode, istate);
  ptr<closure> closure;
  if (auto cls = match<insider::closure>(callee)) {
    callee = cls->procedure();
    closure = cls;
  }

  return {callee, closure};
}

static operand
read_num_args(instruction_state& istate, bool is_tail) {
  if (is_tail)
    return istate.reader.read_operand();
  else {
    istate.reader.read_operand(); // Destination register
    return istate.reader.read_operand();
  }
}

static std::size_t
get_closure_size(ptr<closure> cls) {
  if (cls)
    return cls->size();
  else
    return 0;
}

namespace {
  class frame_stack {
  public:
    explicit
    frame_stack(ptr<stack_frame> frame) : frame_{frame} { }

    void
    push(ptr<> value) { frame_->set(top_++, value); }

    void
    set_rest_to_null() { frame_->set_rest_to_null(top_); }

  private:
    ptr<stack_frame> frame_;
    std::size_t      top_ = 0;
  };
}

static void
push_closure(frame_stack& stack, ptr<insider::closure> closure) {
  std::size_t closure_size = get_closure_size(closure);
  for (std::size_t j = 0; j < closure_size; ++j)
    stack.push(closure->ref(j));
}

static void
push_mandatory_args(instruction_state& istate, frame_stack& stack, std::size_t min_args) {
  for (std::size_t j = 0; j < min_args; ++j)
    stack.push(istate.frame()->ref(istate.reader.read_operand()));
}

static ptr<>
convert_args_to_list(instruction_state& istate, std::size_t num_rest) {
  ptr<pair> head = cons(istate.context(),
                        istate.frame()->ref(istate.reader.read_operand()),
                        istate.context().constants->null.get());
  ptr<pair> last = head;

  for (std::size_t i = 1; i < num_rest; ++i) {
    ptr<pair> new_tail = cons(istate.context(),
                              istate.frame()->ref(istate.reader.read_operand()),
                              istate.context().constants->null.get());
    last->set_cdr(istate.context().store, new_tail);
    last = new_tail;
  }

  return head;
}

static void
push_rest_arg(instruction_state& istate, frame_stack& stack, ptr<procedure> proc, std::size_t num_args) {
  std::size_t num_rest = num_args - proc->min_args;

  if (num_rest > 0)
    stack.push(convert_args_to_list(istate, num_rest));
  else
    stack.push(istate.context().constants->null.get());
}

static void
push_arguments_and_closure(ptr<procedure> proc, ptr<insider::closure> closure, instruction_state& istate,
                           ptr<stack_frame> new_frame, std::size_t num_args) {
  frame_stack stack{new_frame};
  push_closure(stack, closure);
  push_mandatory_args(istate, stack, proc->min_args);
  stack.set_rest_to_null();

  if (proc->has_rest)
    push_rest_arg(istate, stack, proc, num_args);
}

static void
jump_to_procedure(execution_state& state, ptr<procedure> proc, ptr<stack_frame> procedure_frame) {
  state.current_frame = track(state.ctx, procedure_frame);
  state.pc = proc->entry_pc;
}

static ptr<stack_frame>
make_scheme_frame(instruction_state& istate, ptr<procedure> proc) {
  stack_cache& stack = istate.context().store.stack();
  return stack.make(proc->locals_size, proc, istate.frame(), istate.reader.previous_pc());
}

static void
push_scheme_call_frame(ptr<procedure> proc, ptr<insider::closure> closure,
                       instruction_state& istate, bool is_tail) {
  operand num_args = read_num_args(istate, is_tail);
  throw_if_wrong_number_of_args(proc, num_args);

  auto new_frame = make_scheme_frame(istate, proc);
  push_arguments_and_closure(proc, closure, istate, new_frame, num_args);

  if (is_tail)
    new_frame = make_tail_call(istate.context().store.stack(), new_frame);

  jump_to_procedure(istate.execution_state, proc, new_frame);
}

static ptr<stack_frame>
make_native_frame(instruction_state& istate, operand num_args, ptr<native_procedure> proc, bool is_tail) {
  stack_cache& stack = istate.context().store.stack();
  auto new_frame = stack.make(num_args, proc, istate.frame(), istate.reader.previous_pc());

  frame_stack args_stack{new_frame};
  push_mandatory_args(istate, args_stack, num_args);

  if (is_tail)
    new_frame = make_tail_call(stack, new_frame);

  return new_frame;
}

static ptr<>
call_native_procedure(instruction_state& istate, ptr<stack_frame> frame, ptr<native_procedure> proc) {
  ptr<> result;
  do
  {
    result = proc->target(istate.context(), frame->span(0, frame->size()));
    frame = istate.execution_state.current_frame.get();
    if (auto np = match<native_procedure>(frame->callable))
      proc = np;
  } while (is<native_procedure>(frame->callable) && result == istate.context().constants->tail_call_tag.get());

  return result;
}

static ptr<>
perform_native_return(execution_state& state, ptr<> result) {
  ptr<stack_frame> current_frame = state.current_frame.get();
  if (is<native_procedure>(current_frame->callable)) {
    // Return from a native call (potentially a different native call than
    // what we started with, due to native tail-calls).

    state.pc = current_frame->previous_pc;
    state.current_frame = track(state.ctx, current_frame->parent);
    ptr<stack_frame> frame = state.current_frame.get();
    state.ctx.store.stack().deallocate(current_frame);

    if (frame)
      frame->set(get_destination_register(state), result);
    else
      return result;
  }

  // Otherwise, the native procedure frame was replaced (by means of a tail
  // call) with a Scheme procedure.

  return {};
}

static ptr<>
do_native_call(ptr<native_procedure> proc, instruction_state& istate, bool is_tail) {
  operand num_args = read_num_args(istate, is_tail);

  ptr<stack_frame> new_frame = make_native_frame(istate, num_args, proc, is_tail);
  istate.execution_state.current_frame = track(istate.context(), new_frame);

  ptr<> result = call_native_procedure(istate, new_frame, proc);
  return perform_native_return(istate.execution_state, result);
}

static bool
is_tail(opcode opcode) {
  return opcode == opcode::tail_call
                || opcode == opcode::tail_call_top_level
                || opcode == opcode::tail_call_static;
}

static ptr<>
call(opcode opcode, instruction_state& istate) {
  auto [call_target, closure] = read_callee_and_closure(opcode, istate);

  if (auto scheme_proc = match<procedure>(call_target)) {
    push_scheme_call_frame(scheme_proc, closure, istate, is_tail(opcode));
    return {};
  } else if (auto native_proc = match<native_procedure>(call_target)) {
    assert(!closure);
    return do_native_call(native_proc, istate, is_tail(opcode));
  } else
    throw error{"Application: Not a procedure: {}", datum_to_string(istate.context(), call_target)};
}

static generic_tracked_ptr
ret(instruction_state& istate) {
  operand return_reg = istate.reader.read_operand();
  ptr<> result = istate.frame()->ref(return_reg);

  ptr<stack_frame> old_frame = istate.frame();

  istate.execution_state.pc = istate.frame()->previous_pc;
  istate.execution_state.current_frame = track(istate.context(), istate.frame()->parent);

  istate.context().store.stack().deallocate(old_frame);

  if (!istate.frame())
    // We are returning from the global procedure, so we return back to the
    // calling C++ code.
    return track(istate.context(), result);

  if (is<native_procedure>(istate.frame()->callable))
    // Return to a native procedure. We'll abandon run() immediately; the
    // native procedure will then return back to a previous run() call.
    return track(istate.context(), result);

  istate.frame()->set(get_destination_register(istate.execution_state), result);
  return {};
}

static void
jump(opcode opcode, instruction_state& istate) {
  operand off;
  operand condition_reg{};

  if (opcode == opcode::jump || opcode == opcode::jump_back)
    off = istate.reader.read_operand();
  else {
    condition_reg = istate.reader.read_operand();
    off = istate.reader.read_operand();
  }

  int offset = (opcode == opcode::jump_back || opcode == opcode::jump_back_unless) ? -off : off;
  if (opcode == opcode::jump_unless || opcode == opcode::jump_back_unless) {
    ptr<> test_value = istate.frame()->ref(condition_reg);

    // The only false value in Scheme is #f. So we only jump if the test_value
    // is exactly #f.

    if (test_value != istate.context().constants->f.get())
      return;
  }

  istate.execution_state.pc += offset;
}

static void
make_closure(instruction_state& istate) {
  ptr<procedure> proc = assume<procedure>(istate.frame()->ref(istate.reader.read_operand()));
  operand dest = istate.reader.read_operand();
  operand num_captures = istate.reader.read_operand();

  auto result = make<closure>(istate.context(), proc, num_captures);
  for (std::size_t i = 0; i < num_captures; ++i)
    result->set(istate.context().store, i, istate.frame()->ref(istate.reader.read_operand()));

  istate.frame()->set(dest, result);
}

static void
make_box(instruction_state& istate) {
  ptr<> value = istate.frame()->ref(istate.reader.read_operand());
  istate.frame()->set(istate.reader.read_operand(), istate.context().store.make<box>(value));
}

static void
unbox(instruction_state& istate) {
  auto box = expect<insider::box>(istate.frame()->ref(istate.reader.read_operand()));
  istate.frame()->set(istate.reader.read_operand(), box->get());
}

static void
box_set(instruction_state& istate) {
  auto box = expect<insider::box>(istate.frame()->ref(istate.reader.read_operand()));
  box->set(istate.context().store, istate.frame()->ref(istate.reader.read_operand()));
}

static void
cons(instruction_state& istate) {
  ptr<> car = istate.frame()->ref(istate.reader.read_operand());
  ptr<> cdr = istate.frame()->ref(istate.reader.read_operand());
  istate.frame()->set(istate.reader.read_operand(), make<pair>(istate.context(), car, cdr));
}

static void
make_vector(instruction_state& istate) {
  operand dest = istate.reader.read_operand();
  operand num_elems = istate.reader.read_operand();

  auto result = make<vector>(istate.context(), istate.context(), num_elems);
  for (std::size_t i = 0; i < num_elems; ++i)
    result->set(istate.context().store, i, istate.frame()->ref(istate.reader.read_operand()));

  istate.frame()->set(dest, result);
}

static void
vector_set(instruction_state& istate) {
  ptr<vector> v = expect<vector>(istate.frame()->ref(istate.reader.read_operand()));
  integer::value_type i = expect<integer>(istate.frame()->ref(istate.reader.read_operand())).value();
  ptr<> o = istate.frame()->ref(istate.reader.read_operand());

  if (i < 0)
    throw std::runtime_error{"vector-set!: Negative index"};

  v->set(istate.context().store, static_cast<std::size_t>(i), o);
}

static void
vector_ref(instruction_state& istate) {
  ptr<vector> v = expect<vector>(istate.frame()->ref(istate.reader.read_operand()));
  integer::value_type i = expect<integer>(istate.frame()->ref(istate.reader.read_operand())).value();

  if (i < 0)
    throw std::runtime_error{"vector-ref: Negative index"};

  istate.frame()->set(istate.reader.read_operand(), v->ref(i));
}

static generic_tracked_ptr
run(execution_state& state) {
  std::optional<execution_action> a;
  gc_disabler no_gc{state.ctx.store};

  if (!state.current_frame->parent)
    // Only create an execution_action if this is the top-level execution.
    a.emplace(state);

  while (true) {
    instruction_state istate{state};
    bytecode_reader& reader = istate.reader;
    ptr<stack_frame> frame = istate.frame();

    opcode opcode = istate.reader.read_opcode();

    switch (opcode) {
    case opcode::no_operation:
      break;

    case opcode::load_static:
      load_static(istate);
      break;

    case opcode::load_top_level:
      load_top_level(istate);
      break;

    case opcode::store_top_level:
      store_top_level(istate);
      break;

    case opcode::add:
    case opcode::subtract:
    case opcode::multiply:
    case opcode::divide:
      arithmetic(opcode, istate);
      break;

    case opcode::arith_equal:
    case opcode::less:
    case opcode::greater:
    case opcode::less_or_equal:
    case opcode::greater_or_equal:
      relational(opcode, istate);
      break;

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
      ptr<> result = call(opcode, istate);
      if (result)
        return track(state.ctx, result);
      no_gc.force_update();
      break;
    }

    case opcode::ret: {
      generic_tracked_ptr result = ret(istate);
      if (result)
        return result;
      no_gc.force_update();
      break;
    }

    case opcode::jump:
    case opcode::jump_back:
    case opcode::jump_unless:
    case opcode::jump_back_unless:
      jump(opcode, istate);
      break;

    case opcode::make_closure:
      make_closure(istate);
      break;

    case opcode::box:
      make_box(istate);
      break;

    case opcode::unbox:
      unbox(istate);
      break;

    case opcode::box_set:
      box_set(istate);
      break;

    case opcode::cons:
      cons(istate);
      break;

    case opcode::make_vector:
      make_vector(istate);
      break;

    case opcode::vector_set:
      vector_set(istate);
      break;

    case opcode::vector_ref:
      vector_ref(istate);
      break;

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
