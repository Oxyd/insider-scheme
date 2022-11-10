#include "vm/vm.hpp"

#include "compiler/compiler.hpp"
#include "compiler/source_code_provider.hpp"
#include "io/read.hpp"
#include "io/write.hpp"
#include "memory/free_store.hpp"
#include "memory/root_provider.hpp"
#include "ptr.hpp"
#include "runtime/action.hpp"
#include "runtime/error.hpp"
#include "runtime/integer.hpp"
#include "runtime/numeric.hpp"
#include "runtime/parameter_map.hpp"
#include "runtime/syntax.hpp"
#include "util/define_procedure.hpp"
#include "util/integer_cast.hpp"
#include "vm/call_stack.hpp"
#include "vm/execution_state.hpp"

#include <cassert>
#include <concepts>
#include <cstdint>
#include <limits>
#include <optional>
#include <stdexcept>
#include <vector>

namespace insider {

static ptr<>
get_constant(ptr<call_stack> stack, operand index) {
  procedure_prototype const& proto
    = assume<procedure>(stack->callable())->prototype();
  assert(index < proto.constants_size);
  return proto.constants[index];
}

static bool
current_frame_is_native(ptr<call_stack> stack) {
  return stack->current_frame_type() != call_stack::frame_type::scheme;
}

static std::size_t
find_index_of_call_instruction(procedure_prototype const& proto,
                               instruction_pointer call_ip) {
  assert(opcode_to_info(opcode::call).num_operands == 3);
  static constexpr std::size_t call_instruction_size = 4;
  return call_ip - proto.code.get() - call_instruction_size;
}

static instruction_pointer const dummy_ip
  = reinterpret_cast<instruction_pointer>(
      std::numeric_limits<std::uintptr_t>::max()
    );

static std::vector<std::string>
find_inlined_procedures(frame_reference frame,
                        std::optional<instruction_pointer> call_ip) {
  if (call_ip) {
    assert(*call_ip != dummy_ip);

    procedure_prototype const& proto
      = assume<procedure>(frame.callable())->prototype();
    std::size_t call_idx = find_index_of_call_instruction(proto, *call_ip);
    debug_info_map const& debug_info = *proto.debug_info;
    if (auto di = debug_info.find(call_idx); di != debug_info.end())
      return di->second.inlined_call_chain;
  }
  return {};
}

static void
append_scheme_frame_to_stacktrace(std::vector<stacktrace_record>& trace,
                                  procedure_prototype const& proto,
                                  frame_reference frame,
                                  std::optional<instruction_pointer> call_ip) {
  auto inlined = find_inlined_procedures(frame, call_ip);
  for (std::string const& inlined_proc : inlined)
    trace.push_back({inlined_proc, stacktrace_record::kind::scheme});

  trace.push_back({*proto.name, stacktrace_record::kind::scheme});
}

static void
append_frame_to_stacktrace(std::vector<stacktrace_record>& trace,
                           frame_reference frame,
                           std::optional<instruction_pointer> call_ip) {
  ptr<> proc = frame.callable();
  if (auto np = match<native_procedure>(proc))
    trace.push_back({np->name, stacktrace_record::kind::native});
  else
    append_scheme_frame_to_stacktrace(trace,
                                      assume<procedure>(proc)->prototype(),
                                      frame,
                                      call_ip);
}

static std::vector<stacktrace_record>
stacktrace(execution_state& state) {
  std::vector<stacktrace_record> result;
  std::optional<instruction_pointer> call_ip;
  for (call_stack::frame_index idx : state.stack->frames_range()) {
    frame_reference frame{state.stack, idx};
    append_frame_to_stacktrace(result, frame, call_ip);
    call_ip = frame.previous_ip();
  }
  return result;
}

std::vector<stacktrace_record>
stacktrace(context& ctx) {
  assert(ctx.current_execution);
  return stacktrace(*ctx.current_execution);
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

      for (auto const& [name, kind] : stacktrace(state_)) {
        if (!first)
          result += '\n';

        result += fmt::format(
          "in {}{}",
          kind == stacktrace_record::kind::native ? "native procedure " : "",
          name
        );

        first = false;
      }

      return result;
    }

  private:
    execution_state& state_;
  };
}

inline void
throw_if_wrong_number_of_args(procedure_prototype const& proc,
                              std::size_t num_args) {
  if (num_args < proc.min_args
      || (!proc.has_rest && num_args > proc.min_args))
    throw make_error("{}: Wrong number of arguments, expected {}{}, got {}",
                     *proc.name,
                     proc.has_rest ? "at least " : "",
                     proc.min_args, num_args);
}

static void
clear_native_continuations(ptr<call_stack> stack) {
  if (auto e = stack->extra())
    e->native_continuations.clear();
}

static opcode
read_opcode(execution_state& state) {
  return insider::read_opcode(state.ip);
}

static operand
read_operand(execution_state& state) {
  return insider::read_operand(state.ip);
}

static void
load_constant(execution_state& state) {
  operand const_num = read_operand(state);
  operand dest = read_operand(state);
  state.stack->local(dest) = get_constant(state.stack, const_num);
}

static void
load_top_level(execution_state& state) {
  operand global_num = read_operand(state);
  operand dest = read_operand(state);
  state.stack->local(dest) = state.ctx.get_top_level(global_num);
}

static void
load_dynamic_top_level(execution_state& state) {
  operand id_idx = read_operand(state);
  operand dest = read_operand(state);

  auto id = assume<syntax>(get_constant(state.stack, id_idx));
  assert(id->contains<symbol>());

  if (auto binding = lookup(id))
    if (binding->variable) {
      assert(is<top_level_variable>(binding->variable));

      state.stack->local(dest) = state.ctx.get_top_level(
        assume<top_level_variable>(binding->variable)->index
      );
      return;
    }

  throw unbound_variable_error{fmt::format(
    "Identifier {} not bound to variable", id->get_symbol()->value()
  )};
}

static void
store_top_level(execution_state& state) {
  operand reg = read_operand(state);
  operand global_num = read_operand(state);
  state.ctx.set_top_level(global_num, state.stack->local(reg));
}

template <auto Constant>
static void
load_special_constant(execution_state& state) {
  operand dest = read_operand(state);
  state.stack->local(dest) = (state.ctx.constants.get())->*Constant;
}

auto load_null = load_special_constant<&context::constants::null>;
auto load_void = load_special_constant<&context::constants::void_>;
auto load_t = load_special_constant<&context::constants::t>;
auto load_f = load_special_constant<&context::constants::f>;
auto load_eof = load_special_constant<&context::constants::eof>;

static void
load_fixnum(execution_state& state) {
  immediate_type value = operand_to_immediate(read_operand(state));
  operand dest = read_operand(state);
  state.stack->local(dest) = integer_to_ptr(value);
}

static void
arithmetic(opcode opcode, execution_state& state) {
  ptr<> lhs = state.stack->local(read_operand(state));
  ptr<> rhs = state.stack->local(read_operand(state));
  operand dest = read_operand(state);

  if (is<integer>(lhs) && is<integer>(rhs) && opcode != opcode::divide) {
    switch (opcode) {
    case opcode::add:
      if (ptr<> result = add_fixnums(assume<integer>(lhs).value(),
                                     assume<integer>(rhs).value()))
        state.stack->local(dest) = result;
      else
        state.stack->local(dest) = add(state.ctx, lhs, rhs);
      break;

    case opcode::subtract:
      if (ptr<> result = subtract_fixnums(assume<integer>(lhs).value(),
                                          assume<integer>(rhs).value()))
        state.stack->local(dest) = result;
      else
        state.stack->local(dest) = subtract(state.ctx, lhs, rhs);
      break;

    case opcode::multiply:
      if (ptr<> result = multiply_fixnums(assume<integer>(lhs).value(),
                                          assume<integer>(rhs).value()))
        state.stack->local(dest) = result;
      else
        state.stack->local(dest) = multiply(state.ctx, lhs, rhs);
      break;

    default:
      assert(false);
      break;
    }

    return;
  }

  switch (opcode) {
  case opcode::add:
    state.stack->local(dest) = add(state.ctx, lhs, rhs);
    break;
  case opcode::subtract:
    state.stack->local(dest) = subtract(state.ctx, lhs, rhs);
    break;
  case opcode::multiply:
    state.stack->local(dest) = multiply(state.ctx, lhs, rhs);
    break;
  case opcode::divide:
    state.stack->local(dest) = divide(state.ctx, lhs, rhs);
    break;
  default:
    assert(!"Cannot get here");
  }
}

static void
relational(opcode opcode, execution_state& state) {
  ptr<> lhs = state.stack->local(read_operand(state));
  ptr<> rhs = state.stack->local(read_operand(state));
  operand dest = read_operand(state);

  if (is<integer>(lhs) && is<integer>(rhs)) {
    integer::value_type x = assume<integer>(lhs).value();
    integer::value_type y = assume<integer>(rhs).value();
    ptr<> t = state.ctx.constants->t;
    ptr<> f = state.ctx.constants->f;

    switch (opcode) {
    case opcode::arith_equal:
      state.stack->local(dest) = x == y ? t : f;
      break;

    case opcode::less:
      state.stack->local(dest) = x < y ? t : f;
      break;

    case opcode::greater:
      state.stack->local(dest) = x > y ? t : f;
      break;

    case opcode::less_or_equal:
      state.stack->local(dest) = x <= y ? t : f;
      break;

    case opcode::greater_or_equal:
      state.stack->local(dest) = x >= y ? t : f;
      break;

    default:
      assert(false);
    }

    return;
  }

  switch (opcode) {
  case opcode::arith_equal:
    state.stack->local(dest) = arith_equal(state.ctx, lhs, rhs);
    break;
  case opcode::less:
    state.stack->local(dest) = less(state.ctx, lhs, rhs);
    break;
  case opcode::greater:
    state.stack->local(dest) = greater(state.ctx, lhs, rhs);
    break;
  case opcode::less_or_equal:
    state.stack->local(dest) = less_or_equal(state.ctx, lhs, rhs);
    break;
  case opcode::greater_or_equal:
    state.stack->local(dest) = greater_or_equal(state.ctx, lhs, rhs);
    break;
  default:
    assert(!"Cannot get here");
  }
}

static void
set(execution_state& state) {
  operand src = read_operand(state);
  operand dst = read_operand(state);
  state.stack->local(dst) = state.stack->local(src);
}

static std::size_t
get_closure_size(ptr<procedure> proc) {
  if (proc)
    return proc->size();
  else
    return 0;
}

static std::size_t
actual_args_size(procedure_prototype const& proto) {
  return proto.min_args + (proto.has_rest ? 1 : 0);
}

static void
push_closure(ptr<procedure> proc, ptr<call_stack> stack) {
  std::size_t closure_size = get_closure_size(proc);
  std::size_t begin = actual_args_size(proc->prototype()) + 1;

  for (std::size_t i = 0; i < closure_size; ++i)
    stack->local(operand(begin + i)) = proc->ref(i);
}

static void
convert_tail_args_to_list(context& ctx, ptr<call_stack> stack,
                          std::size_t tail_base, std::size_t num_rest) {
  // Here's an interesting situation: Imagine that a Scheme procedure is calling
  // a variadic procedure with no arguments for the tail parameter. Furthermore,
  // the mandatory arguments are given as the very end of the caller's stack
  // frame. This means that there's no space for the tail argument in the
  // caller's frame.

  if (stack->frame_size() <= tail_base)
    stack->resize_current_frame(tail_base + 1);

  stack->local(operand(tail_base)) = make_list_from_range(
    ctx, std::views::iota(tail_base, tail_base + num_rest),
    [&] (std::size_t i) { return stack->local(operand(i)); }
  );
}

static void
convert_tail_args(context& ctx, ptr<call_stack> stack,
                  procedure_prototype const& proto, std::size_t base,
                  std::size_t num_args) {
  if (proto.has_rest)
    convert_tail_args_to_list(ctx, stack, base + proto.min_args + 1,
                              num_args - proto.min_args);
}

static instruction_pointer
current_procedure_bytecode_base(execution_state const& state) {
  return assume<procedure>(state.stack->callable())->prototype().code.get();
}

static void
push_scheme_frame(execution_state& state, ptr<procedure> proc,
                  operand base, operand result_reg) {
  state.stack->push_frame(
    proc,
    state.stack->frame_base() + base,
    proc->prototype().locals_size,
    state.ip,
    result_reg
  );
}

static void
make_tail_call_frame(ptr<call_stack> stack, ptr<> proc,
                     std::size_t locals_size,
                     std::size_t args_base, std::size_t num_args) {
  stack->replace_frame(proc, locals_size, args_base, num_args);
  clear_native_continuations(stack);
}

static void
make_scheme_tail_call_frame(ptr<call_stack> stack, ptr<procedure> proc,
                            std::size_t args_base, std::size_t num_args) {
  make_tail_call_frame(stack, proc, proc->prototype().locals_size, args_base,
                       num_args);
}

static void
check_and_convert_scheme_call_arguments(execution_state& state,
                                        procedure_prototype const& proto,
                                        operand base) {
  operand num_args = read_operand(state);
  throw_if_wrong_number_of_args(proto, num_args);
  convert_tail_args(state.ctx, state.stack, proto, base, num_args);
}

static void
make_scheme_frame(ptr<procedure> proc, execution_state& state,
                  operand base, bool is_tail) {
  check_and_convert_scheme_call_arguments(state, proc->prototype(), base);
  if (!is_tail) {
    operand result_reg = read_operand(state);
    push_scheme_frame(state, proc, base, result_reg);
  } else
    make_scheme_tail_call_frame(state.stack, proc, base,
                                actual_args_size(proc->prototype()));
}

static void
push_scheme_call_frame(ptr<procedure> proc, execution_state& state,
                       operand base, bool is_tail) {
  make_scheme_frame(proc, state, base, is_tail);
  push_closure(proc, state.stack);
  state.ip = proc->prototype().code.get();
}

static void
make_native_non_tail_call_frame(execution_state& state,
                                ptr<native_procedure> proc, std::size_t base,
                                std::size_t num_args, operand dest_reg) {
  state.stack->push_frame(
    proc,
    state.stack->frame_base() + base,
    num_args + 1,
    state.ip,
    dest_reg
  );
}

static void
make_native_tail_call_frame(ptr<call_stack> stack,
                            ptr<native_procedure> proc,
                            std::size_t args_base, std::size_t num_args) {
  make_tail_call_frame(stack, proc, num_args + 1, args_base, num_args);
}

static void
discard_later_native_continuations(ptr<stack_frame_extra_data> e,
                                   std::size_t continuations_size) {
  if (e)
    e->native_continuations.resize(continuations_size);
}

static native_continuation_type
find_current_native_continuation(ptr<call_stack> stack) {
  if (auto e = stack->extra())
    if (!e->native_continuations.empty())
      return e->native_continuations.back();

  return {};
}

static ptr<>
call_native_frame_target(context& ctx, ptr<call_stack> stack,
                         ptr<> scheme_result) {
  if (native_continuation_type cont = find_current_native_continuation(stack))
    return cont(ctx, scheme_result);
  else {
    auto proc_and_args = stack->current_frame_span();
    auto proc = assume<native_procedure>(proc_and_args[0]);
    return proc->target(ctx, proc, proc_and_args.subspan<1>());
  }
}

static void
pop_frame(execution_state& state) {
  instruction_pointer previous_ip = state.stack->previous_ip();

  state.stack->pop_frame();
  state.ip = previous_ip;
}

static void
resume_native_call(execution_state& state, ptr<> scheme_result);

static inline void // GCC won't inline this function automatically.
pop_frame_and_set_return_value(execution_state& state, ptr<> result) {
  assert(result);

  ptr<call_stack> stack = state.stack;
  operand dest_reg = stack->result_register();
  pop_frame(state);

  if (state.stack->empty())
    // We are returning from the global procedure, so we return back to the
    // calling C++ code.
    state.result = result;
  else if (current_frame_is_native(state.stack))
    resume_native_call(state, result);
  else
    state.stack->local(dest_reg) = result;
}

static void
call_native_procedure(execution_state& state, ptr<> scheme_result = {}) {
  ptr<> result;
  do
    result = call_native_frame_target(state.ctx, state.stack, scheme_result);
  while (result == state.ctx.constants->tail_call_tag
         && current_frame_is_native(state.stack));

  if (current_frame_is_native(state.stack))
    // Return from a native call (potentially a different native call than
    // what we started with, due to native tail-calls).
    pop_frame_and_set_return_value(state, result);

  // Otherwise, the native procedure frame was replaced (by means of a tail
  // call) with a Scheme procedure.
}

static void
make_native_frame(ptr<native_procedure> proc, execution_state& state,
                  operand base, bool is_tail) {
  auto num_args = read_operand(state);
  if (!is_tail) {
    auto dest_reg = read_operand(state);
    make_native_non_tail_call_frame(state, proc, base, num_args, dest_reg);
  } else
    make_native_tail_call_frame(state.stack, proc, base, num_args);
}

static void
do_native_call(ptr<native_procedure> proc, execution_state& state,
               operand base, bool is_tail) {
  make_native_frame(proc, state, base, is_tail);
  call_native_procedure(state);
}

template <bool Scheme, bool Native, bool Tail>
static void
call(execution_state& state) {
  operand base = read_operand(state);
  ptr<> callee = state.stack->local(base);

  if (Scheme || is<procedure>(callee))
    push_scheme_call_frame(assume<procedure>(callee), state, base, Tail);
  else if (Native || is<native_procedure>(callee))
    do_native_call(assume<native_procedure>(callee), state, base, Tail);
  else
    throw make_error("Application: Not a procedure: {}",
                     datum_to_string(state.ctx, callee));
}

static std::size_t
native_continuations_size(execution_state const& state) {
  return reinterpret_cast<std::size_t>(state.ip);
}

static void
resume_native_call(execution_state& state, ptr<> scheme_result) {
  discard_later_native_continuations(state.stack->extra(),
                                     native_continuations_size(state));

  if (find_current_native_continuation(state.stack))
    call_native_procedure(state, scheme_result);
  else
    // Return to a non-continuable native procedure. We'll abandon run()
    // immediately; the native procedure will then return back to a previous
    // run() call.
    state.result = scheme_result;
}

static void
ret(execution_state& state) {
  ptr<> result = state.stack->local(read_operand(state));
  pop_frame_and_set_return_value(state, result);
}

static void
jump(execution_state& state) {
  immediate_type offset = operand_to_immediate(read_operand(state));
  state.ip += offset;
}

static void
jump_unless(execution_state& state) {
  ptr<> test_value = state.stack->local(read_operand(state));
  immediate_type offset = operand_to_immediate(read_operand(state));
  if (test_value == state.ctx.constants->f)
    state.ip += offset;
}

static void
make_closure(execution_state& state) {
  operand base = read_operand(state);
  ptr<procedure_prototype> proto
    = assume<procedure_prototype>(state.stack->local(base));

  auto num_captures = read_operand(state);
  operand captures_base = base + 1;
  operand dest = read_operand(state);

  auto result = make<procedure>(state.ctx, proto, num_captures);
  for (std::size_t i = 0; i < num_captures; ++i)
    result->set(state.ctx.store, i,
                state.stack->local(operand(captures_base + i)));

  state.stack->local(dest) = result;
}

static void
make_box(execution_state& state) {
  ptr<> value = state.stack->local(read_operand(state));
  state.stack->local(read_operand(state))
    = state.ctx.store.make<box>(value);
}

static void
unbox(execution_state& state) {
  auto box
    = expect<insider::box>(state.stack->local(read_operand(state)));
  state.stack->local(read_operand(state)) = box->get();
}

static void
box_set(execution_state& state) {
  auto box
    = expect<insider::box>(state.stack->local(read_operand(state)));
  box->set(state.ctx.store,
           state.stack->local(read_operand(state)));
}

static void
cons(execution_state& state) {
  ptr<> car = state.stack->local(read_operand(state));
  ptr<> cdr = state.stack->local(read_operand(state));
  state.stack->local(read_operand(state))
    = make<pair>(state.ctx, car, cdr);
}

static void
car(execution_state& state) {
  ptr<pair> p = expect<pair>(state.stack->local(read_operand(state)));
  state.stack->local(read_operand(state)) = car(p);
}

static void
cdr(execution_state& state) {
  ptr<pair> p = expect<pair>(state.stack->local(read_operand(state)));
  state.stack->local(read_operand(state)) = cdr(p);
}

static void
eq(execution_state& state) {
  ptr<> lhs = state.stack->local(read_operand(state));
  ptr<> rhs = state.stack->local(read_operand(state));
  state.stack->local(read_operand(state))
    = lhs == rhs
      ? state.ctx.constants->t
      : state.ctx.constants->f;
}

static void
vector_set(execution_state& state) {
  ptr<vector> v
    = expect<vector>(state.stack->local(read_operand(state)));
  integer::value_type i = expect<integer>(
    state.stack->local(read_operand(state))
  ).value();
  ptr<> o = state.stack->local(read_operand(state));

  if (i < 0)
    throw std::runtime_error{"vector-set!: Negative index"};

  v->set(state.ctx.store, static_cast<std::size_t>(i), o);
}

static void
vector_ref(execution_state& state) {
  ptr<vector> v
    = expect<vector>(state.stack->local(read_operand(state)));
  integer::value_type i = expect<integer>(
    state.stack->local(read_operand(state))
  ).value();

  if (i < 0)
    throw std::runtime_error{"vector-ref: Negative index"};

  state.stack->local(read_operand(state)) = v->ref(i);
}

static void
type(execution_state& state) {
  ptr<> o = state.stack->local(read_operand(state));
  state.stack->local(read_operand(state))
    = type(state.ctx, o);
}

static void
do_instruction(execution_state& state, gc_disabler& no_gc) {
  assert(!state.stack->empty());
  assert(is<procedure>(state.stack->callable()));

  opcode opcode = read_opcode(state);

  switch (opcode) {
  case opcode::no_operation:                                             break;
  case opcode::load_constant:          load_constant(state);             break;
  case opcode::load_top_level:         load_top_level(state);            break;
  case opcode::load_dynamic_top_level: load_dynamic_top_level(state);    break;
  case opcode::store_top_level:        store_top_level(state);           break;
  case opcode::load_null:              load_null(state);                 break;
  case opcode::load_void:              load_void(state);                 break;
  case opcode::load_t:                 load_t(state);                    break;
  case opcode::load_f:                 load_f(state);                    break;
  case opcode::load_eof:               load_eof(state);                  break;
  case opcode::load_fixnum:            load_fixnum(state);               break;
  case opcode::add:
  case opcode::subtract:
  case opcode::multiply:
  case opcode::divide:                 arithmetic(opcode, state);        break;
  case opcode::arith_equal:
  case opcode::less:
  case opcode::greater:
  case opcode::less_or_equal:
  case opcode::greater_or_equal:       relational(opcode, state);        break;
  case opcode::set:                    set(state);                       break;
  case opcode::tail_call:              call<false, false, true>(state);  break;
  case opcode::call:                   call<false, false, false>(state); break;
  case opcode::scheme_tail_call:       call<true, false, true>(state);   break;
  case opcode::scheme_call:            call<true, false, false>(state);  break;
  case opcode::native_tail_call:       call<false, true, true>(state);   break;
  case opcode::native_call:            call<false, true, false>(state);  break;
  case opcode::ret:                    ret(state);                       break;
  case opcode::jump:                   jump(state);                      break;
  case opcode::jump_unless:            jump_unless(state);               break;
  case opcode::make_closure:           make_closure(state);              break;
  case opcode::box:                    make_box(state);                  break;
  case opcode::unbox:                  unbox(state);                     break;
  case opcode::box_set:                box_set(state);                   break;
  case opcode::cons:                   cons(state);                      break;
  case opcode::car:                    car(state);                       break;
  case opcode::cdr:                    cdr(state);                       break;
  case opcode::eq:                     eq(state);                        break;
  case opcode::vector_set:             vector_set(state);                break;
  case opcode::vector_ref:             vector_ref(state);                break;
  case opcode::type:                   type(state);                      break;

  default:
    assert(false); // Invalid opcode
  } // end switch

  if (opcode == opcode::ret
      || opcode == opcode::tail_call
      || opcode == opcode::scheme_tail_call
      || opcode == opcode::native_tail_call)
    no_gc.force_update();
}

static ptr<tail_call_tag_type>
raise(context& ctx, ptr<> e);

static void
do_instructions(execution_state& state) {
  gc_disabler no_gc{state.ctx.store};

  while (true)
    try {
      while (true) {
        do_instruction(state, no_gc);
        if (state.result)
          return;
      }
    } catch (ptr<> e) {
      raise(state.ctx, e);
    } catch (tracked_ptr<> const& e) {
      raise(state.ctx, e.get());
    } catch (scheme_exception& e) {
      throw e;
    } catch (translatable_runtime_error& e) {
      raise(state.ctx, e.translate(state.ctx));
    } catch (...) {
      raise(state.ctx, make<cxx_exception>(state.ctx, std::current_exception()));
    }

  assert(false); // The only way the loop above will exit is via return.
}

static ptr<>
run(execution_state& state) {
  std::optional<execution_action> a;
  assert(!state.result);

  if (!state.stack->parent())
    // Only create an execution_action if this is the top-level execution.
    a.emplace(state);

  do_instructions(state);

  ptr<> result = state.result;
  state.result = {};
  return result;
}

static void
push_rest_argument_for_call_from_native(context& ctx,
                                        procedure_prototype const& proto,
                                        ptr<call_stack> stack,
                                        auto tail_begin,
                                        auto tail_end) {
  stack->local(operand(proto.min_args + 1))
    = make_list_from_range(ctx,
                           std::ranges::subrange(tail_begin, tail_end));
}

static void
push_scheme_arguments_for_call_from_native(context& ctx,
                                           ptr<procedure> callable,
                                           ptr<call_stack> stack,
                                           auto const& args) {
  procedure_prototype const& proto = callable->prototype();

  stack->local(operand{0}) = callable;
  auto arg_it = args.begin();
  for (std::size_t i = 0; i < proto.min_args; ++i)
    stack->local(operand(i) + 1) = *arg_it++;

  if (proto.has_rest)
    push_rest_argument_for_call_from_native(ctx, proto, stack,
                                            arg_it, args.end());
}

static void
make_scheme_frame_for_call_from_native(execution_state& state,
                                       ptr<procedure> callable,
                                       auto const& arguments,
                                       instruction_pointer previous_ip) {
  throw_if_wrong_number_of_args(callable->prototype(), arguments.size());

  state.stack->push_frame(callable, state.stack->size(),
                          callable->prototype().locals_size, previous_ip, {});

  push_scheme_arguments_for_call_from_native(state.ctx, callable, state.stack,
                                             arguments);
  push_closure(callable, state.stack);
  state.ip = callable->prototype().code.get();
}

static ptr<stack_frame_extra_data>
create_or_get_extra_data(context& ctx, frame_reference frame) {
  if (auto e = frame.extra())
    return e;
  else {
    frame.set_extra(make<stack_frame_extra_data>(ctx));
    return frame.extra();
  }
}

static ptr<stack_frame_extra_data>
create_or_get_extra_data(context& ctx, ptr<call_stack> stack) {
  if (auto e = stack->extra())
    return e;
  else {
    stack->set_extra(make<stack_frame_extra_data>(ctx));
    return stack->extra();
  }
}

static void
erect_barrier(context& ctx, ptr<call_stack> stack,
              bool allow_out, bool allow_in) {
  ptr<stack_frame_extra_data> extra = create_or_get_extra_data(ctx, stack);
  extra->allow_jump_out = allow_out;
  extra->allow_jump_in = allow_in;
}

namespace {
  class native_frame_guard : public root_provider {
  public:
    native_frame_guard(context& ctx, ptr<call_stack> stack)
      : root_provider{ctx.store}
      , stack_{stack}
    { }

    ~native_frame_guard() override {
      if (stack_)
        stack_->pop_frame();
    }

    native_frame_guard(native_frame_guard const&) = delete;

    native_frame_guard(native_frame_guard&& other) noexcept
      : root_provider{std::move(other)}
      , stack_{other.stack_}
    {
      other.stack_ = nullptr;
    }

    void
    operator = (native_frame_guard const&) = delete;

  private:
    ptr<call_stack> stack_;

    void
    visit_roots(member_visitor const& f) override {
      f(stack_);
    }
  };
}

[[nodiscard]] static native_frame_guard
setup_native_frame_for_call_from_native(execution_state& state,
                                        ptr<native_procedure> proc) {
  state.stack->push_frame(proc, state.stack->size(), 1, state.ip, {});
  state.stack->local(0) = proc;
  return {state.ctx, state.stack};
}

static ptr<>
call_native_in_current_frame(execution_state& state, ptr<native_procedure> proc,
                             std::vector<ptr<>> const& arguments) {
  ptr<> result = proc->target(state.ctx, proc,
                              object_span(arguments.begin(), arguments.end()));
  return result;
}

static ptr<>
call_native_from_native(execution_state& state, ptr<native_procedure> proc,
                        std::vector<ptr<>> const& arguments) {
  native_frame_guard guard
    = setup_native_frame_for_call_from_native(state, proc);
  return call_native_in_current_frame(state, proc, arguments);
}

static void
add_parameter_value(context& ctx, ptr<call_stack> stack, ptr<parameter_tag> tag,
                    ptr<> value);

static ptr<>
call_native_with_continuation_barrier(execution_state& state,
                                      ptr<native_procedure> proc,
                                      std::vector<ptr<>> const& arguments,
                                      parameter_assignments const& params,
                                      bool allow_jump_out,
                                      bool allow_jump_in) {
  native_frame_guard guard
    = setup_native_frame_for_call_from_native(state, proc);

  if (state.stack->parent() != -1)
    erect_barrier(state.ctx, state.stack, allow_jump_out, allow_jump_in);

  for (auto const& p : params)
    add_parameter_value(state.ctx, state.stack, p.tag, p.value);

  return call_native_in_current_frame(state, proc, arguments);
}

static instruction_pointer
continuations_size_to_ip(std::size_t size) {
  return reinterpret_cast<instruction_pointer>(size);
}

static instruction_pointer
get_native_ip(ptr<call_stack> stack) {
  if (!stack->empty() && current_frame_is_native(stack))
    if (auto e = stack->extra())
      return continuations_size_to_ip(e->native_continuations.size());
  return continuations_size_to_ip(std::numeric_limits<std::size_t>::max());
}

static void
setup_scheme_frame_for_potential_call_from_native(
  execution_state& state,
  ptr<procedure> callable,
  std::vector<ptr<>> const& arguments
) {
  assert(is_callable(callable));
  make_scheme_frame_for_call_from_native(state, callable, arguments,
                                         get_native_ip(state.stack));
}

static ptr<>
call_scheme_with_continuation_barrier(execution_state& state,
                                      ptr<procedure> callable,
                                      std::vector<ptr<>> const& arguments,
                                      parameter_assignments const& params,
                                      bool allow_jump_out,
                                      bool allow_jump_in) {
  make_scheme_frame_for_call_from_native(state, callable, arguments,
                                         get_native_ip(state.stack));
  if (state.stack->parent())
    erect_barrier(state.ctx, state.stack, allow_jump_out, allow_jump_in);

  for (auto const& p : params)
    add_parameter_value(state.ctx, state.stack, p.tag, p.value);

  return run(state);
}

namespace {
  class current_execution_setter {
  public:
    explicit
    current_execution_setter(context& ctx)
      : ctx_{ctx}
    {
      if (!ctx.current_execution) {
        ctx.current_execution = std::make_unique<execution_state>(ctx);
        created_ = true;
      }
    }

    ~current_execution_setter() {
      if (created_)
        ctx_.current_execution.reset();
    }

    current_execution_setter(current_execution_setter const&) = delete;
    void operator = (current_execution_setter const&) = delete;

  private:
    context& ctx_;
    bool     created_ = false;
  };
}

static void
mark_current_frame_noncontinuable(ptr<call_stack> stack) {
  if (!stack->empty())
    if (ptr<stack_frame_extra_data> e = stack->extra())
      e->native_continuations.emplace_back();
}

ptr<>
call_parameterized_with_continuation_barrier(
  context& ctx,
  parameter_assignments const& params,
  ptr<> callable,
  std::vector<ptr<>> const& arguments,
  bool allow_jump_out,
  bool allow_jump_in
) {
  expect_callable(callable);

  current_execution_setter ces{ctx};
  mark_current_frame_noncontinuable(ctx.current_execution->stack);

  if (auto native_proc = match<native_procedure>(callable))
    return call_native_with_continuation_barrier(
      *ctx.current_execution, native_proc, arguments, params,
      allow_jump_out, allow_jump_in
    );
  else
    return call_scheme_with_continuation_barrier(
      *ctx.current_execution, assume<procedure>(callable), arguments, params,
      allow_jump_out, allow_jump_in
    );
}

ptr<>
call_with_continuation_barrier(context& ctx, ptr<> callable,
                               std::vector<ptr<>> const& arguments) {
  return call_parameterized_with_continuation_barrier(ctx, {}, callable,
                                                      arguments);
}

static ptr<>
call_native_continuable(execution_state& state, ptr<native_procedure> proc,
                        std::vector<ptr<>> const& arguments,
                        native_continuation_type const& cont) {
  ptr<> result = call_native_from_native(state, proc, arguments);
  return cont(state.ctx, result);
}

static ptr<>
call_scheme_continuable(execution_state& state, ptr<procedure> callable,
                        std::vector<ptr<>> const& arguments,
                        native_continuation_type cont) {
  auto extra = create_or_get_extra_data(state.ctx, state.stack);
  extra->native_continuations.emplace_back(std::move(cont));
  setup_scheme_frame_for_potential_call_from_native(state, callable, arguments);
  return state.ctx.constants->tail_call_tag;
}

ptr<tail_call_tag_type>
call_continuable(context& ctx, ptr<> callable,
                 std::vector<ptr<>> const& arguments,
                 native_continuation_type cont) {
  expect_callable(callable);
  current_execution_setter ces{ctx};

  if (auto native_proc = match<native_procedure>(callable))
    call_native_continuable(*ctx.current_execution, native_proc, arguments,
                            cont);
  else
    call_scheme_continuable(*ctx.current_execution, assume<procedure>(callable),
                            arguments, std::move(cont));

  return ctx.constants->tail_call_tag;
}

static void
make_native_tail_call_frame_for_call_from_native(
  ptr<call_stack> stack,
  ptr<native_procedure> callable,
  auto const& arguments
) {
  stack->replace_frame(callable, arguments.size() + 1);
  clear_native_continuations(stack);
  stack->local(operand{0}) = callable;
  auto arg = arguments.begin();
  for (std::size_t i = 0; i < arguments.size(); ++i)
    stack->local(operand(i + 1)) = *arg++;
}

static void
make_scheme_tail_call_frame_for_call_from_native(
  context& ctx,
  ptr<call_stack> stack,
  ptr<procedure> callable,
  auto const& arguments
) {
  throw_if_wrong_number_of_args(callable->prototype(), arguments.size());

  stack->replace_frame(callable, callable->prototype().locals_size);
  push_scheme_arguments_for_call_from_native(ctx, callable, stack, arguments);
  push_closure(callable, stack);
  clear_native_continuations(stack);
}

static void
make_tail_call_frame_for_call_from_native(context& ctx, ptr<> callable,
                                          auto const& arguments) {
  if (!is_callable(callable))
    throw std::runtime_error{"Expected a callable"};

  assert(ctx.current_execution);
  ptr<call_stack> stack = ctx.current_execution->stack;

  if (auto native = match<native_procedure>(callable))
    make_native_tail_call_frame_for_call_from_native(stack, native, arguments);
  else
    make_scheme_tail_call_frame_for_call_from_native(ctx, stack,
                                                     assume<procedure>(callable),
                                                     arguments);
}

static void
install_call_frame(execution_state& state) {
  if (!current_frame_is_native(state.stack))
    state.ip = current_procedure_bytecode_base(state);
}

static ptr<tail_call_tag_type>
tail_call_range(context& ctx, ptr<> callable, auto const& arguments) {
  make_tail_call_frame_for_call_from_native(ctx, callable, arguments);
  install_call_frame(*ctx.current_execution);
  return ctx.constants->tail_call_tag;
}

ptr<tail_call_tag_type>
tail_call(context& ctx, ptr<> callable, std::vector<ptr<>> const& arguments) {
  return tail_call_range(ctx, callable, arguments);
}

// A stack segment is a contiguous sequence of stack frames whose last frame has
// non-empty extra field, or is at the top of the stack. No other frames in a
// segment may have a non-empty extra field. This way, the entire call stack can
// be decomposed into a sequence of segments.
//
// The motivation here is that the extra field can be compared using simple
// pointer comparison to determine the identity of two frames when we are trying
// to merge two different call stacks. Also, the extra field stores things like
// before and after thunks, and the semantics of the language require that
// continuation jumps don't run before and after thunks that don't need to be
// run -- i.e. those in the common segment of two call stacks.

namespace {
  struct stack_segment {
    ptr<stack_frame_extra_data> extra;
    call_stack::frame_index     begin;
    call_stack::frame_index     end; // Index *past* the last frame of this
                                     // segment
  };

  struct stack_segments {
    ptr<call_stack>            stack;
    std::vector<stack_segment> segments;
  };
} // anonymous namespace

static stack_segments
decompose_into_segments(ptr<call_stack> stack) {
  std::vector<stack_segment> result;

  stack_segment current_segment;
  current_segment.end = stack->frames_end();

  call_stack::frame_index previous = stack->frames_end();
  for (call_stack::frame_index frame : stack->frames_range()) {
    if (stack->extra(frame)) {
      current_segment.begin = previous;
      if (current_segment.begin != current_segment.end)
        result.push_back(current_segment);

      current_segment.extra = stack->extra(frame);
      current_segment.end = previous;
    }

    previous = frame;
  }

  current_segment.begin = 0;
  result.push_back(current_segment);

  std::reverse(result.begin(), result.end());
  return {stack, result};
}

static std::size_t
common_prefix_length(std::vector<stack_segment> const& x,
                     std::vector<stack_segment> const& y) {
  std::size_t result = 0;
  while (result < x.size() && result < y.size()
         && x[result].extra && y[result].extra
         && x[result].extra == y[result].extra)
    ++result;
  return result;
}

static std::optional<call_stack::frame_index>
common_frame_index(stack_segments const& segments, std::size_t common_prefix) {
  if (common_prefix == 0)
    return std::nullopt;
  else
    return segments.stack->parent(segments.segments[common_prefix].begin);
}

static ptr<tail_call_tag_type>
capture_stack(context& ctx, ptr<> receiver) {
  auto copy = make<call_stack>(ctx, *ctx.current_execution->stack);
  return tail_call(ctx, receiver, {copy});
}

static bool
all_frames(ptr<call_stack> stack,
           std::optional<call_stack::frame_index> begin,
           std::optional<call_stack::frame_index> end,
           auto pred) {
  while (begin != end)
    if (!pred(frame_reference{stack, *begin}))
      return false;
    else
      begin = stack->parent(*begin);

  return true;
}

static bool
allows_jump_out(frame_reference f) {
  return !f.extra() || f.extra()->allow_jump_out;
}

static bool
allows_jump_in(frame_reference f) {
  return !f.extra() || f.extra()->allow_jump_in;
}

static bool
jump_out_allowed(ptr<call_stack> stack,
                 std::optional<call_stack::frame_index> common) {
  return all_frames(stack, stack->current_frame_index(), common,
                    allows_jump_out);
}

static bool
jump_in_allowed(ptr<call_stack> stack,
                std::optional<call_stack::frame_index> common) {
  return all_frames(stack, stack->current_frame_index(), common,
                    allows_jump_in);
}

static bool
continuation_jump_allowed(ptr<call_stack> current_stack,
                          ptr<call_stack> new_stack,
                          std::optional<call_stack::frame_index> common) {
  return jump_out_allowed(current_stack, common)
         && jump_in_allowed(new_stack, common);
}

static void
throw_if_jump_not_allowed(context& ctx,
                          ptr<call_stack> new_stack,
                          std::optional<call_stack::frame_index> common) {
  if (!continuation_jump_allowed(ctx.current_execution->stack, new_stack,
                                 common))
    throw std::runtime_error{"Continuation jump across barrier not allowed"};
}

static ptr<>
get_after_thunk(ptr<call_stack> stack) {
  if (auto e = stack->extra())
    return e->after_thunk;
  else
    return {};
}

static ptr<>
get_before_thunk(ptr<call_stack> stack) {
  if (auto e = stack->extra())
    return e->before_thunk;
  else
    return {};
}

static void
unwind_stack(execution_state& state,
             std::optional<call_stack::frame_index> end) {
  while (state.stack->current_frame_index() != end) {
    if (ptr<> thunk = get_after_thunk(state.stack))
      call_with_continuation_barrier(state.ctx, thunk, {});

    state.stack->pop_frame();
  }
}

static void
rewind_stack(execution_state& state, stack_segments const& new_segments,
             std::size_t common_prefix) {
  assert(!new_segments.segments.empty());

  for (std::size_t i = common_prefix; i < new_segments.segments.size(); ++i) {
    state.stack->append_frames(
      new_segments.stack->frames(new_segments.segments[i].begin,
                                 new_segments.segments[i].end)
    );
    if (ptr<> thunk = get_before_thunk(state.stack))
      call_with_continuation_barrier(state.ctx, thunk, {});
  }
}

static ptr<>
replace_stack(context& ctx, ptr<call_stack> cont, ptr<> value) {
  stack_segments current_segments
    = decompose_into_segments(ctx.current_execution->stack);
  stack_segments new_segments = decompose_into_segments(cont);
  std::size_t prefix
    = common_prefix_length(current_segments.segments, new_segments.segments);
  std::optional<call_stack::frame_index> common_frame_idx
    = common_frame_index(current_segments, prefix);
  throw_if_jump_not_allowed(ctx, cont, common_frame_idx);

  unwind_stack(*ctx.current_execution, common_frame_idx);
  rewind_stack(*ctx.current_execution, new_segments, prefix);

  return value;
}

static ptr<>
call_with_continuation_barrier(context& ctx, bool allow_out, bool allow_in,
                               ptr<> callable) {
  erect_barrier(ctx, ctx.current_execution->stack, allow_out, allow_in);

  // Non-tail call even though there is nothing to do after this. This is to
  // preserve this frame on the stack because it holds information about the
  // barrier.
  return call_continuable(ctx, callable, {},
                          [] (context&, ptr<> result) { return result; });
}

ptr<parameter_tag>
create_parameter_tag(context& ctx, ptr<> initial_value) {
  auto tag = make<parameter_tag>(ctx);
  ctx.parameters->add_value(ctx.store, tag, initial_value);
  return tag;
}

static ptr<>
find_parameter_in_frame(ptr<call_stack> stack, std::size_t frame_idx,
                        ptr<parameter_tag> tag) {
  if (ptr<stack_frame_extra_data> e = stack->extra(frame_idx))
    for (auto const& p : e->parameters)
      if (p.tag == tag)
        return p.value;

  return {};
}

static std::optional<std::size_t>
find_stack_frame_for_parameter(ptr<call_stack> stack, ptr<parameter_tag> tag) {
  auto idx = stack->current_frame_index();
  while (idx)
    if (find_parameter_in_frame(stack, *idx, tag))
      return idx;
    else
      idx = stack->parent(*idx);
  return std::nullopt;
}

static void
set_parameter_value_in_frame(ptr<stack_frame_extra_data> extra,
                             ptr<parameter_tag> tag, ptr<> value) {
  for (auto& p : extra->parameters)
    if (p.tag == tag) {
      p.value = value;
      return;
    }

  extra->parameters.push_back({tag, value});
}

static void
set_parameter_value_in_frame(ptr<call_stack> stack, std::size_t frame,
                             ptr<parameter_tag> tag, ptr<> value) {
  auto extra = stack->extra(frame);
  set_parameter_value_in_frame(extra, tag, value);
}

static void
set_parameter_value(context& ctx, ptr<parameter_tag> tag, ptr<> value) {
  auto stack = ctx.current_execution->stack;
  auto frame = find_stack_frame_for_parameter(stack, tag);
  if (frame) 
    set_parameter_value_in_frame(stack, *frame, tag, value);
  else
    ctx.parameters->set_value(ctx.store, tag, value);
}

static ptr<>
find_parameter_value_in_stack(context& ctx, ptr<parameter_tag> tag) {
  if (!ctx.current_execution)
    return {};

  auto stack = ctx.current_execution->stack;
  auto frame = stack->current_frame_index();

  while (frame)
    if (auto value = find_parameter_in_frame(stack, *frame, tag))
      return value;
    else
      frame = stack->parent(*frame);

  return {};
}

ptr<>
find_parameter_value(context& ctx, ptr<parameter_tag> tag) {
  if (auto value = find_parameter_value_in_stack(ctx, tag))
    return value;
  else
    return ctx.parameters->find_value(tag);
}

static void
add_parameter_value(context& ctx, ptr<call_stack> stack, ptr<parameter_tag> tag,
                    ptr<> value) {
  ptr<stack_frame_extra_data> extra = create_or_get_extra_data(ctx, stack);
  set_parameter_value_in_frame(extra, tag, value);
}

static ptr<tail_call_tag_type>
call_parameterized(context& ctx, ptr<parameter_tag> tag, ptr<> value,
                   ptr<> callable) {
  add_parameter_value(ctx, ctx.current_execution->stack, tag, value);
  return call_continuable(ctx, callable, {},
                          [] (context&, ptr<> result) { return result; });
}

static ptr<>
dynamic_wind(context& ctx,
             tracked_ptr<> const& before,
             tracked_ptr<> const& thunk,
             tracked_ptr<> const& after) {
  ptr<stack_frame_extra_data> e
    = create_or_get_extra_data(ctx, ctx.current_execution->stack);
  e->before_thunk = before.get();
  e->after_thunk = after.get();

  return call_continuable(
    ctx, before.get(), {},
    [=] (context& ctx, ptr<>) {
      return call_continuable(
        ctx, thunk.get(), {},
        [=] (context& ctx, ptr<> result) {
          return call_continuable(
            ctx, after.get(), {},
            [result = track(ctx, result)] (context&, ptr<>) {
              return result.get();
            }
          );
        }
      );
    }
  );
}

static frame_reference
find_exception_handler_frame(ptr<call_stack> stack, frame_reference frame) {
  while (frame) {
    if (ptr<stack_frame_extra_data> e = frame.extra()) {
      if (e->exception_handler)
        return frame;
      else if (e->next_exception_handler_frame) {
        frame = frame_reference{stack, *e->next_exception_handler_frame};
        continue;
      }
    }

    frame = frame.parent();
  }

  return {};
}

static ptr<tail_call_tag_type>
with_exception_handler(context& ctx, ptr<> handler, ptr<> thunk) {
  call_continuable(ctx, thunk, {},
                   [] (context&, ptr<> result) { return result; });

  ptr<stack_frame_extra_data> extra
    = create_or_get_extra_data(ctx, ctx.current_execution->stack);
  extra->exception_handler = handler;

  return ctx.constants->tail_call_tag;
}

static ptr<>
get_frame_exception_handler(frame_reference frame) {
  assert(frame.extra());
  assert(frame.extra()->exception_handler);
  return frame.extra()->exception_handler;
}

static void
setup_exception_handler_frame(context& ctx,
                              frame_reference invocation_frame,
                              frame_reference definition_frame) {
  ptr<stack_frame_extra_data> invocation_extra
    = create_or_get_extra_data(ctx, invocation_frame);
  invocation_extra->next_exception_handler_frame
    = definition_frame.parent().index();
}

static ptr<tail_call_tag_type>
call_continuable_exception_handler(context& ctx, frame_reference handler_frame,
                                   ptr<> exception) {
  call_continuable(ctx, get_frame_exception_handler(handler_frame), {exception},
                   [] (context&, ptr<> result) { return result; });
  setup_exception_handler_frame(
    ctx, current_frame(ctx.current_execution->stack), handler_frame
  );
  return ctx.constants->tail_call_tag;
}

static ptr<tail_call_tag_type>
raise_from(context& ctx, ptr<> e, frame_reference frame);

static ptr<tail_call_tag_type>
call_noncontinuable_exception_handler(context& ctx,
                                      frame_reference handler_frame,
                                      ptr<> exception) {
  call_continuable(
    ctx, get_frame_exception_handler(handler_frame), {exception},
    [exception = track(ctx, exception),
     handler_frame_idx = handler_frame.index()] (context& ctx, ptr<>) {
      return raise_from(ctx,
                        make<uncaught_exception>(ctx, exception.get()),
                        frame_reference{ctx.current_execution->stack,
                                        handler_frame_idx}.parent());
    }
  );
  setup_exception_handler_frame(ctx,
                                current_frame(ctx.current_execution->stack),
                                handler_frame);
  return ctx.constants->tail_call_tag;
}

[[noreturn]] static void
builtin_exception_handler(context& ctx, ptr<> e) {
  if (auto cxx_e = match<cxx_exception>(e))
    cxx_e->rethrow();
  else
    throw scheme_exception{ctx, e};
}

static ptr<tail_call_tag_type>
raise_from(context& ctx, ptr<> e, frame_reference frame) {
  if (frame_reference handler_frame
      = find_exception_handler_frame(ctx.current_execution->stack, frame))
    return call_noncontinuable_exception_handler(ctx, handler_frame, e);
  else
    builtin_exception_handler(ctx, e);
}

static ptr<tail_call_tag_type>
raise(context& ctx, ptr<> e) {
  return raise_from(ctx, e, current_frame(ctx.current_execution->stack));
}

static ptr<tail_call_tag_type>
raise_continuable(context& ctx, ptr<> e) {
  frame_reference handler_frame = find_exception_handler_frame(
    ctx.current_execution->stack,
    current_frame(ctx.current_execution->stack)
  );
  if (handler_frame)
    return call_continuable_exception_handler(ctx, handler_frame, e);
  else
    builtin_exception_handler(ctx, e);
}

namespace {
  struct apply_args_iterator {
    using iterator_concept = std::forward_iterator_tag;
    using value_type = ptr<>;
    using reference_type = ptr<>;
    using difference_type = std::ptrdiff_t;

    object_span args;
    std::size_t current_arg;
    ptr<>       current_tail_arg;

    ptr<>
    operator * () const {
      if (current_tail_arg)
        return car(assume<pair>(current_tail_arg));
      else
        return args[current_arg];
    }

    apply_args_iterator&
    operator ++ () {
      if (current_tail_arg)
        current_tail_arg = cdr(assume<pair>(current_tail_arg));
      else {
        ++current_arg;
        if (current_arg == args.size() - 1)
          current_tail_arg = args.back();
      }
      return *this;
    }

    apply_args_iterator
    operator ++ (int) {
      apply_args_iterator result{*this};
      operator ++ ();
      return result;
    }

    bool
    operator == (apply_args_iterator const& other) const {
      return current_arg == other.current_arg
             && current_tail_arg == other.current_tail_arg;
    }
  };

  static_assert(std::forward_iterator<apply_args_iterator>);

  struct apply_args_range {
    object_span    args;
    ptr<null_type> null;
    std::size_t    args_size = 0;

    apply_args_range(context& ctx, object_span args)
      : args{args}
      , null{ctx.constants->null}
    {
      assert(args.size() >= 2);
      args_size = args.size() - 2 + list_length(args.back());
    }

    std::size_t
    size() const { return args_size; }

    apply_args_iterator
    begin() const {
      if (args.size() == 2)
        return {args, 1, args.back()};
      else
        return {args, 1, {}};
    }

    apply_args_iterator
    end() const {
      return {args, args.size() - 1, null};
    }
  };
}

static ptr<>
apply(context& ctx, object_span args) {
  if (args.size() < 2)
    throw std::runtime_error{"apply: Expected at least 2 arguments"};

  ptr<> f = args[0];
  return tail_call_range(ctx, f, apply_args_range{ctx, args});
}

static std::vector<ptr<>>
unpack_values(ptr<> v) {
  if (auto values = match<values_tuple>(v)) {
    std::vector<ptr<>> result(values->size());
    for (std::size_t i = 0; i < values->size(); ++i)
      result[i] = values->ref(i);

    return result;
  } else
    return {v};
}

static ptr<tail_call_tag_type>
call_with_values(context& ctx, ptr<> producer, ptr<> consumer) {
  return call_continuable(
    ctx, producer, {},
    [consumer = track(ctx, consumer)] (context& ctx, ptr<> producer_result) {
      return tail_call(ctx, consumer.get(), unpack_values(producer_result));
    }
  );
}

static ptr<>
values(context& ctx, object_span args) {
  if (args.size() == 1)
    return args[0];
  else
    return make<values_tuple>(ctx, args);
}

static ptr<tail_call_tag_type>
eval_proc(context& ctx, ptr<> expr, tracked_ptr<module_> const& m) {
  ptr<syntax> stx = datum_to_syntax(ctx, {}, expr);
  auto f = compile_expression(ctx, stx, m, make_eval_origin());
  return tail_call(ctx, f, {});
}

ptr<>
eval(context& ctx, tracked_ptr<module_> const& mod, ptr<syntax> expr) {
  auto f = compile_expression(ctx, expr, mod, make_eval_origin());
  return call_with_continuation_barrier(ctx, f, {});
}

ptr<>
eval(context& ctx, tracked_ptr<module_> const& mod, std::string const& expr) {
  if (auto stx = match<syntax>(read_syntax(ctx, expr)))
    return eval(ctx, mod, stx);
  else
    throw std::runtime_error{"Unexpected EOF"};
}

void
export_vm(context& ctx, ptr<module_> result) {
  define_procedure<capture_stack>(ctx, "capture-stack", result);
  define_procedure<replace_stack>(ctx, "replace-stack!", result);
  define_procedure<create_parameter_tag>(ctx, "create-parameter-tag", result);
  define_procedure<find_parameter_value>(ctx, "find-parameter-value", result);
  define_procedure<set_parameter_value>(ctx, "set-parameter-value!", result);
  define_procedure<
    static_cast<ptr<> (*)(context&, bool, bool, ptr<>)>(
      call_with_continuation_barrier
    )
  >(ctx, "call-with-continuation-barrier", result);
  define_procedure<call_parameterized>(ctx, "call-parameterized", result);
  define_procedure<dynamic_wind>(ctx, "dynamic-wind", result);
  define_procedure<with_exception_handler>(ctx, "with-exception-handler",
                                           result);
  define_procedure<raise_continuable>(ctx, "raise-continuable", result);
  define_procedure<raise>(ctx, "raise", result);
  define_raw_procedure<apply>(ctx, "apply", result);
  define_procedure<call_with_values>(ctx, "call-with-values", result);
  define_raw_procedure<values>(ctx, "values", result);
  define_procedure<eval_proc>(ctx, "eval", result);
}

} // namespace insider
