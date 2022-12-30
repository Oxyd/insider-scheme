#include "vm/vm.hpp"

#include "compiler/compiler.hpp"
#include "context.hpp"
#include "io/port.hpp"
#include "io/read.hpp"
#include "io/write.hpp"
#include "memory/free_store.hpp"
#include "memory/tracker.hpp"
#include "ptr.hpp"
#include "runtime/error.hpp"
#include "runtime/integer.hpp"
#include "runtime/numeric.hpp"
#include "runtime/parameter_map.hpp"
#include "runtime/syntax.hpp"
#include "util/define_procedure.hpp"
#include "util/from_scheme.hpp"
#include "util/integer_cast.hpp"
#include "util/to_scheme.hpp"
#include "vm/call_stack.hpp"
#include "vm/stacktrace.hpp"

#include <cassert>
#include <concepts>
#include <cstdint>
#include <limits>
#include <optional>
#include <stdexcept>
#include <type_traits>
#include <vector>

namespace insider {

vm::vm(context& ctx)
  : root_provider{ctx.store}
  , ctx{ctx}
{ }

void
vm::visit_roots(member_visitor const& f) {
  stack.visit_members(f);
  f(result);
}

static ptr<>
get_constant(call_stack& stack, operand index) {
  procedure_prototype const& proto
    = assume<procedure>(stack.callable())->prototype();
  assert(index < proto.constants_size);
  return proto.constants[index];
}

static bool
current_frame_is_native(call_stack& stack) {
  return stack.current_frame_type() != call_stack::frame_type::scheme;
}

static void
check_argument_count_for_procedure_without_tail(procedure_prototype const& proc,
                                                std::size_t num_args) {
  unsigned min = proc.info.num_required_args;
  unsigned max = proc.info.num_leading_args;
  bool is_variadic = min != max;

  if (num_args < min)
    throw make_error("{}: Wrong number of arguments, expected {}{}, got {}",
                     *proc.info.name,
                     is_variadic ? "at least " : "",
                     min,
                     num_args);
  else if (num_args > max)
    throw make_error("{}: Wrong number of arguments, expected {}{}, got {}",
                     *proc.info.name,
                     is_variadic ? "at most " : "",
                     max,
                     num_args);
}

static void
check_argument_count_for_procedure_with_tail(procedure_prototype const& proc,
                                             std::size_t num_args) {
  if (num_args < proc.info.num_required_args)
    throw make_error(
      "{}: Wrong number of arguments, expected at least {}, got {}",
      *proc.info.name,
      proc.info.num_required_args, num_args
    );
}

static void
throw_if_wrong_number_of_args(procedure_prototype const& proc,
                              std::size_t num_args) {
  if (proc.info.has_rest)
    check_argument_count_for_procedure_with_tail(proc, num_args);
  else
    check_argument_count_for_procedure_without_tail(proc, num_args);
}

static opcode
read_opcode(vm& state) {
  return insider::read_opcode(state.ip);
}

static operand
read_operand(vm& state) {
  return insider::read_operand(state.ip);
}

static void
load_dynamic_top_level(vm& state) {
  operand id_idx = read_operand(state);
  operand dest = read_operand(state);

  auto id = assume<syntax>(get_constant(state.stack, id_idx));
  assert(id->contains<symbol>());

  if (auto binding = lookup(id))
    if (binding->variable) {
      assert(is<top_level_variable>(binding->variable));

      state.stack.local(dest) = state.ctx.get_top_level(
        assume<top_level_variable>(binding->variable)->index
      );
      return;
    }

  throw unbound_variable_error{fmt::format(
    "Identifier {} not bound to variable", id->get_symbol()->value()
  )};
}
 
static register_index
actual_args_size(procedure_prototype const& proto) {
  return proto.info.num_leading_args + (proto.info.has_rest ? 1 : 0);
}

static void
push_closure(ptr<procedure> proc, call_stack& stack) {
  std::size_t closure_size = proc->size();
  std::size_t begin = actual_args_size(proc->prototype()) + 1;

  for (std::size_t i = 0; i < closure_size; ++i)
    stack.local(operand(begin + i)) = proc->ref(i);
}

static void
convert_tail_args_to_list(context& ctx, call_stack& stack,
                          register_index tail_base, std::size_t num_rest) {
  // Here's an interesting situation: Imagine that a Scheme procedure is calling
  // a variadic procedure with no arguments for the tail parameter. Furthermore,
  // the mandatory arguments are given as the very end of the caller's stack
  // frame. This means that there's no space for the tail argument in the
  // caller's frame.

  if (stack.frame_size() <= tail_base)
    stack.resize_current_frame(tail_base + 1);

  stack.local(operand(tail_base)) = make_list_from_range(
    ctx, std::views::iota(tail_base, tail_base + num_rest),
    [&] (std::size_t i) { return stack.local(operand(i)); }
  );
}

static std::size_t
tail_args_length(procedure_prototype const& proto, std::size_t num_args) {
  if (num_args >= proto.info.num_leading_args)
    return num_args - proto.info.num_leading_args;
  else
    return 0;
}

static void
convert_tail_args(context& ctx, call_stack& stack,
                  procedure_prototype const& proto, register_index base,
                  register_index num_args) {
  if (proto.info.has_rest)
    convert_tail_args_to_list(ctx, stack,
                              base + proto.info.num_leading_args + 1,
                              tail_args_length(proto, num_args));
}

static void
fill_in_default_values(context& ctx, call_stack& stack,
                       procedure_prototype const& proto, register_index base,
                       register_index num_args) {
  register_index begin = base + num_args + 1;
  register_index end = base + proto.info.num_leading_args + 1;

  if (begin < end) {
    if (stack.frame_size() <= end)
      stack.resize_current_frame(end + 1);

    for (std::size_t arg = begin; arg < end; ++arg)
      stack.local(operand(arg)) = ctx.constants->default_value;
  }
}

static instruction_pointer
current_procedure_bytecode_base(vm const& state) {
  return assume<procedure>(state.stack.callable())->prototype().code.get();
}

static void
push_scheme_frame(vm& state, ptr<procedure> proc,
                  operand base, operand result_reg) {
  state.stack.push_frame({
    .type = call_stack::frame_type::scheme,
    .result_register = result_reg,
    .base = state.stack.frame_base() + base,
    .size = proc->prototype().info.locals_size,
    .previous_ip = state.ip,
    .extra = {}
  });
}

static void
make_tail_call_frame(call_stack& stack, ptr<> proc,
                     register_index locals_size,
                     register_index args_base, register_index num_args) {
  stack.replace_frame(callable_to_frame_type(proc), locals_size, args_base,
                       num_args);
}

static void
make_scheme_tail_call_frame(call_stack& stack, ptr<procedure> proc,
                           register_index args_base, register_index num_args) {
  make_tail_call_frame(stack, proc, proc->prototype().info.locals_size,
                       args_base, num_args);
}

static void
check_and_convert_scheme_call_arguments(vm& state,
                                        procedure_prototype const& proto,
                                        operand base) {
  operand num_args = read_operand(state);
  throw_if_wrong_number_of_args(proto, num_args);
  fill_in_default_values(state.ctx, state.stack, proto, base, num_args);
  convert_tail_args(state.ctx, state.stack, proto, base, num_args);
}

template <bool IsTail, bool CheckAndConvertArgs>
static void
make_scheme_frame(ptr<procedure> proc, vm& state, operand base) {
  if constexpr (CheckAndConvertArgs)
    check_and_convert_scheme_call_arguments(state, proc->prototype(), base);
  else
    read_operand(state);

  if constexpr (!IsTail) {
    operand result_reg = read_operand(state);
    push_scheme_frame(state, proc, base, result_reg);
  } else
    make_scheme_tail_call_frame(state.stack, proc, base,
                                actual_args_size(proc->prototype()));
}

template <bool IsTail, bool CheckAndConvertArgs>
static void
push_scheme_call_frame(ptr<procedure> proc, vm& state,
                       operand base) {
  make_scheme_frame<IsTail, CheckAndConvertArgs>(proc, state, base);
  push_closure(proc, state.stack);
  state.ip = proc->prototype().code.get();
}

static void
make_native_non_tail_call_frame(vm& state, register_index base,
                                register_index num_args, operand dest_reg) {
  state.stack.push_frame({
    .type = call_stack::frame_type::native,
    .result_register = dest_reg,
    .base = state.stack.frame_base() + base,
    .size = num_args + 1,
    .previous_ip = state.ip,
    .extra = {}
  });
}

static void
make_native_tail_call_frame(call_stack& stack,
                            ptr<native_procedure> proc,
                            register_index args_base, register_index num_args) {
  make_tail_call_frame(stack, proc, num_args + 1, args_base, num_args);
}

static ptr<>
call_native_procedure(vm& state, call_stack& stack) {
  auto proc_and_args = stack.current_frame_span();
  auto proc = assume<native_procedure>(proc_and_args[0]);
  return proc->target(state, proc, proc_and_args.subspan<1>());
}

static ptr<>
call_native_continuation(vm& state, call_stack& stack, ptr<> value) {
  auto cont = assume<native_continuation>(stack.local(operand{0}));
  return cont->target(state, value);
}

static ptr<>
call_native_frame_target(vm& state, call_stack& stack, ptr<> scheme_result) {
  switch (stack.current_frame_type()) {
  case call_stack::frame_type::native:
    return call_native_procedure(state, stack);

  case call_stack::frame_type::native_continuation:
    return call_native_continuation(state, stack, scheme_result);

  default:
    assert(!"Bad frame type");
    std::abort();
  }
}

static void
pop_frame(vm& state) {
  instruction_pointer previous_ip = state.stack.previous_ip();

  state.stack.pop_frame();
  state.ip = previous_ip;
}

static void
resume_native_call(vm& state, ptr<> scheme_result);

static void
pop_frame_and_set_return_value(vm& state, ptr<> result) {
  assert(result);

  call_stack& stack = state.stack;
  operand dest_reg = stack.result_register();
  pop_frame(state);

  if (state.stack.empty())
    // We are returning from the global procedure, so we return back to the
    // calling C++ code.
    state.result = result;
  else if (current_frame_is_native(state.stack))
    resume_native_call(state, result);
  else
    state.stack.local(dest_reg) = result;
}

static void
call_native_procedure(vm& state, ptr<> scheme_result = {}) {
  tracker t{state.ctx, scheme_result};
  ptr<> result;
  do
    result = call_native_frame_target(state, state.stack, scheme_result);
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
make_native_frame(ptr<native_procedure> proc, vm& state,
                  operand base, bool is_tail) {
  auto num_args = read_operand(state);
  if (!is_tail) {
    auto dest_reg = read_operand(state);
    make_native_non_tail_call_frame(state, base, num_args, dest_reg);
  } else
    make_native_tail_call_frame(state.stack, proc, base, num_args);
}

static void
do_native_call(ptr<native_procedure> proc, vm& state,
               operand base, bool is_tail) {
  make_native_frame(proc, state, base, is_tail);
  call_native_procedure(state);
}

static void
throw_application_error(context& ctx, ptr<> callee) {
  throw make_error("Application: Not a procedure: {}",
                   datum_to_string(ctx, callee));
}

static void
resume_native_call(vm& state, ptr<> scheme_result) {
  if (state.stack.current_frame_type()
      == call_stack::frame_type::native_continuation)
    call_native_procedure(state, scheme_result);
  else
    // Return to a non-continuable native procedure. We'll abandon run()
    // immediately; the native procedure will then return back to a previous
    // run() call.
    state.result = scheme_result;
}

namespace {
template <auto Proc, typename Type>
  struct procedure_instruction_helper;

  template <auto Proc, typename Ret, typename Arg1>
  struct procedure_instruction_helper<Proc, Ret (*)(context&, Arg1)> {
    static void
    f(vm& state) {
      auto argument
        = from_scheme<Arg1>(state.ctx, state.stack.local(read_operand(state)));
      operand dest = read_operand(state);
      state.stack.local(dest) = to_scheme(state.ctx, Proc(state.ctx, argument));
    }
  };

  template <auto Proc, typename Ret, typename Arg1>
  struct procedure_instruction_helper<Proc, Ret (*)(Arg1)> {
    static void
    f(vm& state) {
      auto argument
        = from_scheme<Arg1>(state.ctx, state.stack.local(read_operand(state)));
      operand dest = read_operand(state);
      state.stack.local(dest) = to_scheme<Ret>(state.ctx, Proc(argument));
    }
  };

  template <auto Proc, typename Arg1, typename Arg2>
  struct procedure_instruction_helper<Proc, void (*)(context&, Arg1, Arg2)> {
    static void
    f(vm& state) {
      auto argument1
        = from_scheme<Arg1>(state.ctx, state.stack.local(read_operand(state)));
      auto argument2
        = from_scheme<Arg2>(state.ctx, state.stack.local(read_operand(state)));
      Proc(state.ctx, argument1, argument2);
    }
  };

  template <auto Proc, typename Arg1, typename Arg2, typename Arg3>
  struct procedure_instruction_helper<Proc,
                                      void (*)(context&, Arg1, Arg2, Arg3)> {
    static void
    f(vm& state) {
      auto argument1
        = from_scheme<Arg1>(state.ctx, state.stack.local(read_operand(state)));
      auto argument2
        = from_scheme<Arg2>(state.ctx, state.stack.local(read_operand(state)));
      auto argument3
        = from_scheme<Arg3>(state.ctx, state.stack.local(read_operand(state)));
      Proc(state.ctx, argument1, argument2, argument3);
    }
  };

  template <auto Proc, typename Arg1, typename Arg2, typename Arg3>
  struct procedure_instruction_helper<Proc,
                                      void (*)(Arg1, Arg2, Arg3)> {
    static void
    f(vm& state) {
      auto argument1
        = from_scheme<Arg1>(state.ctx, state.stack.local(read_operand(state)));
      auto argument2
        = from_scheme<Arg2>(state.ctx, state.stack.local(read_operand(state)));
      auto argument3
        = from_scheme<Arg3>(state.ctx, state.stack.local(read_operand(state)));
      Proc(argument1, argument2, argument3);
    }
  };

  template <auto Proc, typename Ret, typename Arg1, typename Arg2>
  struct procedure_instruction_helper<Proc, Ret (*)(context&, Arg1, Arg2)> {
    static void
    f(vm& state) {
      auto argument1
        = from_scheme<Arg1>(state.ctx, state.stack.local(read_operand(state)));
      auto argument2
        = from_scheme<Arg2>(state.ctx, state.stack.local(read_operand(state)));
      operand dest = read_operand(state);
      state.stack.local(dest)
        = to_scheme(state.ctx, Proc(state.ctx, argument1, argument2));
    }
  };

  template <auto Proc, typename Arg1, typename Arg2>
  struct procedure_instruction_helper<Proc, void (*)(Arg1, Arg2)> {
    static void
    f(vm& state) {
      auto argument1
        = from_scheme<Arg1>(state.ctx, state.stack.local(read_operand(state)));
      auto argument2
        = from_scheme<Arg2>(state.ctx, state.stack.local(read_operand(state)));
      Proc(argument1, argument2);
    }
  };

  template <auto Proc, typename Ret, typename Arg1, typename Arg2>
  struct procedure_instruction_helper<Proc, Ret (*)(Arg1, Arg2)> {
    static void
    f(vm& state) {
      auto argument1
        = from_scheme<Arg1>(state.ctx, state.stack.local(read_operand(state)));
      auto argument2
        = from_scheme<Arg2>(state.ctx, state.stack.local(read_operand(state)));
      operand dest = read_operand(state);
      state.stack.local(dest)
        = to_scheme(state.ctx, Proc(argument1, argument2));
    }
  };

  template <auto Proc, typename Ret, typename Cls>
  struct procedure_instruction_helper<Proc, Ret (Cls::*)() const> {
    static void
    f(vm& state) {
      auto obj = expect<Cls>(state.stack.local(read_operand(state)));
      operand dest = read_operand(state);
      state.stack.local(dest) = to_scheme(state.ctx, (obj->*Proc)());
    }
  };

  template <auto Proc, typename Ret, typename Cls, typename Arg1>
  struct procedure_instruction_helper<Proc, Ret (Cls::*)(Arg1) const> {
    static void
    f(vm& state) {
      auto obj = expect<Cls>(state.stack.local(read_operand(state)));
      auto argument1
        = from_scheme<Arg1>(state.ctx, state.stack.local(read_operand(state)));
      operand dest = read_operand(state);
      state.stack.local(dest)
        = to_scheme(state.ctx, (obj->*Proc)(argument1));
    }
  };

  template <auto Proc, typename Cls, typename Arg1>
  struct procedure_instruction_helper<Proc, void (Cls::*)(Arg1)> {
    static void
    f(vm& state) {
      auto obj = expect<Cls>(state.stack.local(read_operand(state)));
      auto argument1
        = from_scheme<Arg1>(state.ctx, state.stack.local(read_operand(state)));
      (obj->*Proc)(argument1);
    }
  };

template <auto Proc, typename Ret, typename Cls>
  struct procedure_instruction_helper<Proc, Ret (Cls::*)(context&)> {
    static void
    f(vm& state) {
      auto obj = expect<Cls>(state.stack.local(read_operand(state)));
      operand dest = read_operand(state);
      state.stack.local(dest) = to_scheme(state.ctx, (obj->*Proc)(state.ctx));
    }
  };

  template <auto Proc, typename Cls, typename Arg1, typename Arg2>
  struct procedure_instruction_helper<Proc, void (Cls::*)(Arg1, Arg2)> {
    static void
    f(vm& state) {
      auto obj = expect<Cls>(state.stack.local(read_operand(state)));
      auto argument1
        = from_scheme<Arg1>(state.ctx, state.stack.local(read_operand(state)));
      auto argument2
        = from_scheme<Arg2>(state.ctx, state.stack.local(read_operand(state)));
      (obj->*Proc)(argument1, argument2);
    }
  };
}

template <auto Proc>
static void
procedure_instruction(vm& state) {
  procedure_instruction_helper<Proc, std::decay_t<decltype(Proc)>>::f(state);
}

static void
car(vm& state) {
  ptr<pair> p = expect<pair>(state.stack.local(read_operand(state)));
  state.stack.local(read_operand(state)) = car(p);
}

static void
cdr(vm& state) {
  ptr<pair> p = expect<pair>(state.stack.local(read_operand(state)));
  state.stack.local(read_operand(state)) = cdr(p);
}

static ptr<tail_call_tag_type>
raise(vm&, ptr<> e);

static void
do_instructions(vm& state) {
  while (!state.result) {
    assert(!state.stack.empty());
    assert(is<procedure>(state.stack.callable()));

    opcode opcode = read_opcode(state);
    switch (opcode) {
    case opcode::no_operation:
      break;

    case opcode::load_constant: {
      operand const_num = read_operand(state);
      operand dest = read_operand(state);

      procedure_prototype const& proto
        = assume<procedure>(state.stack.callable())->prototype();
      assert(const_num < proto.constants_size);
      state.stack.local(dest) = proto.constants[const_num];
      break;
    }

    case opcode::load_top_level: {
      operand global_num = read_operand(state);
      operand dest = read_operand(state);
      state.stack.local(dest) = state.ctx.get_top_level(global_num);
      break;
    }

    case opcode::store_top_level: {
      operand reg = read_operand(state);
      operand global_num = read_operand(state);
      state.ctx.set_top_level(global_num, state.stack.local(reg));
      break;
    }

    case opcode::load_dynamic_top_level: {
      load_dynamic_top_level(state);
      break;
    }

    case opcode::load_null: {
      operand dest = read_operand(state);
      state.stack.local(dest) = state.ctx.constants->null;
      break;
    }

    case opcode::load_void: {
      operand dest = read_operand(state);
      state.stack.local(dest) = state.ctx.constants->void_;
      break;
    }

    case opcode::load_t: {
      operand dest = read_operand(state);
      state.stack.local(dest) = state.ctx.constants->t;
      break;
    }

    case opcode::load_f: {
      operand dest = read_operand(state);
      state.stack.local(dest) = state.ctx.constants->f;
      break;
    }

    case opcode::load_eof: {
      operand dest = read_operand(state);
      state.stack.local(dest) = state.ctx.constants->eof;
      break;
    }

    case opcode::load_default_value: {
      operand dest = read_operand(state);
      state.stack.local(dest) = state.ctx.constants->default_value;
      break;
    }

    case opcode::load_fixnum: {
      immediate_type value = operand_to_immediate(read_operand(state));
      operand dest = read_operand(state);
      state.stack.local(dest) = integer_to_ptr(value);
      break;
    }

    case opcode::load_character: {
      immediate_type value = operand_to_immediate(read_operand(state));
      operand dest = read_operand(state);
      state.stack.local(dest)
        = character_to_ptr(static_cast<char32_t>(value));
      break;
    }

    case opcode::add: {
      ptr<> lhs = state.stack.local(read_operand(state));
      ptr<> rhs = state.stack.local(read_operand(state));
      operand dest = read_operand(state);

      if (is<integer>(lhs) && is<integer>(rhs))
        if (ptr<> result = add_fixnums(assume<integer>(lhs).value(),
                                       assume<integer>(rhs).value())) {
          state.stack.local(dest) = result;
          break;
        }

      state.stack.local(dest) = add(state.ctx, lhs, rhs);
      break;
    }

    case opcode::subtract: {
      ptr<> lhs = state.stack.local(read_operand(state));
      ptr<> rhs = state.stack.local(read_operand(state));
      operand dest = read_operand(state);

      if (is<integer>(lhs) && is<integer>(rhs))
        if (ptr<> result = subtract_fixnums(assume<integer>(lhs).value(),
                                            assume<integer>(rhs).value())) {
          state.stack.local(dest) = result;
          break;
        }

      state.stack.local(dest) = subtract(state.ctx, lhs, rhs);
      break;
    }

    case opcode::multiply: {
      ptr<> lhs = state.stack.local(read_operand(state));
      ptr<> rhs = state.stack.local(read_operand(state));
      operand dest = read_operand(state);

      if (is<integer>(lhs) && is<integer>(rhs))
        if (ptr<> result = multiply_fixnums(assume<integer>(lhs).value(),
                                            assume<integer>(rhs).value())) {
          state.stack.local(dest) = result;
          break;
        }

      state.stack.local(dest) = multiply(state.ctx, lhs, rhs);
      break;
    }

    case opcode::divide: {
      ptr<> lhs = state.stack.local(read_operand(state));
      ptr<> rhs = state.stack.local(read_operand(state));
      operand dest = read_operand(state);
      state.stack.local(dest) = divide(state.ctx, lhs, rhs);
      break;
    }

    case opcode::truncate_quotient: {
      ptr<> lhs = state.stack.local(read_operand(state));
      ptr<> rhs = state.stack.local(read_operand(state));
      operand dest = read_operand(state);
      if (is<integer>(lhs) && is<integer>(rhs))
        state.stack.local(dest) = integer_to_ptr(
          assume<integer>(lhs).value() / assume<integer>(rhs).value()
        );
      else
        state.stack.local(dest) = truncate_quotient(state.ctx, lhs, rhs);
      break;
    }

    case opcode::truncate_remainder: {
      ptr<> lhs = state.stack.local(read_operand(state));
      ptr<> rhs = state.stack.local(read_operand(state));
      operand dest = read_operand(state);
      if (is<integer>(lhs) && is<integer>(rhs))
        state.stack.local(dest) = integer_to_ptr(
          assume<integer>(lhs).value() % assume<integer>(rhs).value()
        );
      else
        state.stack.local(dest) = truncate_quotient(state.ctx, lhs, rhs);
      break;
    }

    case opcode::increment: {
      ptr<> value = state.stack.local(read_operand(state));
      operand dest = read_operand(state);

      if (auto i = match<integer>(value))
        if (auto result = add_fixnums(i->value(), 1)) {
          state.stack.local(dest) = result;
          break;
        }

      state.stack.local(dest) = add(state.ctx, value, integer_to_ptr(1));
      break;
    }

    case opcode::decrement: {
      ptr<> value = state.stack.local(read_operand(state));
      operand dest = read_operand(state);

      if (auto i = match<integer>(value))
        if (auto result = subtract_fixnums(i->value(), 1)) {
          state.stack.local(dest) = result;
          break;
        }

      state.stack.local(dest)
        = subtract(state.ctx, value, integer_to_ptr(1));
      break;
    }

    case opcode::negate: {
      ptr<> value = state.stack.local(read_operand(state));
      operand dest = read_operand(state);

      if (auto i = match<integer>(value))
        if (auto result = multiply_fixnums(i->value(), -1)) {
          state.stack.local(dest) = result;
          break;
        }

      state.stack.local(dest) = negate(state.ctx, value);
      break;
    }

    case opcode::arith_equal: {
      ptr<> lhs = state.stack.local(read_operand(state));
      ptr<> rhs = state.stack.local(read_operand(state));
      operand dest = read_operand(state);

      ptr<> t = state.ctx.constants->t;
      ptr<> f = state.ctx.constants->f;

      if (is<integer>(lhs) && is<integer>(rhs))
        state.stack.local(dest)
          = assume<integer>(lhs).value() == assume<integer>(rhs).value()
          ? t : f;
      else
        state.stack.local(dest) = arith_equal(state.ctx, lhs, rhs);
      break;
    }

    case opcode::less: {
      ptr<> lhs = state.stack.local(read_operand(state));
      ptr<> rhs = state.stack.local(read_operand(state));
      operand dest = read_operand(state);

      ptr<> t = state.ctx.constants->t;
      ptr<> f = state.ctx.constants->f;

      if (is<integer>(lhs) && is<integer>(rhs))
        state.stack.local(dest)
          = assume<integer>(lhs).value() < assume<integer>(rhs).value()
          ? t : f;
      else
        state.stack.local(dest) = less(state.ctx, lhs, rhs);
      break;
    }

    case opcode::greater: {
      ptr<> lhs = state.stack.local(read_operand(state));
      ptr<> rhs = state.stack.local(read_operand(state));
      operand dest = read_operand(state);

      ptr<> t = state.ctx.constants->t;
      ptr<> f = state.ctx.constants->f;

      if (is<integer>(lhs) && is<integer>(rhs))
        state.stack.local(dest)
          = assume<integer>(lhs).value() > assume<integer>(rhs).value()
          ? t : f;
      else
        state.stack.local(dest) = greater(state.ctx, lhs, rhs);
      break;
    }

    case opcode::less_or_equal: {
      ptr<> lhs = state.stack.local(read_operand(state));
      ptr<> rhs = state.stack.local(read_operand(state));
      operand dest = read_operand(state);

      ptr<> t = state.ctx.constants->t;
      ptr<> f = state.ctx.constants->f;

      if (is<integer>(lhs) && is<integer>(rhs))
        state.stack.local(dest)
          = assume<integer>(lhs).value() <= assume<integer>(rhs).value()
          ? t : f;
      else
        state.stack.local(dest) = less_or_equal(state.ctx, lhs, rhs);
      break;
    }

    case opcode::greater_or_equal: {
      ptr<> lhs = state.stack.local(read_operand(state));
      ptr<> rhs = state.stack.local(read_operand(state));
      operand dest = read_operand(state);

      ptr<> t = state.ctx.constants->t;
      ptr<> f = state.ctx.constants->f;

      if (is<integer>(lhs) && is<integer>(rhs))
        state.stack.local(dest)
          = assume<integer>(lhs).value() >= assume<integer>(rhs).value()
          ? t : f;
      else
        state.stack.local(dest) = greater_or_equal(state.ctx, lhs, rhs);
      break;
    }

    case opcode::char_eq: {
      auto lhs = expect<char32_t>(state.stack.local(read_operand(state)));
      auto rhs = expect<char32_t>(state.stack.local(read_operand(state)));
      operand dest = read_operand(state);
      bool result = lhs == rhs;
      state.stack.local(dest)
        = result ? state.ctx.constants->t : state.ctx.constants->f;
      break;
    }

    case opcode::char_lt: {
      auto lhs = expect<char32_t>(state.stack.local(read_operand(state)));
      auto rhs = expect<char32_t>(state.stack.local(read_operand(state)));
      operand dest = read_operand(state);
      bool result = lhs < rhs;
      state.stack.local(dest)
        = result ? state.ctx.constants->t : state.ctx.constants->f;
      break;
    }

    case opcode::char_le: {
      auto lhs = expect<char32_t>(state.stack.local(read_operand(state)));
      auto rhs = expect<char32_t>(state.stack.local(read_operand(state)));
      operand dest = read_operand(state);
      bool result = lhs <= rhs;
      state.stack.local(dest)
        = result ? state.ctx.constants->t : state.ctx.constants->f;
      break;
    }

    case opcode::char_gt: {
      auto lhs = expect<char32_t>(state.stack.local(read_operand(state)));
      auto rhs = expect<char32_t>(state.stack.local(read_operand(state)));
      operand dest = read_operand(state);
      bool result = lhs > rhs;
      state.stack.local(dest)
        = result ? state.ctx.constants->t : state.ctx.constants->f;
      break;
    }

    case opcode::char_ge: {
      auto lhs = expect<char32_t>(state.stack.local(read_operand(state)));
      auto rhs = expect<char32_t>(state.stack.local(read_operand(state)));
      operand dest = read_operand(state);
      bool result = lhs >= rhs;
      state.stack.local(dest)
        = result ? state.ctx.constants->t : state.ctx.constants->f;
      break;
    }

    case opcode::set: {
      operand src = read_operand(state);
      operand dst = read_operand(state);
      state.stack.local(dst) = state.stack.local(src);
      break;
    }

    case opcode::call: {
      operand base = read_operand(state);
      ptr<> callee = state.stack.local(base);

      if (auto proc = match<procedure>(callee))
        push_scheme_call_frame<false, true>(proc, state, base);
      else if (auto native = match<native_procedure>(callee))
        do_native_call(native, state, base, false);
      else
        throw_application_error(state.ctx, callee);
      break;
    }

    case opcode::tail_call: {
      operand base = read_operand(state);
      ptr<> callee = state.stack.local(base);

      if (auto proc = match<procedure>(callee))
        push_scheme_call_frame<true, true>(proc, state, base);
      else if (auto native = match<native_procedure>(callee))
        do_native_call(native, state, base, true);
      else
        throw_application_error(state.ctx, callee);
      break;
    }

    case opcode::call_known_scheme: {
      operand base = read_operand(state);
      auto callee = assume<procedure>(state.stack.local(base));
      read_operand(state);
      operand result_reg = read_operand(state);

      state.stack.push_frame({
        .type = call_stack::frame_type::scheme,
        .result_register = result_reg,
        .base = state.stack.frame_base() + base,
        .size = callee->prototype().info.locals_size,
        .previous_ip = state.ip,
        .extra = {}
      });

      std::size_t closure_size = callee->size();
      std::size_t begin = actual_args_size(callee->prototype()) + 1;

      for (std::size_t i = 0; i < closure_size; ++i)
        state.stack.local(operand(begin + i)) = callee->ref(i);

      state.ip = callee->prototype().code.get();
      break;
    }

    case opcode::tail_call_known_scheme: {
      operand base = read_operand(state);
      auto callee = assume<procedure>(state.stack.local(base));
      read_operand(state);

      state.stack.replace_frame(call_stack::frame_type::scheme,
                                callee->prototype().info.locals_size,
                                base,
                                actual_args_size(callee->prototype()));

      std::size_t closure_size = callee->size();
      std::size_t begin = actual_args_size(callee->prototype()) + 1;

      for (std::size_t i = 0; i < closure_size; ++i)
        state.stack.local(operand(begin + i)) = callee->ref(i);

      state.ip = callee->prototype().code.get();
      break;
    }

    case opcode::call_known_native: {
      operand base = read_operand(state);
      auto callee = assume<native_procedure>(state.stack.local(base));
      do_native_call(callee, state, base, false);
      break;
    }

    case opcode::tail_call_known_native: {
      operand base = read_operand(state);
      auto callee = assume<native_procedure>(state.stack.local(base));
      do_native_call(callee, state, base, true);
      break;
    }

    case opcode::ret: {
      ptr<> result = state.stack.local(read_operand(state));
      assert(result);

      call_stack& stack = state.stack;
      operand dest_reg = stack.result_register();

      instruction_pointer previous_ip = stack.previous_ip();
      stack.pop_frame();
      state.ip = previous_ip;

      if (state.stack.empty())
        // We are returning from the global procedure, so we return back to
        // the calling C++ code.
        state.result = result;
      else if (current_frame_is_native(state.stack))
        resume_native_call(state, result);
      else
        state.stack.local(dest_reg) = result;

      break;
    }

    case opcode::jump: {
      immediate_type offset = operand_to_immediate(read_operand(state));
      state.ip += offset;
      break;
    }

    case opcode::jump_unless: {
      ptr<> test_value = state.stack.local(read_operand(state));
      immediate_type offset = operand_to_immediate(read_operand(state));
      if (test_value == state.ctx.constants->f)
        state.ip += offset;
      break;
    }

    case opcode::make_closure: {
      operand base = read_operand(state);
      ptr<procedure_prototype> proto
        = assume<procedure_prototype>(state.stack.local(base));

      auto num_captures = read_operand(state);
      operand captures_base = base + 1;
      operand dest = read_operand(state);

      auto result = make<procedure>(state.ctx, proto, num_captures);
      for (std::size_t i = 0; i < num_captures; ++i)
        result->set(state.ctx.store, i,
                    state.stack.local(operand(captures_base + i)));

      state.stack.local(dest) = result;
      break;
    }

    case opcode::type: {
      ptr<> o = state.stack.local(read_operand(state));
      state.stack.local(read_operand(state)) = type(state.ctx, o);
      break;
    }

    case opcode::box:
      procedure_instruction<make_box>(state);
      break;

    case opcode::unbox:
      procedure_instruction<unbox>(state);
      break;

    case opcode::box_set:
      procedure_instruction<box_set>(state);
      break;

    case opcode::cons:
      procedure_instruction<cons>(state);
      break;

    case opcode::car:
      car(state);
      break;

    case opcode::cdr:
      cdr(state);
      break;

    case opcode::vector_set:
      procedure_instruction<vector_set>(state);
      break;

    case opcode::vector_ref:
      procedure_instruction<&vector::ref>(state);
      break;

    case opcode::vector_length:
      procedure_instruction<&vector::size>(state);
      break;

    case opcode::bytevector_u8_set:
      procedure_instruction<&bytevector::set>(state);
      break;

    case opcode::bytevector_u8_ref:
      procedure_instruction<&bytevector::ref>(state);
      break;

    case opcode::bytevector_length:
      procedure_instruction<&bytevector::size>(state);
      break;

    case opcode::string_ref:
      procedure_instruction<string_ref>(state);
      break;

    case opcode::string_set:
      procedure_instruction<string_set>(state);
      break;

    case opcode::string_length:
      procedure_instruction<&string::length>(state);
      break;

    case opcode::string_append_char:
      procedure_instruction<&string::append_char>(state);
      break;

    case opcode::string_null:
      procedure_instruction<is_string_null>(state);
      break;

    case opcode::string_cursor_start:
      procedure_instruction<string_cursor_start>(state);
      break;

    case opcode::string_cursor_end:
      procedure_instruction<string_cursor_end>(state);
      break;

    case opcode::string_cursor_next:
      procedure_instruction<string_cursor_next>(state);
      break;

    case opcode::string_cursor_prev:
      procedure_instruction<string_cursor_prev>(state);
      break;

    case opcode::eq:
      procedure_instruction<eq>(state);
      break;

    case opcode::eqv:
      procedure_instruction<eqv>(state);
      break;

    case opcode::equal:
      procedure_instruction<equal>(state);
      break;

    case opcode::syntax_expression:
      procedure_instruction<&syntax::update_and_get_expression>(state);
      break;

    case opcode::free_identifier_eq:
      procedure_instruction<free_identifier_eq>(state);
      break;

    case opcode::is_integer:
      procedure_instruction<is_integer>(state);
      break;

    case opcode::is_exact_integer:
      procedure_instruction<is_exact_integer>(state);
      break;

    case opcode::is_zero:
      procedure_instruction<is_zero>(state);
      break;

    case opcode::is_positive:
      procedure_instruction<is_positive>(state);
      break;

    case opcode::is_negative:
      procedure_instruction<is_negative>(state);
      break;

    case opcode::is_number:
      procedure_instruction<is_number>(state);
      break;

    case opcode::is_real:
      procedure_instruction<is_real>(state);
      break;

    case opcode::is_rational:
      procedure_instruction<is_rational>(state);
      break;

    case opcode::is_finite:
      procedure_instruction<is_finite>(state);
      break;

    case opcode::is_infinite:
      procedure_instruction<is_infinite>(state);
      break;

    case opcode::is_nan:
      procedure_instruction<is_nan>(state);
      break;

    case opcode::is_inexact:
      procedure_instruction<is_inexact>(state);
      break;

    case opcode::is_exact:
      procedure_instruction<is_exact>(state);
      break;

    case opcode::inexact:
      procedure_instruction<inexact>(state);
      break;

    case opcode::exact:
      procedure_instruction<exact>(state);
      break;

    case opcode::fraction_numerator:
      procedure_instruction<&fraction::numerator>(state);
      break;

    case opcode::fraction_denominator:
      procedure_instruction<&fraction::denominator>(state);
      break;

    case opcode::real_part:
      procedure_instruction<real_part>(state);
      break;

    case opcode::imag_part:
      procedure_instruction<imag_part>(state);
      break;

    case opcode::read_char:
      procedure_instruction<read_char>(state);
      break;

    case opcode::peek_char:
      procedure_instruction<peek_char>(state);
      break;

    case opcode::write_char:
      procedure_instruction<write_char>(state);
      break;

    case opcode::read_u8:
      procedure_instruction<read_u8>(state);
      break;

    case opcode::peek_u8:
      procedure_instruction<peek_u8>(state);
      break;

    case opcode::write_u8:
      procedure_instruction<write_u8>(state);
      break;

    case opcode::is_default_value:
      procedure_instruction<is_default_value>(state);
      break;

    default:
      assert(false); // Invalid opcode
    } // end switch

    state.ctx.store.update();
  }
}

static void
do_instructions_translate_exceptions(vm& state) {
  while (!state.result)
    try {
      do_instructions(state);
    } catch (ptr<> e) {
      raise(state, e);
    } catch (tracked_ptr<> const& e) {
      raise(state, e.get());
    } catch (scheme_exception& e) {
      throw e;
    } catch (translatable_runtime_error& e) {
      raise(state, e.translate(state.ctx));
    } catch (...) {
      raise(state, make<cxx_exception>(state.ctx, std::current_exception()));
    }
}

static ptr<>
run(vm& state) {
  assert(!state.result);

  do_instructions_translate_exceptions(state);

  ptr<> result = state.result;
  state.result = {};
  return result;
}

static void
push_rest_argument_for_call_from_native(context& ctx,
                                        procedure_prototype const& proto,
                                        call_stack& stack,
                                        auto tail_begin,
                                        auto tail_end) {
  stack.local(operand(proto.info.num_leading_args + 1))
    = make_list_from_range(ctx,
                           std::ranges::subrange(tail_begin, tail_end));
}

static void
push_scheme_arguments_for_call_from_native(context& ctx,
                                           ptr<procedure> callable,
                                           call_stack& stack,
                                           auto const& args) {
  procedure_prototype const& proto = callable->prototype();

  stack.local(operand{0}) = callable;
  auto arg_it = args.begin();
  register_index arg = 0;
  while (arg < proto.info.num_leading_args && arg_it != args.end())
    stack.local(operand(arg++) + 1) = *arg_it++;

  fill_in_default_values(ctx, stack, proto, 0, arg);

  if (proto.info.has_rest)
    push_rest_argument_for_call_from_native(ctx, proto, stack,
                                            arg_it, args.end());
}

static void
make_scheme_frame_for_call_from_native(vm& state,
                                       ptr<procedure> callable,
                                       auto const& arguments) {
  throw_if_wrong_number_of_args(callable->prototype(), arguments.size());

  state.stack.push_frame({
    .type = call_stack::frame_type::scheme,
    .result_register = {},
    .base = state.stack.size(),
    .size = callable->prototype().info.locals_size,
    .previous_ip = nullptr,
    .extra = {}
  });

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
create_or_get_extra_data(context& ctx, call_stack& stack) {
  if (auto e = stack.extra())
    return e;
  else {
    stack.set_extra(make<stack_frame_extra_data>(ctx));
    return stack.extra();
  }
}

static void
erect_barrier(context& ctx, call_stack& stack,
              bool allow_out, bool allow_in) {
  ptr<stack_frame_extra_data> extra = create_or_get_extra_data(ctx, stack);
  extra->allow_jump_out = allow_out;
  extra->allow_jump_in = allow_in;
}

namespace {
  class native_frame_guard {
  public:
    native_frame_guard(call_stack& stack)
      : stack_{&stack}
    { }

    ~native_frame_guard() {
      if (stack_)
        stack_->pop_frame();
    }

    native_frame_guard(native_frame_guard const&) = delete;

    native_frame_guard(native_frame_guard&& other) noexcept
      : stack_{other.stack_}
    {
      other.stack_ = nullptr;
    }

    void
    operator = (native_frame_guard const&) = delete;

  private:
    call_stack* stack_;
  };
}

[[nodiscard]] static native_frame_guard
setup_native_frame_for_call_from_native(vm& state,
                                        ptr<native_procedure> proc) {
  state.stack.push_frame({
    .type = call_stack::frame_type::native,
    .result_register = {},
    .base = state.stack.size(),
    .size = 1,
    .previous_ip = state.ip,
    .extra = {}
  });
  state.stack.local(0) = proc;
  return {state.stack};
}

static ptr<>
call_native_in_current_frame(vm& state, ptr<native_procedure> proc,
                             std::vector<ptr<>> const& arguments) {
  ptr<> result = proc->target(state, proc,
                              object_span(arguments.begin(), arguments.end()));
  return result;
}

static void
add_parameter_value(context& ctx, call_stack& stack, ptr<parameter_tag> tag,
                    ptr<> value);

static ptr<>
call_native(vm& state,
            ptr<native_procedure> proc,
            std::vector<ptr<>> const& arguments,
            parameter_assignments const& params) {
  native_frame_guard guard
    = setup_native_frame_for_call_from_native(state, proc);

  for (auto const& p : params)
    add_parameter_value(state.ctx, state.stack, p.tag, p.value);

  return call_native_in_current_frame(state, proc, arguments);
}

static ptr<>
call_scheme(vm& state,
            ptr<procedure> callable,
            std::vector<ptr<>> const& arguments,
            parameter_assignments const& params) {
  make_scheme_frame_for_call_from_native(state, callable, arguments);

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
        ctx.current_execution = std::make_unique<vm>(ctx);
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

[[nodiscard]]
static native_frame_guard
push_dummy_frame(call_stack& stack) {
  stack.push_frame({
    .type = call_stack::frame_type::dummy,
    .result_register = {},
    .base = stack.size(),
    .size = 1,
    .previous_ip = nullptr,
    .extra = {}
  });
  stack.callable() = nullptr;
  return {stack};
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
  auto& stack = ctx.current_execution->stack;
  auto guard = push_dummy_frame(stack);

  if (!allow_jump_out || !allow_jump_in)
    erect_barrier(ctx, stack, allow_jump_out, allow_jump_in);

  if (auto native_proc = match<native_procedure>(callable))
    return call_native(
      *ctx.current_execution, native_proc, arguments, params
    );
  else
    return call_scheme(
      *ctx.current_execution, assume<procedure>(callable), arguments, params
    );
}

ptr<>
call_with_continuation_barrier(context& ctx, ptr<> callable,
                               std::vector<ptr<>> const& arguments) {
  return call_parameterized_with_continuation_barrier(ctx, {}, callable,
                                                      arguments);
}

static void
make_native_frame_for_call_from_native(vm& state,
                                       ptr<native_procedure> proc,
                                       std::vector<ptr<>> const& arguments) {
  state.stack.push_frame({
    .type = call_stack::frame_type::native,
    .result_register = {},
    .base = state.stack.size(),
    .size = static_cast<register_index>(arguments.size() + 1),
    .previous_ip = nullptr,
    .extra = {}
  });
  state.stack.local(operand{0}) = proc;
}

static void
make_native_continuation_frame(call_stack& stack,
                               ptr<native_continuation> cont) {
  stack.replace_frame(call_stack::frame_type::native_continuation, 1);
  stack.local(operand{0}) = cont;
}

ptr<tail_call_tag_type>
call_continuable(context& ctx, ptr<> callable,
                 std::vector<ptr<>> const& arguments,
                 native_continuation::target_type cont) {
  expect_callable(callable);
  current_execution_setter ces{ctx};

  auto& stack = ctx.current_execution->stack;
  make_native_continuation_frame(
    stack,
    make<native_continuation>(ctx, std::move(cont), stack.callable())
  );

  if (auto scheme_proc = match<procedure>(callable))
    make_scheme_frame_for_call_from_native(*ctx.current_execution, scheme_proc,
                                           arguments);
  else
    make_native_frame_for_call_from_native(*ctx.current_execution,
                                           assume<native_procedure>(callable),
                                           arguments);
  return ctx.constants->tail_call_tag;
}

static void
make_native_tail_call_frame_for_call_from_native(
  call_stack& stack,
  ptr<native_procedure> callable,
  auto const& arguments
) {
  stack.replace_frame(call_stack::frame_type::native,
                      to_smaller<register_index>(arguments.size() + 1));
  stack.local(operand{0}) = callable;
  auto arg = arguments.begin();
  for (std::size_t i = 0; i < arguments.size(); ++i)
    stack.local(operand(i + 1)) = *arg++;
}

static void
make_scheme_tail_call_frame_for_call_from_native(
  context& ctx,
  call_stack& stack,
  ptr<procedure> callable,
  auto const& arguments
) {
  throw_if_wrong_number_of_args(callable->prototype(), arguments.size());

  stack.replace_frame(call_stack::frame_type::scheme,
                       callable->prototype().info.locals_size);
  push_scheme_arguments_for_call_from_native(ctx, callable, stack, arguments);
  push_closure(callable, stack);
}

static void
make_tail_call_frame_for_call_from_native(vm& state, ptr<> callable,
                                          auto const& arguments) {
  if (!is_callable(callable))
    throw std::runtime_error{"Expected a callable"};

  call_stack& stack = state.stack;

  if (auto native = match<native_procedure>(callable))
    make_native_tail_call_frame_for_call_from_native(stack, native, arguments);
  else
    make_scheme_tail_call_frame_for_call_from_native(state.ctx, stack,
                                                     assume<procedure>(callable),
                                                     arguments);
}

static void
install_call_frame(vm& state) {
  if (!current_frame_is_native(state.stack))
    state.ip = current_procedure_bytecode_base(state);
}

static ptr<tail_call_tag_type>
tail_call_range(vm& state, ptr<> callable, auto const& arguments) {
  make_tail_call_frame_for_call_from_native(state, callable, arguments);
  install_call_frame(state);
  return state.ctx.constants->tail_call_tag;
}

ptr<tail_call_tag_type>
tail_call(vm& state, ptr<> callable, std::vector<ptr<>> const& arguments) {
  return tail_call_range(state, callable, arguments);
}

static ptr<tail_call_tag_type>
capture_stack(vm& state, ptr<> receiver) {
  auto copy = make<call_stack>(state.ctx, state.stack);
  return tail_call(state, receiver, {copy});
}

static bool
all_frames(call_stack& stack,
           std::optional<call_stack::frame_index> begin,
           std::optional<call_stack::frame_index> end,
           auto pred) {
  while (begin != end)
    if (!pred(frame_reference{stack, *begin}))
      return false;
    else
      begin = stack.parent(*begin);

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
jump_out_allowed(call_stack& stack,
                 std::optional<call_stack::frame_index> common) {
  return all_frames(stack, stack.current_frame_index(), common,
                    allows_jump_out);
}

static bool
jump_in_allowed(call_stack& stack,
                std::optional<call_stack::frame_index> common) {
  return all_frames(stack, stack.current_frame_index(), common,
                    allows_jump_in);
}

static bool
continuation_jump_allowed(call_stack& current_stack,
                          call_stack& new_stack,
                          std::optional<call_stack::frame_index> common) {
  return jump_out_allowed(current_stack, common)
         && jump_in_allowed(new_stack, common);
}

static void
throw_if_jump_not_allowed(vm& state,
                          call_stack& new_stack,
                          std::optional<call_stack::frame_index> common) {
  if (!continuation_jump_allowed(state.stack, new_stack, common))
    throw std::runtime_error{"Continuation jump across barrier not allowed"};
}

static ptr<>
get_after_thunk(call_stack& stack) {
  if (auto e = stack.extra())
    return e->after_thunk;
  else
    return {};
}

static ptr<>
get_before_thunk(call_stack& stack) {
  if (auto e = stack.extra())
    return e->before_thunk;
  else
    return {};
}

static void
unwind_stack(vm& state,
             std::optional<call_stack::frame_index> end) {
  while (state.stack.current_frame_index() != end) {
    if (ptr<> thunk = get_after_thunk(state.stack))
      call_with_continuation_barrier(state.ctx, thunk, {});

    state.stack.pop_frame();
  }
}

static void
rewind_stack(vm& state, ptr<call_stack> cont,
             std::optional<call_stack::frame_index> common_frame) {
  call_stack::frame_index begin = common_frame ? *common_frame + 1 : 0;
  for (std::size_t i = begin; i < cont->frame_count(); ++i) {
    state.stack.append_frame(cont, i);
    if (ptr<> thunk = get_before_thunk(state.stack))
      call_with_continuation_barrier(state.ctx, thunk, {});
  }
}

static bool
frames_same(call_stack& x, call_stack& y, call_stack::frame_index i) {
  return x.callable(i) == y.callable(i)
         && x.previous_ip(i) == y.previous_ip(i);
}

static std::optional<call_stack::frame_index>
common_frame_index(call_stack& x, call_stack& y) {
  std::optional<call_stack::frame_index> result;
  std::size_t max_index = std::min(x.frame_count(), y.frame_count());

  for (call_stack::frame_index i = 0; i < max_index; ++i)
    if (frames_same(x, y, i))
      result = i;

  return result;
}

static ptr<>
replace_stack(vm& state, ptr<call_stack> cont, ptr<> value) {
  std::optional<call_stack::frame_index> common_frame_idx
    = common_frame_index(state.stack, *cont);
  throw_if_jump_not_allowed(state, *cont, common_frame_idx);

  unwind_stack(state, common_frame_idx);
  rewind_stack(state, cont, common_frame_idx);

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
                          [] (vm&, ptr<> result) { return result; });
}

ptr<parameter_tag>
create_parameter_tag(context& ctx, ptr<> initial_value) {
  auto tag = make<parameter_tag>(ctx);
  ctx.parameters->add_value(ctx.store, tag, initial_value);
  return tag;
}

static ptr<>
find_parameter_in_frame(call_stack& stack, std::size_t frame_idx,
                        ptr<parameter_tag> tag) {
  if (ptr<stack_frame_extra_data> e = stack.extra(frame_idx))
    for (auto const& p : e->parameters)
      if (p.tag == tag)
        return p.value;

  return {};
}

static std::optional<std::size_t>
find_stack_frame_for_parameter(call_stack& stack, ptr<parameter_tag> tag) {
  auto idx = stack.current_frame_index();
  while (idx)
    if (find_parameter_in_frame(stack, *idx, tag))
      return idx;
    else
      idx = stack.parent(*idx);
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
set_parameter_value_in_frame(call_stack& stack, std::size_t frame,
                             ptr<parameter_tag> tag, ptr<> value) {
  auto extra = stack.extra(frame);
  set_parameter_value_in_frame(extra, tag, value);
}

static void
set_parameter_value(vm& state, ptr<parameter_tag> tag, ptr<> value) {
  auto& stack = state.stack;
  auto frame = find_stack_frame_for_parameter(stack, tag);
  if (frame) 
    set_parameter_value_in_frame(stack, *frame, tag, value);
  else
    state.ctx.parameters->set_value(state.ctx.store, tag, value);
}

static ptr<>
find_parameter_value_in_stack(vm& state, ptr<parameter_tag> tag) {
  auto& stack = state.ctx.current_execution->stack;
  auto frame = stack.current_frame_index();

  while (frame)
    if (auto value = find_parameter_in_frame(stack, *frame, tag))
      return value;
    else
      frame = stack.parent(*frame);

  return {};
}

ptr<>
find_parameter_value(vm& state, ptr<parameter_tag> tag) {
  if (auto value = find_parameter_value_in_stack(state, tag))
    return value;
  else
    return state.ctx.parameters->find_value(tag);
}

static void
add_parameter_value(context& ctx, call_stack& stack, ptr<parameter_tag> tag,
                    ptr<> value) {
  ptr<stack_frame_extra_data> extra = create_or_get_extra_data(ctx, stack);
  set_parameter_value_in_frame(extra, tag, value);
}

static ptr<tail_call_tag_type>
call_parameterized(vm& state, ptr<parameter_tag> tag, ptr<> value,
                   ptr<> callable) {
  add_parameter_value(state.ctx, state.stack, tag, value);
  return call_continuable(state.ctx, callable, {},
                          [] (vm&, ptr<> result) { return result; });
}

static ptr<>
dynamic_wind(vm& state,
             tracked_ptr<> const& before,
             tracked_ptr<> const& thunk,
             tracked_ptr<> const& after) {
  ptr<stack_frame_extra_data> e
    = create_or_get_extra_data(state.ctx, state.stack);
  e->before_thunk = before.get();
  e->after_thunk = after.get();

  return call_continuable(
    state.ctx, before.get(), {},
    [=] (vm& state, ptr<>) {
      return call_continuable(
        state.ctx, thunk.get(), {},
        [=] (vm& state, ptr<> result) {
          return call_continuable(
            state.ctx, after.get(), {},
            [result = track(state.ctx, result)] (vm&, ptr<>) {
              return result.get();
            }
          );
        }
      );
    }
  );
}

static frame_reference
find_exception_handler_frame(call_stack& stack, frame_reference frame) {
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
with_exception_handler(vm& state, ptr<> handler, ptr<> thunk) {
  call_continuable(state.ctx, thunk, {},
                   [] (vm&, ptr<> result) { return result; });

  ptr<stack_frame_extra_data> extra
    = create_or_get_extra_data(state.ctx, state.stack);
  extra->exception_handler = handler;

  return state.ctx.constants->tail_call_tag;
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
call_continuable_exception_handler(vm& state, frame_reference handler_frame,
                                   ptr<> exception) {
  call_continuable(state.ctx, get_frame_exception_handler(handler_frame), {exception},
                   [] (vm&, ptr<> result) { return result; });
  setup_exception_handler_frame(state.ctx, current_frame(state.stack),
                                handler_frame);
  return state.ctx.constants->tail_call_tag;
}

static ptr<tail_call_tag_type>
raise_from(vm& state, ptr<> e, frame_reference frame);

static ptr<tail_call_tag_type>
call_noncontinuable_exception_handler(vm& state,
                                      frame_reference handler_frame,
                                      ptr<> exception) {
  call_continuable(
    state.ctx, get_frame_exception_handler(handler_frame), {exception},
    [exception = track(state.ctx, exception),
     handler_frame_idx = handler_frame.index()] (vm& state, ptr<>) {
      return raise_from(state,
                        make<uncaught_exception>(state.ctx, exception.get()),
                        frame_reference{state.stack,
                                        handler_frame_idx}.parent());
    }
  );
  setup_exception_handler_frame(state.ctx, current_frame(state.stack),
                                handler_frame);
  return state.ctx.constants->tail_call_tag;
}

[[noreturn]] static void
builtin_exception_handler(context& ctx, ptr<> e) {
  if (auto cxx_e = match<cxx_exception>(e))
    cxx_e->rethrow();
  else
    throw scheme_exception{ctx, e};
}

static void
remember_current_stacktrace(context& ctx) {
  ctx.last_exception_stacktrace = make_stacktrace(ctx);
}

static ptr<tail_call_tag_type>
raise_from(vm& state, ptr<> e, frame_reference frame) {
  remember_current_stacktrace(state.ctx);

  if (frame_reference handler_frame
      = find_exception_handler_frame(state.stack, frame))
    return call_noncontinuable_exception_handler(state, handler_frame, e);
  else
    builtin_exception_handler(state.ctx, e);
}

static ptr<tail_call_tag_type>
raise(vm& state, ptr<> e) {
  return raise_from(state, e, current_frame(state.stack));
}

static ptr<tail_call_tag_type>
raise_continuable(vm& state, ptr<> e) {
  remember_current_stacktrace(state.ctx);

  frame_reference handler_frame = find_exception_handler_frame(
    state.stack,
    current_frame(state.stack)
  );
  if (handler_frame)
    return call_continuable_exception_handler(state, handler_frame, e);
  else
    builtin_exception_handler(state.ctx, e);
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
apply(vm& state, object_span args) {
  if (args.size() < 2)
    throw std::runtime_error{"apply: Expected at least 2 arguments"};

  ptr<> f = args[0];
  return tail_call_range(state, f, apply_args_range{state.ctx, args});
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
    [consumer = track(ctx, consumer)] (vm& state, ptr<> producer_result) {
      return tail_call(state, consumer.get(),
                       unpack_values(producer_result));
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
eval_proc(vm& state, ptr<> expr, tracked_ptr<module_> const& m) {
  ptr<syntax> stx = datum_to_syntax(state.ctx, {}, expr);
  auto f = compile_expression(state.ctx, stx, m, make_eval_origin());
  return tail_call(state, f, {});
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
