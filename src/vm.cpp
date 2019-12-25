#include "vm.hpp"

#include <cassert>
#include <cstdint>
#include <stdexcept>
#include <vector>

namespace game::scm {

std::size_t
call_frame::extra_storage_size(ptr<scm::procedure> const& proc,
                               ptr<scm::closure> const&,
                               ptr<call_frame> const&,
                               std::vector<generic_ptr> const&,
                               ptr<scm::module> const&) {
  return proc->locals_size * sizeof(object*);
}

call_frame::call_frame(ptr<scm::procedure> const& proc,
                       ptr<scm::closure> const& closure,
                       ptr<call_frame> const& parent,
                       std::vector<generic_ptr> const& arguments,
                       ptr<scm::module> const& m)
  : procedure_{proc.get()}
  , closure_{closure.get()}
  , parent_frame_{parent.get()}
  , locals_size_{proc->locals_size}
  , module_{m.get()}
{
  assert(arguments.size() <= locals_size_);

  for (std::size_t i = 0; i < arguments.size(); ++i)
    dynamic_storage()[i] = arguments[i].get();
}

void
call_frame::for_each_subobject(std::function<void(object*)> const& f) {
  f(procedure_);
  f(closure_);
  f(parent_frame_);
  f(module_);
  for (std::size_t i = 0; i < locals_size_; ++i)
    f(dynamic_storage()[i]);
}

generic_ptr
call_frame::local(std::size_t i) const {
  assert(i < locals_size_);
  return dynamic_storage()[i];
}

void
call_frame::set_local(std::size_t i, generic_ptr const& value) {
  assert(i < locals_size_);
  dynamic_storage()[i] = value.get();
}

generic_ptr
call_frame::closure(std::size_t i) const {
  assert(closure_);
  return closure_->ref(i);
}

static generic_ptr
get_register(execution_state& state, operand op) {
  switch (op.scope()) {
  case operand::scope_type::local:
    return state.current_frame->local(op.value());
  case operand::scope_type::closure:
    return state.current_frame->closure(op.value());
  case operand::scope_type::global: {
    ptr<module> m = state.current_frame->module();
    ptr<module> parent = m->bindings[op.value()].parent;
    return parent->objects[*m->bindings[op.value()].index];
  }
  case operand::scope_type::static_:
    return state.ctx.get_static(op.value());
  }

  assert(!"Cannot get here");
  return {};
}

namespace {
  struct object_and_module {
    generic_ptr      object;
    ptr<scm::module> module;
  };
}

static object_and_module
get_register_and_module(execution_state& state, operand op) {
  switch (op.scope()) {
  case operand::scope_type::local:
  case operand::scope_type::static_:
  case operand::scope_type::closure:
    return {get_register(state, op), state.current_frame->module()};

  case operand::scope_type::global: {
    ptr<module> m = state.current_frame->module();
    ptr<module> parent = m->bindings[op.value()].parent;
    return {parent->objects[*m->bindings[op.value()].index], parent};
  }
  }

  assert(!"Cannot get here");
  return {};
}

static void
set_register(execution_state& state, operand op, generic_ptr const& value) {
  switch (op.scope()) {
  case operand::scope_type::local:
    state.current_frame->set_local(op.value(), value);
    return;
  case operand::scope_type::closure:
    // Does writing to closures make sense? Not implementing until I'm convinced
    // it does.
    throw std::runtime_error{"Cannot write to a closure register"};
  case operand::scope_type::global: {
    ptr<module> m = state.current_frame->module();
    ptr<module> parent = m->bindings[op.value()].parent;
    parent->objects[*m->bindings[op.value()].index] = value.get();
    return;
  }
  case operand::scope_type::static_:
    throw std::runtime_error{"Cannot write to a static register"};
  }

  assert(!"Cannot get here");
}

static instruction const&
find_call(call_frame const& frame) {
  std::uint32_t i = frame.pc - 1;
  while (frame.procedure()->bytecode[i].opcode == opcode::data)
    --i;

  instruction const& call_instr = frame.procedure()->bytecode[i];
  assert(call_instr.opcode == opcode::call);

  return call_instr;
}

static std::vector<generic_ptr>
collect_data(execution_state& state, std::size_t num) {
  std::vector<generic_ptr> result(num);
  ptr<call_frame>& frame = state.current_frame;
  for (operand::representation_type i = 0; i < num; ++i) {
    instruction const& d = frame->procedure()->bytecode[frame->pc];
    assert(d.opcode == opcode::data);

    switch (i % 3) {
    case 0:
      result[i] = get_register(state, d.x);
      break;
    case 1:
      result[i] = get_register(state, d.y);
      break;
    case 2:
      result[i] = get_register(state, d.dest);
      break;

    default:
      assert(!"Can't get here");
    }

    if (i % 3 == 2)
      ++frame->pc;
  }

  if (num % 3 != 0)
    ++frame->pc;

  return result;
}

// Execute the current instruction in the current call frame.
static void
execute_one(execution_state& state) {
  ptr<call_frame>& frame = state.current_frame;
  instruction instr = frame->procedure()->bytecode[frame->pc++];

  switch (instr.opcode) {
  case opcode::no_operation:
    break;

  case opcode::add:
  case opcode::subtract:
  case opcode::multiply:
  case opcode::divide: {
    ptr<integer> lhs = expect<integer>(get_register(state, instr.x));
    ptr<integer> rhs = expect<integer>(get_register(state, instr.y));

    switch (instr.opcode) {
    case opcode::add:      set_register(state, instr.dest, add(lhs, rhs));      break;
    case opcode::subtract: set_register(state, instr.dest, subtract(lhs, rhs)); break;
    case opcode::multiply: set_register(state, instr.dest, multiply(lhs, rhs)); break;
    case opcode::divide:   set_register(state, instr.dest, divide(lhs, rhs));   break;
    default:
      assert(!"Cannot get here");
    }

    break;
  }

  case opcode::arith_equal:
  case opcode::less_than:
  case opcode::greater_than: {
    ptr<integer> lhs = expect<integer>(get_register(state, instr.x));
    ptr<integer> rhs = expect<integer>(get_register(state, instr.y));
    switch (instr.opcode) {
    case opcode::arith_equal: set_register(state, instr.dest, arith_equal(lhs, rhs)); break;
    case opcode::less_than: set_register(state, instr.dest, less(lhs, rhs)); break;
    case opcode::greater_than: set_register(state, instr.dest, greater(lhs, rhs)); break;
    default:
      assert(!"Cannot get here");
    }
    break;
  }

  case opcode::set:
    set_register(state, instr.dest, get_register(state, instr.x));
    break;

  case opcode::call:
  case opcode::tail_call: {
    object_and_module callee = get_register_and_module(state, instr.x);
    operand::representation_type num_args = instr.y.immediate_value();
    if (auto scheme_proc = match<procedure>(callee.object))
      if (num_args != scheme_proc->num_args)
        // TODO: Print the function name
        throw std::runtime_error{"Wrong number of arguments in function call"};

    std::vector<generic_ptr> args = collect_data(state, num_args);
    generic_ptr call_target = callee.object;
    ptr<scm::closure> closure;

    if (auto cls = match<scm::closure>(call_target)) {
      closure = cls;
      call_target = cls->procedure();
    }

    if (auto scheme_proc = match<procedure>(call_target)) {
      if (instr.opcode == opcode::tail_call)
        state.current_frame = frame->parent();

      state.current_frame = state.ctx.store.make<call_frame>(
        scheme_proc, closure, state.current_frame, args, callee.module
      );
    } else if (auto native_proc = match<native_procedure>(call_target)) {
      generic_ptr result = native_proc->target(args);

      if (instr.opcode == opcode::call)
        set_register(state, instr.dest, result);
      else {
        // tail_call. For Scheme procedures, the callee would perform a ret and
        // go back to this frame's caller. Since this is a native procedure, we
        // have to simulate the ret ourselves.

        if (!frame->parent()) {
          // Same as in ret below: We're returning from the global procedure.

          state.global_return = result;
          frame->pc = frame->procedure()->bytecode.size();
          return;
        }

        state.current_frame = frame->parent();
        set_register(state, find_call(*frame).dest, result);
      }
    } else
      throw std::runtime_error{"Error in application: Not a procedure"};
    break;
  }

  case opcode::ret: {
    if (!frame->parent()) {
      // We are returning from the global procedure, so we have nowhere to
      // return to. We will set the global_return value and *not* pop the
      // stack. We'll then set pc past all the bytecode so we finish execution.

      state.global_return = get_register(state, instr.x);
      frame->pc = frame->procedure()->bytecode.size();
      return;
    }

    generic_ptr return_value = get_register(state, instr.x);
    state.current_frame = frame->parent();

    // Now we are in the parent procedure, the PC points to the instruction
    // after the call and all the arguments. So we'll look back to the call
    // instruction and find the destination register for the return value.

    set_register(state, find_call(*frame).dest, return_value);
    break;
  }

  case opcode::jump:
  case opcode::jump_unless: {
    operand::offset_type offset = instr.opcode == opcode::jump ? instr.x.offset() : instr.y.offset();
    assert(offset >= 0 || static_cast<std::uint32_t>(-offset) <= frame->pc);
    assert(offset < 0 || offset + frame->pc < frame->procedure()->bytecode.size());
    std::uint32_t destination = frame->pc + offset;

    if (instr.opcode == opcode::jump_unless) {
      generic_ptr test_value = get_register(state, instr.x);

      // The only false value in Scheme is #f. So we only jump if the test_value
      // is exactly #f.

      if (test_value != state.ctx.constants->f)
        break;
    }

    frame->pc = destination;
    break;
  }

  case opcode::data:
    assert(!"Attempting to execute data");
    break;

  case opcode::make_closure:
    ptr<procedure> proc = assume<procedure>(get_register(state, instr.x));
    operand::representation_type num_captures = instr.y.immediate_value();
    operand dest = instr.dest;
    std::vector<generic_ptr> captures = collect_data(state, num_captures);

    set_register(state, dest, state.ctx.store.make<closure>(proc, captures));
    break;
  }
}

execution_state
make_state(context& ctx, ptr<procedure> const& global, ptr<module> const& m) {
  ptr<call_frame> root_frame = ctx.store.make<call_frame>(
    global,
    ptr<closure>{},
    ptr<call_frame>{},
    std::vector<generic_ptr>(global->locals_size),
    m
  );

  return execution_state{ctx, root_frame, root_frame, {}};
}

generic_ptr
run(execution_state& state) {
  while (state.current_frame->pc < state.current_frame->procedure()->bytecode.size())
    execute_one(state);

  assert(!state.current_frame->parent()); // Non-global procedures must finish by
                                          // executing ret.

  return state.global_return;
}

} // namespace game::scm
