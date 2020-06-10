#include "vm.hpp"

#include "numeric.hpp"

#include <cassert>
#include <cstdint>
#include <stdexcept>
#include <vector>

namespace insider {

std::size_t
call_frame::extra_storage_size(ptr<insider::procedure> const& proc,
                               ptr<insider::closure> const&,
                               ptr<call_frame> const&,
                               std::vector<generic_ptr> const&) {
  return proc->locals_size * sizeof(object*);
}

call_frame::call_frame(ptr<insider::procedure> const& proc,
                       ptr<insider::closure> const& closure,
                       ptr<call_frame> const& parent,
                       std::vector<generic_ptr> const& arguments)
  : procedure_{proc.get()}
  , closure_{closure.get()}
  , parent_frame_{parent.get()}
  , locals_size_{proc->locals_size}
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
  for (std::size_t i = 0; i < locals_size_; ++i)
    f(dynamic_storage()[i]);
}

generic_ptr
call_frame::local(free_store& store, std::size_t i) const {
  assert(i < locals_size_);
  return {store, dynamic_storage()[i]};
}

void
call_frame::set_local(std::size_t i, generic_ptr const& value) {
  assert(i < locals_size_);
  dynamic_storage()[i] = value.get();
}

generic_ptr
call_frame::closure(free_store& store, std::size_t i) const {
  assert(closure_);
  return closure_->ref(store, i);
}

static generic_ptr
get_register(execution_state& state, operand op) {
  switch (op.scope()) {
  case operand::scope_type::local:
    return call_frame_local(state.current_frame, op.value());
  case operand::scope_type::closure:
    return call_frame_closure(state.current_frame, op.value());
  case operand::scope_type::global: {
    return state.ctx.get_top_level(op.value());
  }
  case operand::scope_type::static_:
    return state.ctx.get_static(op.value());
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
    state.ctx.set_top_level(op.value(), value);
    return;
  }
  case operand::scope_type::static_:
    throw std::runtime_error{"Cannot write to a static register"};
  }

  assert(!"Cannot get here");
}

static instruction const&
find_call(ptr<call_frame> const& frame) {
  free_store& store = frame.store();
  std::uint32_t i = frame->pc - 1;
  while (frame->procedure(store)->bytecode[i].opcode == opcode::data)
    --i;

  instruction const& call_instr = frame->procedure(store)->bytecode[i];
  assert(call_instr.opcode == opcode::call);

  return call_instr;
}

static std::vector<generic_ptr>
collect_data(execution_state& state, std::size_t num) {
  std::vector<generic_ptr> result(num);
  ptr<call_frame>& frame = state.current_frame;
  for (operand::representation_type i = 0; i < num; ++i) {
    instruction const& d = frame->procedure(state.ctx.store)->bytecode[frame->pc];
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
  instruction instr = frame->procedure(state.ctx.store)->bytecode[frame->pc++];

  switch (instr.opcode) {
  case opcode::no_operation:
    break;

  case opcode::add:
  case opcode::subtract:
  case opcode::multiply:
  case opcode::divide: {
    generic_ptr lhs = get_register(state, instr.x);
    generic_ptr rhs = get_register(state, instr.y);

    switch (instr.opcode) {
    case opcode::add:      set_register(state, instr.dest, add(state.ctx, lhs, rhs));               break;
    case opcode::subtract: set_register(state, instr.dest, subtract(state.ctx, lhs, rhs));          break;
    case opcode::multiply: set_register(state, instr.dest, multiply(state.ctx, lhs, rhs));          break;
    case opcode::divide:   set_register(state, instr.dest, truncate_quotient(state.ctx, lhs, rhs)); break;
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
    case opcode::arith_equal: set_register(state, instr.dest, arith_equal(state.ctx, lhs, rhs)); break;
    case opcode::less_than: set_register(state, instr.dest, less(state.ctx, lhs, rhs)); break;
    case opcode::greater_than: set_register(state, instr.dest, greater(state.ctx, lhs, rhs)); break;
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
    generic_ptr callee = get_register(state, instr.x);
    operand::representation_type num_args = instr.y.immediate_value();

    std::vector<generic_ptr> args = collect_data(state, num_args);
    generic_ptr call_target = callee;
    ptr<insider::closure> closure;

    if (auto cls = match<insider::closure>(call_target)) {
      closure = cls;
      call_target = cls->procedure(state.ctx.store);
    }

    if (auto scheme_proc = match<procedure>(call_target)) {
      if (num_args != scheme_proc->num_args)
        // TODO: Print the function name
        throw std::runtime_error{"Wrong number of arguments in function call"};

      if (instr.opcode == opcode::tail_call)
        state.current_frame = frame->parent(state.ctx.store);

      state.current_frame = state.ctx.store.make<call_frame>(
        scheme_proc, closure, state.current_frame, args
      );
    } else if (auto native_proc = match<native_procedure>(call_target)) {
      assert(!closure);
      generic_ptr result = native_proc->target(state.ctx, args);

      if (instr.opcode == opcode::call)
        set_register(state, instr.dest, result);
      else {
        // tail_call. For Scheme procedures, the callee would perform a ret and
        // go back to this frame's caller. Since this is a native procedure, we
        // have to simulate the ret ourselves.

        if (!frame->parent(state.ctx.store)) {
          // Same as in ret below: We're returning from the global procedure.

          state.global_return = result;
          frame->pc = frame->procedure(state.ctx.store)->bytecode.size();
          return;
        }

        state.current_frame = frame->parent(state.ctx.store);
        set_register(state, find_call(frame).dest, result);
      }
    } else
      throw std::runtime_error{"Error in application: Not a procedure"};
    break;
  }

  case opcode::ret: {
    if (!frame->parent(state.ctx.store)) {
      // We are returning from the global procedure, so we have nowhere to
      // return to. We will set the global_return value and *not* pop the
      // stack. We'll then set pc past all the bytecode so we finish execution.

      state.global_return = get_register(state, instr.x);
      frame->pc = frame->procedure(state.ctx.store)->bytecode.size();
      return;
    }

    generic_ptr return_value = get_register(state, instr.x);
    state.current_frame = frame->parent(state.ctx.store);

    // Now we are in the parent procedure, the PC points to the instruction
    // after the call and all the arguments. So we'll look back to the call
    // instruction and find the destination register for the return value.

    set_register(state, find_call(frame).dest, return_value);
    break;
  }

  case opcode::jump:
  case opcode::jump_unless: {
    operand::offset_type offset = instr.opcode == opcode::jump ? instr.x.offset() : instr.y.offset();
    assert(offset >= 0 || static_cast<std::uint32_t>(-offset) <= frame->pc);
    assert(offset < 0 || offset + frame->pc < frame->procedure(state.ctx.store)->bytecode.size());
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

  case opcode::make_closure: {
    ptr<procedure> proc = assume<procedure>(get_register(state, instr.x));
    operand::representation_type num_captures = instr.y.immediate_value();
    operand dest = instr.dest;
    std::vector<generic_ptr> captures = collect_data(state, num_captures);

    set_register(state, dest, state.ctx.store.make<closure>(proc, captures));
    break;
  }

  case opcode::box:
    set_register(state, instr.dest, state.ctx.store.make<box>(get_register(state, instr.x)));
    break;

  case opcode::unbox:
    set_register(state, instr.dest, unbox(expect<box>(get_register(state, instr.x))));
    break;

  case opcode::box_set:
    expect<box>(get_register(state, instr.x))->set(get_register(state, instr.y));
    break;

  case opcode::cons:
    set_register(state, instr.dest, make<pair>(state.ctx,
                                               get_register(state, instr.x),
                                               get_register(state, instr.y)));
    break;

  case opcode::make_vector: {
    operand::representation_type num_elems = instr.x.immediate_value();
    std::vector<generic_ptr> elems = collect_data(state, num_elems);
    set_register(state, instr.dest, make_vector(state.ctx, elems));
    break;
  }
  } // end switch
}

execution_state
make_state(context& ctx, ptr<procedure> const& global) {
  ptr<call_frame> root_frame = ctx.store.make<call_frame>(
    global,
    ptr<closure>{},
    ptr<call_frame>{},
    std::vector<generic_ptr>{}
  );

  return execution_state{ctx, root_frame, root_frame, {}};
}

generic_ptr
run(execution_state& state) {
  while (state.current_frame->pc < state.current_frame->procedure(state.ctx.store)->bytecode.size())
    execute_one(state);

  assert(!state.current_frame->parent(state.ctx.store)); // Non-global procedures must finish by
                                                         // executing ret.

  return state.global_return;
}

generic_ptr
call(context& ctx, generic_ptr callable, std::vector<generic_ptr> const& arguments) {
  ptr<closure> closure;
  if (auto cls = match<insider::closure>(callable)) {
    closure = cls;
    callable = closure_procedure(cls);
  }

  if (auto scheme_proc = match<procedure>(callable)) {
    if (scheme_proc->num_args != arguments.size())
      throw std::runtime_error{"Wrong number of arguments in function call"};

    auto frame = make<call_frame>(ctx, scheme_proc, closure, ptr<call_frame>{}, arguments);
    execution_state state{ctx, frame, frame, {}};
    return run(state);
  } else if (auto native_proc = match<native_procedure>(callable)) {
    assert(!closure);
    return native_proc->target(ctx, arguments);
  } else
    throw std::runtime_error{"Expected a callable"};
}

} // namespace insider
