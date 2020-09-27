#include "vm.hpp"

#include "action.hpp"
#include "io.hpp"
#include "numeric.hpp"

#include <cassert>
#include <cstdint>
#include <stdexcept>
#include <vector>

namespace insider {

std::size_t
call_frame::extra_elements(ptr<insider::procedure> const& proc,
                           ptr<call_frame> const&,
                           std::vector<generic_ptr> const&,
                           std::vector<generic_ptr> const&) {
  return proc->locals_size;
}

call_frame::call_frame(ptr<insider::procedure> const& proc,
                       ptr<call_frame> const& parent,
                       std::vector<generic_ptr> const& closure,
                       std::vector<generic_ptr> const& arguments)
  : procedure_{proc.get()}
  , parent_frame_{parent.get()}
  , locals_size_{proc->locals_size}
{
  assert(closure.size() + arguments.size() <= locals_size_);

  for (std::size_t i = 0; i < closure.size(); ++i) {
    assert(closure[i]);
    storage_element(i) = closure[i].get();
  }

  for (std::size_t i = 0; i < arguments.size(); ++i) {
    assert(arguments[i]);
    storage_element(closure.size() + i) = arguments[i].get();
  }

  if (parent)
    assert(parent->dest_register_ != std::numeric_limits<operand>::max());
}

call_frame::call_frame(call_frame&& other)
  : pc{other.pc}
  , procedure_{other.procedure_}
  , parent_frame_{other.parent_frame_}
  , locals_size_{other.locals_size_}
  , dest_register_{other.dest_register_}
{
  for (std::size_t i = 0; i < locals_size_; ++i)
    storage_element(i) = other.storage_element(i);
}

void
call_frame::trace(tracing_context& tc) {
  tc.trace(procedure_);
  tc.trace(parent_frame_);
  for (std::size_t i = 0; i < locals_size_; ++i)
    tc.trace(storage_element(i));
}

void
call_frame::update_references() {
  update_reference(procedure_);
  update_reference(parent_frame_);
  for (std::size_t i = 0; i < locals_size_; ++i)
    update_reference(storage_element(i));
}

void
call_frame::set_local(free_store& store, std::size_t i, generic_ptr const& value) {
  assert(i < locals_size_);
  storage_element(i) = value.get();
  store.notify_arc(this, value.get());
}

static std::vector<generic_ptr>
collect_arguments(ptr<call_frame> const& frame, instruction instr, std::size_t first) {
  std::vector<generic_ptr> result;
  result.reserve(instr.operands.size() - first);
  for (std::size_t i = 0; i < instr.operands.size() - first; ++i)
    result.emplace_back(call_frame_local(frame, instr.operands[i + first]));

  return result;
}

static std::vector<generic_ptr>
collect_closure(ptr<closure> const& cls) {
  std::vector<generic_ptr> result;
  result.reserve(cls->size());
  for (std::size_t i = 0; i < cls->size(); ++i)
    result.push_back(closure_ref(cls, i));

  return result;
}

// Execute the current instruction in the current call frame.
static void
execute_one(execution_state& state) {
  ptr<call_frame>& frame = state.current_frame;
  frame->pc = decode_instruction(frame->procedure(state.ctx.store)->bytecode, frame->pc, state.current_instruction);
  instruction const& instr = state.current_instruction;

  switch (instr.opcode) {
  case opcode::no_operation:
    break;

  case opcode::load_static:
    call_frame_set_local(frame, instr.operands[1], state.ctx.get_static(instr.operands[0]));
    break;

  case opcode::load_global:
    call_frame_set_local(frame, instr.operands[1], state.ctx.get_top_level(instr.operands[0]));
    break;

  case opcode::store_global:
    state.ctx.set_top_level(instr.operands[1], call_frame_local(frame, instr.operands[0]));
    break;

  case opcode::add:
  case opcode::subtract:
  case opcode::multiply:
  case opcode::divide: {
    generic_ptr lhs = call_frame_local(frame, instr.operands[0]);
    generic_ptr rhs = call_frame_local(frame, instr.operands[1]);

    switch (instr.opcode) {
    case opcode::add:
      call_frame_set_local(frame, instr.operands[2], add(state.ctx, lhs, rhs));
      break;
    case opcode::subtract:
      call_frame_set_local(frame, instr.operands[2], subtract(state.ctx, lhs, rhs));
      break;
    case opcode::multiply:
      call_frame_set_local(frame, instr.operands[2], multiply(state.ctx, lhs, rhs));
      break;
    case opcode::divide:
      call_frame_set_local(frame, instr.operands[2], truncate_quotient(state.ctx, lhs, rhs));
      break;
    default:
      assert(!"Cannot get here");
    }

    break;
  }

  case opcode::arith_equal:
  case opcode::less_than:
  case opcode::greater_than: {
    generic_ptr lhs = call_frame_local(frame, instr.operands[0]);
    generic_ptr rhs = call_frame_local(frame, instr.operands[1]);

    switch (instr.opcode) {
    case opcode::arith_equal:
      call_frame_set_local(frame, instr.operands[2], arith_equal(state.ctx, lhs, rhs));
      break;
    case opcode::less_than:
      call_frame_set_local(frame, instr.operands[2], less(state.ctx, lhs, rhs));
      break;
    case opcode::greater_than:
      call_frame_set_local(frame, instr.operands[2], greater(state.ctx, lhs, rhs));
      break;
    default:
      assert(!"Cannot get here");
    }
    break;
  }

  case opcode::set:
    call_frame_set_local(frame, instr.operands[1], call_frame_local(frame, instr.operands[0]));
    break;

  case opcode::call:
  case opcode::tail_call: {
    generic_ptr callee = call_frame_local(frame, instr.operands[0]);
    operand num_args = instr.operands.size() - (instr.opcode == opcode::call ? 2 : 1);

    if (instr.opcode == opcode::call)
      frame->set_dest_register(instr.operands[1]);

    std::vector<generic_ptr> args = collect_arguments(frame, instr, instr.opcode == opcode::call ? 2 : 1);
    generic_ptr call_target = callee;
    std::vector<generic_ptr> closure;

    if (auto cls = match<insider::closure>(call_target)) {
      call_target = cls->procedure(state.ctx.store);
      closure = collect_closure(cls);
    }

    if (auto scheme_proc = match<procedure>(call_target)) {
      if (num_args < scheme_proc->min_args)
        throw error{"{}: Wrong number of arguments, expected {}{}, got {}",
                    scheme_proc->name ? *scheme_proc->name : "<lambda>",
                    scheme_proc->has_rest ? "at least " : "",
                    scheme_proc->min_args, num_args};

      if (scheme_proc->has_rest) {
        // We have to pass exactly min_args + 1 arguments. The last one is a
        // list with all the arguments after the min_args'th.

        std::size_t num_rest = args.size() - scheme_proc->min_args;
        std::vector<generic_ptr> rest_args(args.begin() + args.size() - num_rest, args.end());
        args.erase(args.begin() + args.size() - num_rest, args.end());

        assert(rest_args.size() == num_rest);
        assert(args.size() == scheme_proc->min_args);

        generic_ptr rest_list = make_list_from_vector(state.ctx, rest_args);
        args.push_back(rest_list);
      }

      if (instr.opcode == opcode::tail_call)
        state.current_frame = frame->parent(state.ctx.store);

      state.current_frame = state.ctx.store.make<call_frame>(
        scheme_proc, state.current_frame, closure, args
      );
    } else if (auto native_proc = match<native_procedure>(call_target)) {
      simple_action a{state.ctx, "in {}", native_proc->name ? *native_proc->name : "<native procedure>"};

      assert(closure.empty());
      generic_ptr result = native_proc->target(state.ctx, args);

      if (instr.opcode == opcode::call)
        call_frame_set_local(frame, frame->dest_register(), result);
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
        call_frame_set_local(state.current_frame, state.current_frame->dest_register(), result);
      }
    } else
      throw error{"Application: Not a procedure: {}", datum_to_string(state.ctx, call_target)};
    break;
  }

  case opcode::ret: {
    if (!frame->parent(state.ctx.store)) {
      // We are returning from the global procedure, so we have nowhere to
      // return to. We will set the global_return value and *not* pop the
      // stack. We'll then set pc past all the bytecode so we finish execution.

      state.global_return = call_frame_local(frame, instr.operands[0]);
      frame->pc = frame->procedure(state.ctx.store)->bytecode.size();
      return;
    }

    generic_ptr return_value = call_frame_local(frame, instr.operands[0]);
    assert(return_value);

    state.current_frame = frame->parent(state.ctx.store);
    call_frame_set_local(state.current_frame, state.current_frame->dest_register(), return_value);
    break;
  }

  case opcode::jump:
  case opcode::jump_back:
  case opcode::jump_unless:
  case opcode::jump_back_unless: {
    operand off = (instr.opcode == opcode::jump || instr.opcode == opcode::jump_back)
                  ? instr.operands[0] : instr.operands[1];
    int offset = (instr.opcode == opcode::jump_back || instr.opcode == opcode::jump_back_unless) ? -off : off;
    assert(offset >= 0 || static_cast<std::uint32_t>(-offset) <= frame->pc);
    assert(offset < 0 || offset + frame->pc < frame->procedure(state.ctx.store)->bytecode.size());
    std::uint32_t destination = frame->pc + offset;

    if (instr.opcode == opcode::jump_unless || instr.opcode == opcode::jump_back_unless) {
      generic_ptr test_value = call_frame_local(frame, instr.operands[0]);

      // The only false value in Scheme is #f. So we only jump if the test_value
      // is exactly #f.

      if (test_value != state.ctx.constants->f)
        break;
    }

    frame->pc = destination;
    break;
  }

  case opcode::make_closure: {
    ptr<procedure> proc = assume<procedure>(call_frame_local(frame, instr.operands[0]));
    operand dest = instr.operands[1];

    std::vector<generic_ptr> captures = collect_arguments(frame, instr, 2);
    call_frame_set_local(frame, dest, state.ctx.store.make<closure>(proc, captures));
    break;
  }

  case opcode::box:
    call_frame_set_local(frame, instr.operands[1],
                         state.ctx.store.make<box>(call_frame_local(frame, instr.operands[0])));
    break;

  case opcode::unbox:
    call_frame_set_local(frame, instr.operands[1],
                         unbox(expect<box>(call_frame_local(frame, instr.operands[0]))));
    break;

  case opcode::box_set:
    box_set(expect<box>(call_frame_local(frame, instr.operands[0])),
            call_frame_local(frame, instr.operands[1]));
    break;

  case opcode::cons:
    call_frame_set_local(frame, instr.operands[2],
                         make<pair>(state.ctx,
                                    call_frame_local(frame, instr.operands[0]),
                                    call_frame_local(frame, instr.operands[1])));
    break;

  case opcode::make_vector: {
    std::vector<generic_ptr> elems = collect_arguments(frame, instr, 1);
    call_frame_set_local(frame, instr.operands[0], make_vector(state.ctx, elems));
    break;
  }
  } // end switch
}

execution_state
make_state(context& ctx, ptr<procedure> const& global,
           std::vector<generic_ptr> const& closure, std::vector<generic_ptr> const& arguments) {
  ptr<call_frame> root_frame = ctx.store.make<call_frame>(
    global,
    ptr<call_frame>{},
    closure,
    arguments
  );

  instruction preallocated{};
  preallocated.operands.reserve(32);
  return execution_state{ctx, root_frame, root_frame, {}, preallocated};
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
      ptr<call_frame> frame = state_.current_frame;
      while (frame) {
        std::optional<std::string> name = call_frame_procedure(frame)->name;

        if (frame != state_.current_frame)
          result += '\n';
        result += fmt::format("in {}", name ? *name : "<lambda>");

        frame = call_frame_parent(frame);
      }

      return result;
    }

  private:
    execution_state& state_;
  };
}

generic_ptr
run(execution_state& state) {
  execution_action a(state);

  while (state.current_frame->pc < state.current_frame->procedure(state.ctx.store)->bytecode.size()) {
    execute_one(state);
    state.ctx.store.update();
  }

  assert(!state.current_frame->parent(state.ctx.store)); // Non-global procedures must finish by
                                                         // executing ret.

  return state.global_return;
}

generic_ptr
call(context& ctx, generic_ptr callable, std::vector<generic_ptr> const& arguments) {
  std::vector<generic_ptr> closure;
  if (auto cls = match<insider::closure>(callable)) {
    callable = closure_procedure(cls);
    closure = collect_closure(cls);
  }

  if (auto scheme_proc = match<procedure>(callable)) {
    if (scheme_proc->min_args != arguments.size())
      throw std::runtime_error{"Wrong number of arguments in function call"};

    execution_state state = make_state(ctx, scheme_proc, closure, arguments);
    return run(state);
  } else if (auto native_proc = match<native_procedure>(callable)) {
    assert(closure.empty());
    return native_proc->target(ctx, arguments);
  } else
    throw std::runtime_error{"Expected a callable"};
}

} // namespace insider
