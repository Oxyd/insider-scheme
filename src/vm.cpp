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

execution_state::execution_state(context& ctx)
  : ctx{ctx}
  , value_stack{make<root_stack>(ctx)}
{
  value_stack->reserve(4096);
  call_stack.reserve(1024);
}

static std::vector<generic_ptr>
collect_closure(ptr<closure> const& cls) {
  std::vector<generic_ptr> result;
  result.reserve(cls->size());
  for (std::size_t i = 0; i < cls->size(); ++i)
    result.push_back(closure_ref(cls, i));

  return result;
}

// Make a call frame and copy closure and call arguments into it, and push the
// new frame to both value_stack and call_stack.
static std::size_t
push_call_frame(execution_state& state, ptr<procedure> const& proc,
                ptr<closure> const& cls, std::size_t num_args) {
  call_frame& result = state.call_stack.emplace_back(call_frame{proc, state.value_stack->size()});
  state.value_stack->grow(proc->locals_size);

  assert(state.call_stack.size() >= 2);
  call_frame& caller = *(state.call_stack.end() - 2);
  bytecode_decoder& bc = caller.bytecode;

  std::size_t closure_size = 0;
  if (cls) {
    closure_size = cls->size();
    for (std::size_t i = 0; i < closure_size; ++i)
      stack_set(state.value_stack, result.stack_top + i, closure_ref(cls, i));
  }

  for (std::size_t i = 0; i < num_args; ++i) {
    stack_set(state.value_stack, result.stack_top + closure_size + i,
              stack_ref(state.value_stack, caller.stack_top + bc.read_operand()));
  }

  return closure_size + num_args;
}

static void
pop_call_frame(execution_state& state) {
  std::size_t num_locals = state.call_stack.back().procedure->locals_size;
  state.call_stack.pop_back();
  state.value_stack->shrink(num_locals);
}

static void
erase_parent_frame(execution_state& state) {
  assert(state.call_stack.size() >= 2);

  call_frame const& parent = *(state.call_stack.end() - 2);
  std::size_t values_begin = parent.stack_top;
  std::size_t values_end = values_begin + parent.procedure->locals_size;

  state.call_stack.erase(state.call_stack.end() - 2);
  state.value_stack->erase(values_begin, values_end);

  state.call_stack.back().stack_top = values_begin;
}

static void
call_frame_set_local(execution_state& state, operand local, generic_ptr const& value) {
  assert(value);
  stack_set(state.value_stack, state.call_stack.back().stack_top + local, value);
}

generic_ptr
call_frame_local(execution_state& state, operand local) {
  return stack_ref(state.value_stack, state.call_stack.back().stack_top + local);
}

// Execute the current instruction in the current call frame.
static void
execute_one(execution_state& state) {
  call_frame& frame = state.call_stack.back();
  bytecode_decoder& bc = frame.bytecode;

  opcode op = bc.read_opcode();
  switch (op) {
  case opcode::no_operation:
    break;

  case opcode::load_static: {
    operand static_num = bc.read_operand();
    operand dest = bc.read_operand();
    call_frame_set_local(state, dest, state.ctx.get_static(static_num));
    break;
  }

  case opcode::load_top_level: {
    operand global_num = bc.read_operand();
    operand dest = bc.read_operand();
    call_frame_set_local(state, dest, state.ctx.get_top_level(global_num));
    break;
  }

  case opcode::store_top_level: {
    operand reg = bc.read_operand();
    operand global_num = bc.read_operand();
    state.ctx.set_top_level(global_num, call_frame_local(state, reg));
    break;
  }

  case opcode::add:
  case opcode::subtract:
  case opcode::multiply:
  case opcode::divide: {
    generic_ptr lhs = call_frame_local(state, bc.read_operand());
    generic_ptr rhs = call_frame_local(state, bc.read_operand());
    operand dest = bc.read_operand();

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
    generic_ptr lhs = call_frame_local(state, bc.read_operand());
    generic_ptr rhs = call_frame_local(state, bc.read_operand());
    operand dest = bc.read_operand();

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
    operand src = bc.read_operand();
    operand dst = bc.read_operand();
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

    generic_ptr callee;
    switch (op) {
    case opcode::call:
    case opcode::tail_call:
      callee = call_frame_local(state, bc.read_operand());
      break;

    case opcode::call_top_level:
    case opcode::tail_call_top_level:
      callee = state.ctx.get_top_level(bc.read_operand());
      break;

    case opcode::call_static:
    case opcode::tail_call_static:
      callee = state.ctx.get_static(bc.read_operand());
      break;

    default:
      assert(false);
    }

    if (!is_tail)
      frame.dest_register = bc.read_operand();

    operand num_args = bc.read_operand();
    generic_ptr call_target = callee;
    ptr<insider::closure> closure;

    if (auto cls = match<insider::closure>(call_target)) {
      call_target = cls->procedure(state.ctx.store);
      closure = cls;
    }

    if (auto scheme_proc = match<procedure>(call_target)) {
      if (num_args < scheme_proc->min_args)
        throw error{"{}: Wrong number of arguments, expected {}{}, got {}",
                    scheme_proc->name ? *scheme_proc->name : "<lambda>",
                    scheme_proc->has_rest ? "at least " : "",
                    scheme_proc->min_args, num_args};

      std::size_t values_used = push_call_frame(state, scheme_proc, closure, scheme_proc->min_args);
      bytecode_decoder& parent_bc = (state.call_stack.end() - 2)->bytecode;

      if (scheme_proc->has_rest) {
        // We have to pass exactly min_args + 1 arguments. The last one is a
        // list with all the arguments after the min_args'th.

        std::size_t num_rest = num_args - scheme_proc->min_args;
        std::vector<generic_ptr> rest_args;
        rest_args.reserve(num_rest);

        call_frame const& caller = *(state.call_stack.end() - 2);
        for (std::size_t i = 0; i < num_rest; ++i)
          rest_args.push_back(stack_ref(state.value_stack, caller.stack_top + parent_bc.read_operand()));

        generic_ptr rest_list = make_list_from_vector(state.ctx, rest_args);
        stack_set(state.value_stack, state.call_stack.back().stack_top + values_used, rest_list);
      }

      if (is_tail)
        erase_parent_frame(state);
    } else if (auto native_proc = match<native_procedure>(call_target)) {
      simple_action a{state.ctx, "in {}", native_proc->name ? *native_proc->name : "<native procedure>"};

      assert(!closure);

      std::vector<generic_ptr> args;
      args.reserve(num_args);
      for (std::size_t i = 0; i < num_args; ++i)
        args.push_back(stack_ref(state.value_stack, state.call_stack.back().stack_top + bc.read_operand()));

      generic_ptr result = native_proc->target(state.ctx, args);

      if (!is_tail)
        stack_set(state.value_stack, frame.stack_top + frame.dest_register, result);
      else {
        // tail_call. For Scheme procedures, the callee would perform a ret and
        // go back to this frame's caller. Since this is a native procedure, we
        // have to simulate the ret ourselves.

        if (state.call_stack.size() == 1) {
          // Same as in ret below: We're returning from the global procedure.

          state.global_return = result;
          frame.bytecode.jump_to_end();
          return;
        }

        pop_call_frame(state);
        stack_set(state.value_stack, state.call_stack.back().stack_top + state.call_stack.back().dest_register,
                  result);
      }
    } else
      throw error{"Application: Not a procedure: {}", datum_to_string(state.ctx, call_target)};
    break;
  }

  case opcode::ret: {
    operand return_reg = bc.read_operand();
    generic_ptr result = call_frame_local(state, return_reg);

    if (state.call_stack.size() == 1) {
      // We are returning from the global procedure, so we have nowhere to
      // return to. We will set the global_return value and *not* pop the
      // stack. We'll then set pc past all the bytecode so we finish execution.

      state.global_return = result;
      frame.bytecode.jump_to_end();
      return;
    }

    pop_call_frame(state);
    stack_set(state.value_stack, state.call_stack.back().stack_top + state.call_stack.back().dest_register,
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
      off = bc.read_operand();
    else {
      condition_reg = bc.read_operand();
      off = bc.read_operand();
    }

    int offset = (op == opcode::jump_back || op == opcode::jump_back_unless) ? -off : off;
    if (op == opcode::jump_unless || op == opcode::jump_back_unless) {
      generic_ptr test_value = call_frame_local(state, condition_reg);

      // The only false value in Scheme is #f. So we only jump if the test_value
      // is exactly #f.

      if (test_value != state.ctx.constants->f)
        break;
    }

    frame.bytecode.jump(offset);
    break;
  }

  case opcode::make_closure: {
    ptr<procedure> proc = assume<procedure>(call_frame_local(state, bc.read_operand()));
    operand dest = bc.read_operand();
    operand num_captures = bc.read_operand();

    auto result = make<closure>(state.ctx, proc, num_captures);
    for (std::size_t i = 0; i < num_captures; ++i)
      closure_set(result, i, call_frame_local(state, bc.read_operand()));

    call_frame_set_local(state, dest, result);
    break;
  }

  case opcode::box: {
    generic_ptr value = call_frame_local(state, bc.read_operand());
    call_frame_set_local(state, bc.read_operand(), state.ctx.store.make<box>(value));
    break;
  }

  case opcode::unbox: {
    auto box = expect<insider::box>(call_frame_local(state, bc.read_operand()));
    call_frame_set_local(state, bc.read_operand(), unbox(box));
    break;
  }

  case opcode::box_set: {
    auto box = expect<insider::box>(call_frame_local(state, bc.read_operand()));
    box_set(box, call_frame_local(state, bc.read_operand()));
    break;
  }

  case opcode::cons: {
    generic_ptr car = call_frame_local(state, bc.read_operand());
    generic_ptr cdr = call_frame_local(state, bc.read_operand());
    call_frame_set_local(state, bc.read_operand(), make<pair>(state.ctx, car, cdr));
    break;
  }

  case opcode::make_vector: {
    operand dest = bc.read_operand();
    operand num_elems = bc.read_operand();

    auto result = make<vector>(state.ctx, state.ctx, num_elems);
    for (std::size_t i = 0; i < num_elems; ++i)
      vector_set(result, i, call_frame_local(state, bc.read_operand()));

    call_frame_set_local(state, dest, result);
    break;
  }
  } // end switch
}

execution_state
make_state(context& ctx, ptr<procedure> const& global,
           std::vector<generic_ptr> const& closure, std::vector<generic_ptr> const& arguments) {
  execution_state result{ctx};
  result.call_stack.emplace_back(global, 0);
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
      for (auto frame = state_.call_stack.rbegin(); frame != state_.call_stack.rend(); ++frame) {
        std::optional<std::string> name = frame->procedure->name;

        if (frame != state_.call_stack.rbegin())
          result += '\n';
        result += fmt::format("in {}", name ? *name : "<lambda>");
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

  while (!state.call_stack.back().bytecode.done()) {
    execute_one(state);
    state.ctx.store.update();
  }

  assert(state.call_stack.size() == 1); // Non-global procedures must finish by executing ret.

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
