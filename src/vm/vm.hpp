#ifndef INSIDER_VM_VM_HPP
#define INSIDER_VM_VM_HPP

#include "ptr.hpp"
#include "runtime/basic_types.hpp"
#include "vm/call_stack.hpp"

#include <vector>

namespace insider {

class context;
class module_;
class syntax;
class tail_call_tag_type;

class vm : public root_provider {
public:
  context&            ctx;
  instruction_pointer ip{};
  call_stack          stack;
  ptr<>               result;

  explicit
  vm(context& ctx);

private:
  void
  visit_roots(member_visitor const&) override;
};

// Add a call frame to the current execution state, and set the continuation for
// the current frame. When the new frame finishes execution, it will call the
// given continuation. This allows for continuation jumps in and out of the
// current native frame.
//
// Only makes sense when called from a native procedure which was in turn called
// from Scheme.
//
// Inteded use is:
// return call_continuable(state, f, {args...},
//                         [=] (ptr<> result) { do something with result })
//
// Causes garbage collection.
ptr<tail_call_tag_type>
call_continuable(vm&, ptr<> callable, std::vector<ptr<>> const& arguments,
                 native_continuation::target_type cont);

// Add a call frame to the current execution state and run the procedure until
// it returns. Creates a new top-level execution state if one does not already
// exist. A full continuation barrier is installed around the call frame unless
// it's the root frame.
ptr<>
call_with_continuation_barrier(vm& state, ptr<> callable,
                               std::vector<ptr<>> const& arguments);

// Like call_with_continuation_barrier but also sets a parameter in the created
// call frame.
ptr<>
call_parameterized_with_continuation_barrier(
  vm& state,
  parameter_assignments const& params,
  ptr<> callable,
  std::vector<ptr<>> const& arguments,
  bool allow_jump_out = false,
  bool allow_jump_in = false
);

// Create a new VM and call given procedure in it. Because it's executed in a
// new VM, it's effectivelly wrapped in a continuation barrier.
ptr<>
call_root(context&, ptr<> callable, std::vector<ptr<>> const& arguments);

// Create a new VM, set the given parameters and call a new root procedure in
// it.
ptr<>
call_root_parameterized(context&, parameter_assignments const& params,
                        ptr<> callable, std::vector<ptr<>> const& arguments);

// Make a new parameter tag and set its top-level value in the context.
ptr<parameter_tag>
create_parameter_tag(context& ctx, ptr<> initial_value);

ptr<>
find_parameter_value(vm&, ptr<parameter_tag>);

// Pop the current call frame (which must be a native procedure frame), and
// replace it with a call frame for the given procedure. This is used to
// implement native procedures tail-calling other procedures.
//
// Intended use is: return tail_call(ctx, f, {args...});
ptr<tail_call_tag_type>
tail_call(vm&, ptr<> callable, std::vector<ptr<>> const& arguments);

// Compile an expression and evaluate it in the given module.
ptr<>
eval(context& ctx, tracked_ptr<module_> const&, ptr<syntax>);

// Parse a string as an expression and evaluate it in the given module.
ptr<>
eval(context& ctx, tracked_ptr<module_> const&, std::string const& expr);

} // namespace insider

#endif

