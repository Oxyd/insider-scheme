#ifndef INSIDER_VM_VM_HPP
#define INSIDER_VM_VM_HPP

#include "memory/root_provider.hpp"
#include "ptr.hpp"
#include "runtime/integer.hpp"
#include "vm/call_stack.hpp"

#include <vector>

namespace insider {

class context;
class module_;
class syntax;
class tail_call_tag_type;

// Add a call frame to the current execution state, and set the continuation for
// the current frame. When the new frame finishes execution, it will call the
// given continuation. This allows for continuation jumps in and out of the
// current native frame.
//
// Only makes sense when called from a native procedure which was in turn called
// from Scheme.
//
// Inteded use is:
// return call_continuable(ctx, f, {args...},
//                         [=] (ptr<> result) { do something with result })
//
// Causes garbage collection.
ptr<tail_call_tag_type>
call_continuable(context&, ptr<> callable, std::vector<ptr<>> const& arguments,
                 native_continuation_type cont);

// Add a call frame to the current execution state and run the procedure until
// it returns. Creates a new top-level execution state if one does not already
// exist. A full continuation barrier is installed around the call frame unless
// it's the root frame.
ptr<>
call_with_continuation_barrier(context& ctx, ptr<> callable,
                               std::vector<ptr<>> const& arguments);

// Like call_with_continuation_barrier but also sets a parameter in the created
// call frame.
ptr<>
call_parameterized_with_continuation_barrier(
  context& ctx,
  parameter_assignments const& params,
  ptr<> callable,
  std::vector<ptr<>> const& arguments
);


// Make a new parameter tag and set its top-level value in the context.
ptr<parameter_tag>
create_parameter_tag(context& ctx, ptr<> initial_value);

ptr<>
find_parameter_value(context&, ptr<parameter_tag>);

// Pop the current call frame (which must be a native procedure frame), and
// replace it with a call frame for the given procedure. This is used to
// implement native procedures tail-calling other procedures.
//
// Intended use is: return tail_call(ctx, f, {args...});
ptr<tail_call_tag_type>
tail_call(context&, ptr<> callable, std::vector<ptr<>> const& arguments);

// Compile an expression and evaluate it in the given module.
ptr<>
eval(context& ctx, tracked_ptr<module_> const&, ptr<syntax>);

// Parse a string as an expression and evaluate it in the given module.
ptr<>
eval(context& ctx, tracked_ptr<module_> const&, std::string const& expr);

struct stacktrace_record {
  enum class kind { scheme, native };

  std::string name;
  kind        kind;
};

std::vector<stacktrace_record>
stacktrace(context&);

} // namespace insider

#endif
