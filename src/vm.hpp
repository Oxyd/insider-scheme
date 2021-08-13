#ifndef INSIDER_VM_HPP
#define INSIDER_VM_HPP

#include "call_stack.hpp"
#include "numeric.hpp"
#include "ptr.hpp"

#include <vector>

namespace insider {

class context;
class module;
class tail_call_tag_type;

class root_stack;

struct execution_state {
  context&            ctx;
  integer::value_type pc = -1;

  execution_state(context& ctx);

  ptr<stack_frame>
  current_frame() const { return current_frame_.get(); }

  void
  set_current_frame(ptr<stack_frame> f);

  void
  set_current_frame_to_parent();

private:
  tracked_ptr<stack_frame> current_frame_;
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
// return call_continuable(ctx, f, {args...},
//                         [=] (ptr<> result) { do something with result })
//
// Causes garbage collection.
tracked_ptr<tail_call_tag_type>
call_continuable(context&, ptr<> callable, std::vector<ptr<>> const& arguments,
                 native_continuation_type cont);

// Add a call frame to the current execution state and run the procedure until
// it returns. Creates a new top-level execution state if one does not already
// exist. A full continuation barrier is installed around the call frame unless
// it's the root frame.
tracked_ptr<>
call_with_continuation_barrier(context& ctx, ptr<> callable, std::vector<ptr<>> const& arguments);

// Pop the current call frame (which must be a native procedure frame), and
// replace it with a call frame for the given procedure. This is used to
// implement native procedures tail-calling other procedures.
//
// Intended use is: return tail_call(ctx, f, {args...});
tracked_ptr<tail_call_tag_type>
tail_call(context&, ptr<> callable, std::vector<ptr<>> const& arguments);

void
export_vm(context&, module&);

} // namespace insider

#endif
