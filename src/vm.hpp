#ifndef INSIDER_VM_HPP
#define INSIDER_VM_HPP

#include "call_stack.hpp"
#include "numeric.hpp"
#include "ptr.hpp"

#include <vector>

namespace insider {

class context;
struct tail_call_tag_type;

class root_stack;

struct execution_state {
  context&                 ctx;
  tracked_ptr<stack_frame> current_frame;
  integer::value_type      pc = -1;

  execution_state(context& ctx);
};

// Add a call frame to the current execution state and run the procedure until
// it returns. Creates a new top-level execution state if one does not already
// exist.
//
// Causes a garbage collection.
generic_tracked_ptr
call(context&, ptr<> callable, std::vector<ptr<>> const& arguments);

// Pop the current call frame (which must be a native procedure frame), and
// replace it with a call frame for the given procedure. This is used to
// implement native procedures tail-calling other procedures.
//
// Intended use is: return tail_call(ctx, f, {args...});
tracked_ptr<tail_call_tag_type>
tail_call(context&, ptr<> callable, std::vector<ptr<>> const& arguments);

} // namespace insider

#endif
