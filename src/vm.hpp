#ifndef INSIDER_VM_HPP
#define INSIDER_VM_HPP

#include "numeric.hpp"
#include "ptr.hpp"

#include <vector>

namespace insider {

class context;

class root_stack;

struct execution_state {
  context&                ctx;
  tracked_ptr<root_stack> value_stack;
  integer::value_type     pc = -1;
  integer::value_type     frame_base = -1;

  execution_state(context& ctx);
};

// Add a call frame to the current execution state and run the procedure until
// it returns. Creates a new top-level execution state if one does not already
// exist.
//
// Causes a garbage collection.
generic_tracked_ptr
call(context&, ptr<> callable, std::vector<ptr<>> const& arguments);

} // namespace insider

#endif
