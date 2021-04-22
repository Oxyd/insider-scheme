#ifndef INSIDER_VM_HPP
#define INSIDER_VM_HPP

#include "scheme.hpp"

#include <vector>

namespace insider {

// Create a new execution state with the given procedure as the root frame,
// execute it, and return the procedure's return value.
//
// Causes a garbage collection.
generic_tracked_ptr
call(context&, ptr<> callable, std::vector<ptr<>> const& arguments);

} // namespace insider

#endif
