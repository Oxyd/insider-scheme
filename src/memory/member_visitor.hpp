#ifndef INSIDER_MEMORY_MEMBER_VISITOR_HPP
#define INSIDER_MEMORY_MEMBER_VISITOR_HPP

#include "ptr.hpp"

#include <functional>

namespace insider {

// Helper type to allow visit_members overrides to call either f(member) or
// f(weak(member)).
struct ptr_wrapper {
  ptr<>& value;
  bool   weak = false;

  ptr_wrapper(ptr<>& value)
    : value{value}
  { }

  ptr_wrapper(ptr<>& value, bool weak)
    : value{value}
    , weak{weak}
  { }
};

inline ptr_wrapper
weak(ptr<>& x) { return {x, true}; }

class member_visitor {
public:
  virtual
  ~member_visitor() = default;

  virtual void
  operator () (ptr_wrapper) const = 0;
};

} // namespace insider

#endif
