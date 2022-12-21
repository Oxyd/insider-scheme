#ifndef INSIDER_MEMORY_MEMBER_VISITOR_HPP
#define INSIDER_MEMORY_MEMBER_VISITOR_HPP

#include "ptr.hpp"

#include <functional>

namespace insider {

class member_visitor {
public:
  virtual
  ~member_visitor() = default;

  virtual void
  operator () (ptr<>&) const = 0;

  virtual void
  weak(ptr<>&) const = 0;
};

} // namespace insider

#endif
