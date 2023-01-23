#ifndef INSIDER_MEMORY_MEMBER_VISITOR_HPP
#define INSIDER_MEMORY_MEMBER_VISITOR_HPP

#include "object.hpp"

#include <functional>

namespace insider {

class member_visitor {
public:
  virtual
  ~member_visitor() = default;

  virtual void
  operator () (ptr<> const&) const = 0;

  void
  operator () (ptr<>&&) const = delete;

  virtual void
  weak(ptr<> const&) const = 0;
};

} // namespace insider

#endif
