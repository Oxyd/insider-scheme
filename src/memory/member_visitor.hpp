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
};

template <typename T>
concept visitable = requires (T& t, member_visitor const& f) {
  t.visit_members(f);
};

inline void
visit_members(member_visitor const& f, ptr<>& p) {
  f(p);
}

void
visit_members(member_visitor const& f, visitable auto& x) {
  x.visit_members(f);
}

} // namespace insider

#endif
