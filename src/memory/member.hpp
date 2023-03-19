#ifndef INSIDER_MEMORY_MEMBER_HPP
#define INSIDER_MEMORY_MEMBER_HPP

#include "memory/free_store.hpp"
#include "object.hpp"

#include <utility>

namespace insider {

namespace detail {

  inline void
  notify_arcs(free_store& store, ptr<> owner, ptr<> value) {
    store.notify_arc(owner, value);
  }

  void
  notify_arcs(free_store& store, ptr<> owner, visitable auto const& value) {
    visit_members(value, [&] (ptr<> member) {
      store.notify_arc(owner, member);
    });
  }

  template <typename T>
  void
  notify_arcs(free_store& fs, ptr<> owner, std::vector<T> const& v) {
    for (auto const& x : v)
      detail::notify_arcs(fs, owner, x);
  }

} // namespace detail

// Wrapper around a value that may contain pointers to other Scheme objects,
// inteded to be used as a member of Scheme object tyeps. This wrapper is not
// assignable to enforce the free store's write barrier -- value may be changed
// using the .assign member.
template <typename T>
class member {
public:
  // Create a member without notifying the store. This is intended for use in
  // Scheme types' constructors where a write barrier isn't needed.
  static member
  initialise(T value) {
    return member{std::move(value)};
  }

  member() = default;

  member(free_store& store, ptr<> owner, T value)
    : value_{std::move(value)}
  {
    detail::notify_arcs(store, owner, value_);
  }

  void
  assign(free_store& store, ptr<> owner, T value) {
    value_ = std::move(value);
    detail::notify_arcs(store, owner, value_);
  }

  T const&
  get() const { return value_; }

  void
  visit_members(member_visitor const& f) const {
    insider::visit_members(value_, f);
  }

private:
  member(T value)
    : value_{std::move(value)}
  { }

  T value_;
};

template <typename T = void>
using member_ptr = member<ptr<T>>;

template <typename T>
member<T>
init(T t) { return member<T>::initialise(std::move(t)); }

} // namespace insider

#endif
