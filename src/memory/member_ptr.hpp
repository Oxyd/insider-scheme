#ifndef INSIDER_MEMORY_MEMBER_PTR_HPP
#define INSIDER_MEMORY_MEMBER_PTR_HPP

#include "memory/free_store.hpp"
#include "object.hpp"

namespace insider {

// Pointer type like ptr<T> that enforces a write barrier. For this reason, it
// is intentionally not assignable and assignment needs to be done through the
// .assign member which will notify the store of a possible old-to-new arc.
template <typename T = void>
class member_ptr {
public:
  // Object constructors don't need to notify the store about arcs because
  // they're always to older objects. This function meant for use in
  // constructors and doesn't notify the store.
  static member_ptr
  initialise(ptr<T> value) {
    return member_ptr{value};
  }

  member_ptr() = default;

  member_ptr(free_store& store, ptr<> owner, ptr<T> value)
    : value_{value}
  { 
    store.notify_arc(owner, value_);
  }

  member_ptr(member_ptr const&) = delete;

  void
  operator = (member_ptr const&) = delete;

  void
  assign(free_store& store, ptr<> owner, ptr<T> new_value) {
    value_ = new_value;
    store.notify_arc(owner, new_value);
  }
  
  // Similar to initialise. Won't notify the store. Only useful in constructors.
  void
  assign_without_notify(ptr<T> new_value) {
    value_ = new_value;
  }

  ptr<T>
  get() const { return value_; }

  operator ptr<T> () const { return get(); }

  void
  visit_members(member_visitor const& f) const {
    f(value_);
  }

private:
  ptr<T> value_;

  explicit
  member_ptr(ptr<T> value) 
    : value_{value}
  { }
};

} // namespace insider

#endif
