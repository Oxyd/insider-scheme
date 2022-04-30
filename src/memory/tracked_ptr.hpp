#ifndef INSIDER_MEMORY_TRACKED_PTR_HPP
#define INSIDER_MEMORY_TRACKED_PTR_HPP

#include "object.hpp"
#include "ptr.hpp"
#include "root_provider.hpp"

#include <type_traits>

namespace insider {

namespace detail {
  template <typename T>
  class tracked_ptr_base : public root_provider {
  public:
    explicit
    tracked_ptr_base(free_store& fs)
      : root_provider{fs}
    { }

    tracked_ptr_base(free_store& fs, T* value)
      : root_provider{fs}
      , value_{value}
    { }

    tracked_ptr_base(free_store& fs, ptr<T> value)
      : root_provider{fs}
      , value_{value}
    { }

    tracked_ptr_base(root_list& list, ptr<T> value)
      : root_provider{list}
      , value_{value}
    { }

    tracked_ptr_base(tracked_ptr_base const& other)
      : root_provider{other}
      , value_{other.value_}
    { }

    tracked_ptr_base(tracked_ptr_base&& other)
      : root_provider{std::move(other)}
      , value_{other.value_}
    {
      other.value_ = {};
    }

    tracked_ptr_base&
    operator = (tracked_ptr_base const&) = default;

    tracked_ptr_base&
    operator = (tracked_ptr_base&&) = default;

    template <typename U, typename = std::enable_if_t<std::is_convertible_v<U*, T*>>>
    tracked_ptr_base&
    operator = (ptr<U> value) { value_ = value; return *this; }

    void
    reset() noexcept { value_ = {}; }

    ptr<T>
    operator -> () const noexcept { return get(); }

    ptr<T>
    get() const noexcept { return value_; }

    explicit
    operator bool () const { return value_ != nullptr; }

  protected:
    ptr<T> value_;
  };
} // namespace detail

// Untyped pointer to a Scheme object, registered with the garbage collector as a GC root.
template <typename T = void>
class tracked_ptr : public detail::tracked_ptr_base<T> {
public:
  using detail::tracked_ptr_base<T>::tracked_ptr_base;

  template <typename U, typename = std::enable_if_t<std::is_convertible_v<U*, T*>>>
  tracked_ptr(tracked_ptr<U> const& other)
    : detail::tracked_ptr_base<T>{other.list(), other.get()}
  { }

  using detail::tracked_ptr_base<T>::operator =;

private:
  void
  visit_roots(member_visitor const& f) override {
    f(this->value_);
  }
};

// Like tracked_ptr, but does not keep an object alive.
template <typename T = void>
class weak_ptr : public detail::tracked_ptr_base<T> {
public:
  using detail::tracked_ptr_base<T>::tracked_ptr_base;

  weak_ptr(tracked_ptr<T> const& tp)
    : detail::tracked_ptr_base<T>{tp}
  { }

  tracked_ptr<T>
  lock() const {
    return {this->list(), this->get()};
  }

private:
  void
  visit_roots(member_visitor const& f) override {
    f(weak(this->value_));
  }
};

template <typename T>
bool
operator == (detail::tracked_ptr_base<T> const& lhs, detail::tracked_ptr_base<T> const& rhs) {
  return lhs.get() == rhs.get();
}

template <typename T>
bool
operator != (detail::tracked_ptr_base<T> const& lhs, detail::tracked_ptr_base<T> const& rhs) {
  return lhs.get() != rhs.get();
}

} // namespace insider

#endif
