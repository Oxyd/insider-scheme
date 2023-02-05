#ifndef INSIDER_MEMORY_ROOT_PTR_HPP
#define INSIDER_MEMORY_ROOT_PTR_HPP

#include "object.hpp"
#include "root_provider.hpp"

#include <concepts>

namespace insider {

namespace detail {
  template <typename T>
  class root_ptr_base : public root_provider {
  public:
    explicit
    root_ptr_base(free_store& fs)
      : root_provider{fs}
    { }

    root_ptr_base(free_store& fs, T* value)
      : root_provider{fs}
      , value_{value}
    { }

    root_ptr_base(free_store& fs, ptr<T> value)
      : root_provider{fs}
      , value_{value}
    { }

    root_ptr_base(root_list& list, ptr<T> value)
      : root_provider{list}
      , value_{value}
    { }

    root_ptr_base(root_ptr_base const& other)
      : root_provider{other}
      , value_{other.value_}
    { }

    root_ptr_base(root_ptr_base&& other) noexcept
      : root_provider{std::move(other)}
      , value_{other.value_}
    {
      other.value_ = {};
    }

    root_ptr_base&
    operator = (root_ptr_base const&) = default;

    root_ptr_base&
    operator = (root_ptr_base&&)  noexcept = default;

    template <typename U>
    requires std::convertible_to<U*, T*>
    root_ptr_base&
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

// Untyped pointer to a Scheme object, registered with the garbage collector as
// a GC root.
template <typename T = void>
class root_ptr : public detail::root_ptr_base<T> {
public:
  using detail::root_ptr_base<T>::root_ptr_base;

  template <typename U>
  requires std::convertible_to<U*, T*>
  root_ptr(root_ptr<U> const& other)
    : detail::root_ptr_base<T>{other.list(), other.get()}
  { }

  using detail::root_ptr_base<T>::operator =;

private:
  void
  visit_roots(member_visitor const& f) override {
    f(this->value_);
  }
};

template <typename T>
bool
operator == (root_ptr<T> const& lhs, root_ptr<T> const& rhs) {
  return lhs.get() == rhs.get();
}

template <typename T>
bool
operator == (root_ptr<T> const& lhs, ptr<T> rhs) {
  return lhs.get() == rhs;
}

template <typename T>
auto
operator <=> (root_ptr<T> const& lhs, root_ptr<T> const& rhs) {
  return lhs.get() <=> rhs.get();
}

template <typename T>
auto
operator <=> (root_ptr<T> const& lhs, ptr<T> rhs) {
  return lhs.get() <=> rhs;
}

} // namespace insider

#endif
