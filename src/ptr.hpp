#ifndef INSIDER_PTR_HPP
#define INSIDER_PTR_HPP

#include <cassert>
#include <cstddef>
#include <functional>

namespace insider {

template <typename = void>
class tracked_ptr;

class integer;
class object;

// Non-tracked pointer to a Scheme object, including to immediate values such as
// fixnums. ptr<> is a pointer to any object, ptr<T> is a pointer to an object
// of type T.
template <typename = void>
class ptr;

template <>
class ptr<> {
public:
  ptr() = default;

  ptr(object* value) : value_{value} { }

  ptr(std::nullptr_t) { }

  explicit
  operator bool () const { return value_ != nullptr; }

  void
  reset() { value_ = nullptr; }

  void
  reset(ptr<> new_value) { value_ = new_value.value_; }

  object*
  value() const { return value_; }

protected:
  object* value_ = nullptr;
};

template <typename T>
class ptr : public ptr<> {
public:
  ptr() = default;

  ptr(T* value) : ptr<>(value) { }

  ptr(std::nullptr_t) { }

  T*
  operator -> () const { return static_cast<T*>(value_); }

  T&
  operator * () const { return *static_cast<T*>(value_); }

  T*
  value() const { return static_cast<T*>(value_); }
};

template <typename T, typename U>
bool
operator == (ptr<T> lhs, ptr<U> rhs) { return lhs.value() == rhs.value(); }

template <typename T>
bool
operator == (ptr<T> lhs, std::nullptr_t) { return lhs.value() == nullptr; }

template <typename T>
bool
operator == (std::nullptr_t, ptr<T> rhs) { return rhs.value() == nullptr; }

template <typename T, typename U>
bool
operator != (ptr<T> lhs, ptr<U> rhs) { return lhs.value() != rhs.value(); }

template <typename T>
bool
operator != (ptr<T> lhs, std::nullptr_t) { return lhs.value() != nullptr; }

template <typename T>
bool
operator != (std::nullptr_t, ptr<T> rhs) { return rhs.value() != nullptr; }

template <typename T>
bool
operator < (ptr<T> lhs, ptr<T> rhs) { return std::less<T*>{}(lhs.value(), rhs.value()); }

template <typename>
bool
is(ptr<>);

template <typename T>
ptr<T>
ptr_cast(ptr<> value) {
  assert(!value || is<T>(value));
  return ptr<T>{static_cast<T*>(value.value())};
}

template <>
inline ptr<>
ptr_cast<void>(ptr<> value) { return value; }

} // namespace insider

namespace std {
  template <typename T>
  struct hash<insider::ptr<T>> {
    auto
    operator () (insider::ptr<T> value) const { return std::hash<insider::object*>{}(value.value()); }
  };
} // namespace std

namespace insider {

class free_store;

namespace detail {
  template <typename Derived>
  class tracked_ptr_base {
  public:
    tracked_ptr_base() noexcept = default;

    tracked_ptr_base(free_store& fs, object* value) noexcept
      : value_{value}
      , store_{&fs}
    {
      link();
    }

    tracked_ptr_base(free_store& fs, ptr<> value) noexcept
      : tracked_ptr_base(fs, value.value())
    { }

    tracked_ptr_base(tracked_ptr_base const& other) noexcept
      : value_{other.value_}
      , store_{other.store_}
    {
      if (store_)
        link();
    }

    ~tracked_ptr_base() {
      if (store_)
        unlink();
    }

    tracked_ptr_base&
    operator = (tracked_ptr_base const& other) noexcept {
      if (this == &other)
        return *this;

      if (store_ && store_ != other.store_)
        unlink();

      value_ = other.value_;
      store_ = other.store_;

      if (store_ && !prev_)
        link();

      return *this;
    }

    void
    reset() noexcept { value_ = nullptr; }

    object&
    operator * () const noexcept { return *get(); }

    ptr<>
    operator -> () const noexcept { return get(); }

    ptr<>
    get() const noexcept { return ptr<>{value_}; }

    explicit
    operator bool () const { return value_ != nullptr; }

    Derived*
    next() const noexcept { return next_; }

    Derived*
    prev() const noexcept { return prev_; }

    free_store&
    store() const noexcept { assert(store_); return *store_; }

  protected:
    friend class insider::free_store;

    object*     value_ = nullptr;
    free_store* store_ = nullptr;
    Derived*    prev_  = nullptr;
    Derived*    next_  = nullptr;

    void
    link() noexcept {
      assert(!prev_);
      assert(!next_);
      assert(store_);

      Derived* root = Derived::root_list(*store_);
      Derived* root_next = root->next_;

      prev_ = root;
      root->next_ = static_cast<Derived*>(this);
      next_ = root_next;

      if (next_)
        next_->prev_ = static_cast<Derived*>(this);

      assert(prev_);
    }

    void
    unlink() noexcept {
      assert(prev_);

      prev_->next_ = next_;
      if (next_)
        next_->prev_ = prev_;

      prev_ = nullptr;
      next_ = nullptr;
    }
  };
} // namespace detail

template <typename>
class tracked_ptr;

// Untyped pointer to a Scheme object, registered with the garbage collector as a GC root.
template <>
class tracked_ptr<> : public detail::tracked_ptr_base<tracked_ptr<>> {
public:
  using tracked_ptr_base::tracked_ptr_base;

  tracked_ptr<>&
  operator = (tracked_ptr<> const& other) noexcept = default;

private:
  friend class detail::tracked_ptr_base<tracked_ptr<>>;

  static tracked_ptr<>*
  root_list(free_store& fs) noexcept;
};

template <typename = void>
class weak_ptr;

// Like generic_ptr, but does not keep an object alive.
template <>
class weak_ptr<> : public detail::tracked_ptr_base<weak_ptr<>> {
public:
  using tracked_ptr_base::tracked_ptr_base;

  weak_ptr(weak_ptr<> const& other) noexcept : tracked_ptr_base{other} { }

  weak_ptr<>&
  operator = (weak_ptr<> const& other) noexcept = default;

  tracked_ptr<>
  lock(free_store& fs) const noexcept { return {fs, value_}; }

private:
  friend class detail::tracked_ptr_base<weak_ptr<>>;

  static weak_ptr<>*
  root_list(free_store& fs) noexcept;
};

template <typename Derived>
bool
operator == (detail::tracked_ptr_base<Derived> const& lhs, detail::tracked_ptr_base<Derived> const& rhs) noexcept {
  return lhs.get() == rhs.get();
}

template <typename Derived>
bool
operator != (detail::tracked_ptr_base<Derived> const& lhs, detail::tracked_ptr_base<Derived> const& rhs) noexcept {
  return !operator == (lhs, rhs);
}

// Typed pointer to a garbage-collectable object.
template <typename T>
class tracked_ptr : public tracked_ptr<> {
public:
  using tracked_ptr<>::tracked_ptr;

  tracked_ptr&
  operator = (tracked_ptr const& other) noexcept = default;

  T&
  operator * () const noexcept { return *get(); }

  T*
  operator -> () const noexcept { return get().value(); }

  ptr<T>
  get() const noexcept { return ptr_cast<T>(tracked_ptr<>::get()); }
};

// Typed weak pointer to a garbage-collectable object.
template <typename T>
class weak_ptr : public weak_ptr<> {
public:
  using weak_ptr<>::weak_ptr;

  weak_ptr(tracked_ptr<T> const& other) noexcept
    : weak_ptr{other.store(), other.get()}
  { }

  weak_ptr&
  operator = (weak_ptr const& other) noexcept = default;

  T&
  operator * () const noexcept { return *get(); }

  T*
  operator -> () const noexcept { return get(); }

  ptr<T>
  get() const noexcept { return ptr_cast<T>(weak_ptr<>::get()); }

  tracked_ptr<T>
  lock() const noexcept { return {*store_, get()}; }
};

} // namespace insider

#endif
