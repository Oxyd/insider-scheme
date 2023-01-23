#ifndef INSIDER_PTR_HPP
#define INSIDER_PTR_HPP

#include <cassert>
#include <cstddef>
#include <cstdint>
#include <functional>

namespace insider {

class object;
using word_type = std::uint64_t;

// Non-tracked pointer to a Scheme object, including to immediate values such as
// fixnums. ptr<> is a pointer to any object, ptr<T> is a pointer to an object
// of type T.
template <typename = void>
class ptr;

namespace detail {
  // The free store needs to be able to update pointers when it moves objects.
  // This update makes a pointer point to the same object representing the same
  // value, except at a new location. Since this isn't morally a modification of
  // the pointer, we allow the free store to mutate this value even for const
  // ptr<>'s.

  void
  update_ptr(ptr<> const& p, ptr<> new_value);
}

inline word_type
object_header_address(object* o) {
  return reinterpret_cast<word_type>(
    reinterpret_cast<std::byte*>(o) - sizeof(word_type)
  );
}

inline object*
object_address(word_type w) {
  return reinterpret_cast<object*>(w + sizeof(word_type));
}

template <>
class ptr<> {
public:
  ptr() = default;

  explicit
  ptr(word_type value)
    : value_{value}
  { }

  ptr(object* value) {
    if (value)
      value_ = object_header_address(value);
  }

  ptr(std::nullptr_t) { }

  explicit
  operator bool () const { return value_ != 0; }

  void
  reset() { value_ = 0; }

  void
  reset(ptr<> new_value) { value_ = new_value.value_; }

  word_type
  as_word() const { return value_; }

  word_type&
  header() const { return *reinterpret_cast<word_type*>(value_); }

  object*
  value() const {
    if (value_)
      return object_address(value_);
    else
      return nullptr;
  }

  friend auto
  operator <=> (ptr const&, ptr const&) = default;

private:
  friend void detail::update_ptr(ptr<> const&, ptr<>);

  mutable word_type value_ = 0;
};

inline bool
operator == (ptr<> p, std::nullptr_t) {
  return p.value() == nullptr;
}

template <typename T>
class ptr : public ptr<> {
public:
  ptr() = default;

  explicit
  ptr(word_type w) : ptr<>{w} { }

  ptr(T* value) : ptr<>(value) { }

  ptr(std::nullptr_t) { }

  T*
  operator -> () const { return value(); }

  T&
  operator * () const { return *value(); }

  auto&
  operator ->* (auto T::* ptr) const { return value()->*ptr; }

  template <typename Ret, typename... Args>
  auto
  operator ->* (Ret (T::* fun)(Args...)) const {
    return [fun, this] (auto&&... args) {
      return (value()->*fun)(std::forward<decltype(args)>(args)...);
    };
  }

  template <typename Ret, typename... Args>
  auto
  operator ->* (Ret (T::* fun)(Args...) const) const {
    return [fun, this] (auto&&... args) {
      return (value()->*fun)(std::forward<decltype(args)>(args)...);
    };
  }

  T*
  value() const { return static_cast<T*>(ptr<>::value()); }
};

template <typename>
bool
is(ptr<>);

template <typename T>
ptr<T>
ptr_cast(ptr<> value) {
  assert(!value || is<T>(value));
  return ptr<T>{value.as_word()};
}

template <>
inline ptr<>
ptr_cast<void>(ptr<> value) { return value; }

} // namespace insider

namespace std {
  template <typename T>
  struct hash<insider::ptr<T>> {
    auto
    operator () (insider::ptr<T> value) const {
      return std::hash<insider::object*>{}(value.value());
    }
  };
} // namespace std

#endif
