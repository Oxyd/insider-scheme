#ifndef INSIDER_PTR_HPP
#define INSIDER_PTR_HPP

#include <cassert>
#include <cstddef>
#include <functional>

namespace insider {

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

  friend auto
  operator <=> (ptr const&, ptr const&) = default;

protected:
  object* value_ = nullptr;
};

inline bool
operator == (ptr<> p, std::nullptr_t) {
  return p.value() == nullptr;
}

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

  auto&
  operator ->* (auto T::* ptr) const { return value()->*ptr; }

  template <typename Ret, typename... Args>
  auto
  operator ->* (Ret (T::* fun)(Args...)) const {
    return [fun, this] (Args... args) {
      return (value()->*fun)(args...);
    };
  }

  template <typename Ret, typename... Args>
  auto
  operator ->* (Ret (T::* fun)(Args...) const) const {
    return [fun, this] (Args... args) {
      return (value()->*fun)(args...);
    };
  }

  T*
  value() const { return static_cast<T*>(value_); }
};

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
    operator () (insider::ptr<T> value) const {
      return std::hash<insider::object*>{}(value.value());
    }
  };
} // namespace std

#endif
