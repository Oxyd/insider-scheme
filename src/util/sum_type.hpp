#ifndef INSIDER_UTIL_SUM_TYPE_HPP
#define INSIDER_UTIL_SUM_TYPE_HPP

#include "memory/root_provider.hpp"
#include "object.hpp"
#include "ptr.hpp"
#include "runtime/error.hpp"
#include "util/object_conversions.hpp"

#include <fmt/format.h>

#include <array>
#include <type_traits>

namespace insider {

template <typename>
struct from_scheme_converter;

template <typename>
struct to_scheme_converter;

namespace detail {

  template <typename... Us>
  void
  throw_if_not_one_of(ptr<> x) {
    if ((!is<Us>(x) && ...))
      throw make_error<type_error>(
        "Invalid type: Expected one of {}; got {}",
        fmt::join(std::array{type_name<Us>()...}, ", "),
        object_type_name(x)
      );
  }

} // namespace detail

template <typename... Ts>
class sum_type {
public:
  sum_type() = default;

  template <typename U>
  requires (std::is_same_v<U, Ts> || ...)
  sum_type(ptr<U> x)
    : value_{x}
  { }

  sum_type(ptr<> x)
    : value_{x}
  {
    if (x)
      detail::throw_if_not_one_of<Ts...>(x);
  }

  ptr<>
  get() const { return value_; }

  explicit
  operator bool () const { return static_cast<bool>(value_); }

  bool
  operator == (sum_type<Ts...> const&) const = default;

  void
  visit_members(member_visitor const& f) {
    f(value_);
  }

private:
  ptr<> value_;
};

template <typename T, typename... Ts>
bool
is(sum_type<Ts...> s) {
  return is<T>(s.get());
}

template <typename T, typename... Ts>
ptr<T>
expect(sum_type<Ts...> s) {
  return expect<T>(s.get());
}

template <typename T, typename... Ts>
ptr<T>
match(sum_type<Ts...> s) {
  return match<T>(s.get());
}

template <typename T, typename... Ts>
ptr<T>
assume(sum_type<Ts...> s) {
  return assume<T>(s.get());
}

template <typename... Ts, typename U>
bool
operator == (sum_type<Ts...> s, ptr<U> p) {
  return s.get() == p;
}

template <typename... Ts>
bool
operator == (sum_type<Ts...> s, std::nullptr_t) {
  return s.get() == nullptr;
}

namespace detail {

  template <typename... Ts>
  struct visit_impl;

  template <typename T>
  struct visit_impl<T> {
    template <typename Visitor, typename Sum>
    static auto
    visit(Visitor& v, Sum s) {
      return v(assume<T>(s.get()));
    }
  };

  template <typename T, typename... Rest>
  struct visit_impl<T, Rest...> {
    template <typename Visitor, typename Sum>
    static auto
    visit(Visitor& v, Sum s) {
      if (auto x = match<T>(s.get()))
        return v(x);
      else
        return visit_impl<Rest...>::visit(v, s);
    }
  };

} // namespace detail

template <typename Visitor, typename... Ts>
auto
visit(Visitor&& v, sum_type<Ts...> s) {
  return detail::visit_impl<Ts...>::visit(v, s);
}

template <typename... Ts>
struct from_scheme_converter<sum_type<Ts...>> {
  static sum_type<Ts...>
  convert(context&, ptr<> o) { return o; }
};

template <typename... Ts>
struct to_scheme_converter<sum_type<Ts...>> {
  static ptr<>
  convert(context&, sum_type<Ts...> sum) { return sum.get(); }
};

template <typename Sum>
class tracked_sum_type : public root_provider {
public:
  explicit
  tracked_sum_type(free_store& fs) : root_provider(fs) { }

  tracked_sum_type(free_store& fs, Sum s)
    : root_provider(fs)
    , sum_{s}
  { }

  Sum&
  get() { return sum_; }

  Sum
  get() const { return sum_; }

  tracked_sum_type&
  operator = (Sum s) { sum_ = s; return *this; }

private:
  Sum sum_;

  void
  visit_roots(member_visitor const& f) override {
    sum_.visit_members(f);
  }
};

} // namespace insider

#endif
