#ifndef INSIDER_UTIL_TO_SCHEME_HPP
#define INSIDER_UTIL_TO_SCHEME_HPP

#include "context.hpp"
#include "ptr.hpp"
#include "runtime/basic_types.hpp"
#include "runtime/integer.hpp"
#include "runtime/numeric.hpp"
#include "runtime/string.hpp"

#include <concepts>
#include <optional>

namespace insider {

template <typename>
struct to_scheme_converter;

template <typename T>
ptr<>
to_scheme(context& ctx, T const& t) {
  return to_scheme_converter<T>::convert(ctx, t);
}

template <>
struct to_scheme_converter<ptr<>> {
  static ptr<>
  convert(context&, ptr<> o) {
    assert(o);
    return o;
  }
};

template <>
struct to_scheme_converter<tracked_ptr<>> {
  static ptr<>
  convert(context&, tracked_ptr<> const& p) { return p.get(); }
};

template <typename T>
struct to_scheme_converter<ptr<T>> {
  static ptr<>
  convert(context&, ptr<T> o) { return o; }
};

template <typename T>
struct to_scheme_converter<tracked_ptr<T>> {
  static ptr<>
  convert(context&, tracked_ptr<T> const& p) { return p.get(); }
};

template <>
struct to_scheme_converter<integer> {
  static ptr<>
  convert(context&, integer i) { return integer_to_ptr(i); }
};

template <>
struct to_scheme_converter<char32_t> {
  static ptr<>
  convert(context&, char32_t c) { return character_to_ptr(c); }
};

template <>
struct to_scheme_converter<string_cursor> {
  static ptr<>
  convert(context&, string_cursor c) { return string_cursor_to_ptr(c); }
};

template <>
struct to_scheme_converter<bool> {
  static ptr<>
  convert(context& ctx, bool b) {
    return b ? ctx.constants->t : ctx.constants->f;
  }
};

template <std::integral T>
struct to_scheme_converter<T> {
  static ptr<>
  convert(context& ctx, T t) { return integer_to_scheme(ctx, t); }
};

template <>
struct to_scheme_converter<double> {
  static ptr<>
  convert(context& ctx, double value) {
    return make<floating_point>(ctx, value);
  }
};

template <>
struct to_scheme_converter<std::string> {
  static ptr<>
  convert(context& ctx, std::string const& s) { return make<string>(ctx, s); }
};

template <typename T>
struct to_scheme_converter<std::optional<T>> {
  static ptr<>
  convert(context& ctx, std::optional<T> const& t) {
    if (t)
      return to_scheme(ctx, *t);
    else
      return ctx.constants->f;
  }
};

template <typename... Ts>
ptr<>
to_scheme_list(context& ctx, Ts&&... ts) {
  return make_list(ctx, to_scheme(ctx, ts)...);
}

} // namespace insider

#endif
