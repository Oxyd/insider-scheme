#ifndef INSIDER_TO_SCHEME_HPP
#define INSIDER_TO_SCHEME_HPP

#include "basic_types.hpp"
#include "context.hpp"
#include "integer.hpp"
#include "numeric.hpp"
#include "ptr.hpp"
#include "string.hpp"

#include <optional>

namespace insider {

template <typename T, typename Enable = void>
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

template <typename T>
struct to_scheme_converter<T, std::enable_if_t<std::is_integral_v<T> && !std::is_same_v<T, bool>>> {
  static ptr<>
  convert(context& ctx, T t) { return integer_to_scheme(ctx, t); }
};

template <>
struct to_scheme_converter<double> {
  static ptr<>
  convert(context& ctx, double value) { return make<floating_point>(ctx, value); }
};

template <>
struct to_scheme_converter<bool> {
  static ptr<>
  convert(context& ctx, bool b) { return b ? ctx.constants->t.get() : ctx.constants->f.get(); }
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
      return ctx.constants->f.get();
  }
};

template <typename... Ts>
ptr<>
to_scheme_list(context& ctx, Ts&&... ts) {
  return make_list(ctx, to_scheme(ctx, ts)...);
}

} // namespace insider

#endif
