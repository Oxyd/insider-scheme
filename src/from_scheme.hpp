#ifndef INSIDER_FROM_SCHEME_HPP
#define INSIDER_FROM_SCHEME_HPP

#include "basic_types.hpp"
#include "context.hpp"
#include "ptr.hpp"
#include "string.hpp"

#include <filesystem>

namespace insider {

template <typename T, typename Enable = void>
struct from_scheme_converter;

template <typename T>
auto
from_scheme(context& ctx, ptr<> o) {
  return from_scheme_converter<std::remove_cv_t<std::remove_reference_t<T>>>::convert(ctx, o);
}

template <typename T>
auto
from_scheme(context& ctx, tracked_ptr<> const& x) {
  return from_scheme<T>(ctx, x.get());
}

template <>
struct from_scheme_converter<ptr<>> {
  static ptr<>
  convert(context&, ptr<> o) { return o; }
};

template <>
struct from_scheme_converter<tracked_ptr<>> {
  static tracked_ptr<>
  convert(context& ctx, ptr<> o) { return track(ctx, o); }
};

template <typename T>
struct from_scheme_converter<ptr<T>> {
  static ptr<T>
  convert(context&, ptr<> o) { return expect<T>(o); }
};

template <typename T>
struct from_scheme_converter<tracked_ptr<T>> {
  static tracked_ptr<T>
  convert(context& ctx, ptr<> o) { return track(ctx, expect<T>(o)); }
};

template <>
struct from_scheme_converter<integer> {
  static integer
  convert(context&, ptr<> o) { return expect<integer>(o); }
};

template <>
struct from_scheme_converter<bool> {
  static bool
  convert(context& ctx, ptr<> o) {
    ptr<boolean> b = expect<boolean>(o);
    return b == ctx.constants->t.get();
  }
};

template <>
struct from_scheme_converter<char32_t> {
  static char32_t
  convert(context&, ptr<> o) {
    return expect<char32_t>(o);
  }
};

template <>
struct from_scheme_converter<char> {
  static char
  convert(context&, ptr<> o) {
    char32_t value = expect<char32_t>(o);
    if (value <= 0x7F)
      return static_cast<char>(value);
    else
      throw std::runtime_error{"Expected an ASCII character"};
  }
};

template <typename T>
struct from_scheme_converter<T, std::enable_if_t<std::is_integral_v<T>>> {
  static T
  convert(context&, ptr<> o) {
    integer::value_type value = expect<integer>(o).value();
    if (in_range(value))
      return static_cast<T>(value);
    else
      throw std::runtime_error{fmt::format("Expected integer between {} and {}",
                                           std::numeric_limits<T>::min(),
                                           std::numeric_limits<T>::max())};
  }

  static bool
  in_range(integer::value_type value) {
    if constexpr (std::is_signed_v<T>)
      return value >= std::numeric_limits<T>::min() && value <= std::numeric_limits<T>::max();
    else
      return value >= 0
             && static_cast<std::make_unsigned_t<integer::value_type>>(value)
                <= std::numeric_limits<T>::max();
  }
};

template <>
struct from_scheme_converter<std::string> {
  static std::string
  convert(context&, ptr<> o) { return expect<string>(o)->value(); }
};

template <>
struct from_scheme_converter<std::filesystem::path> {
  static std::filesystem::path
  convert(context&, ptr<> o) { return std::filesystem::path{expect<string>(o)->value()}; }
};

template <typename T>
struct from_scheme_converter<std::vector<T>> {
  static std::vector<T>
  convert(context& ctx, ptr<> o) {
    std::vector<T> result;

    if (auto v = match<vector>(o)) {
      result.reserve(v->size());
      for (std::size_t i = 0; i < v->size(); ++i)
        result.emplace_back(from_scheme<T>(ctx, v->ref(i)));
    }
    else if (is_list(o)) {
      ptr<> elem = o;
      while (elem != ctx.constants->null.get()) {
        auto p = assume<pair>(elem);
        result.emplace_back(from_scheme<T>(ctx, car(p)));
        elem = cdr(p);
      }
    }
    else
      throw std::runtime_error{"Expected vector or list"};

    return result;
  }
};

} // namespace insider

#endif
