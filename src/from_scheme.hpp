#ifndef INSIDER_FROM_SCHEME_HPP
#define INSIDER_FROM_SCHEME_HPP

#include "basic_types.hpp"
#include "context.hpp"
#include "ptr.hpp"
#include "string.hpp"

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
struct from_scheme_converter<bool> {
  static bool
  convert(context& ctx, ptr<> o) {
    ptr<boolean> b = expect<boolean>(o);
    return b == ctx.constants->t.get();
  }
};

template <typename T>
struct from_scheme_converter<T, std::enable_if_t<std::is_integral_v<T>>> {
  static T
  convert(context&, ptr<> o) { return expect<integer>(o).value(); }
};

template <>
struct from_scheme_converter<std::string> {
  static std::string
  convert(context&, ptr<> o) { return expect<string>(o)->value(); }
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
