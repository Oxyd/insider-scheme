#ifndef INSIDER_UTIL_SYMBOLIC_ENUM_HPP
#define INSIDER_UTIL_SYMBOLIC_ENUM_HPP

#include "runtime/symbol.hpp"
#include "util/from_scheme.hpp"
#include "util/join.hpp"
#include "util/to_scheme.hpp"

#include <format>
#include <ranges>
#include <stdexcept>
#include <tuple>

namespace insider {

template <typename E>
class symbolic_enum {
  using values = typename E::values;

public:
  explicit
  symbolic_enum(values v) : value_{v} { }

  values
  value() const { return value_; }

private:
  values value_;
};

template <typename E>
struct from_scheme_converter<symbolic_enum<E>> {
  static symbolic_enum<E>
  convert(context& ctx, ptr<> o) {
    auto name = expect<symbol>(o);

    for (auto [n, value] : E::mapping)
      if (name == ctx.intern(n))
        return symbolic_enum<E>{value};

    throw std::runtime_error{
      std::format(
        "Invalid value, expected one of {}",
        join(std::views::transform(E::mapping,
                                   [] (auto pair) {
                                     return std::get<0>(pair);
                                   }),
             ", ")
      )
    };
  }
};

template <typename E>
struct to_scheme_converter<symbolic_enum<E>> {
  static ptr<>
  convert(context& ctx, symbolic_enum<E> value) {
    for (auto [name, v] : E::mapping)
      if (value.value() == v)
        return ctx.intern(name);

    assert(false);
    throw std::logic_error{"Invalid enumerator"};
  }
};

} // namespace insider

#endif
