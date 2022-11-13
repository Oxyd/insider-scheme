#ifndef INSIDER_UTIL_INTEGER_CAST_HPP
#define INSIDER_UTIL_INTEGER_CAST_HPP

#include <cassert>
#include <concepts>
#include <limits>

namespace insider {

template <std::unsigned_integral To, std::signed_integral From>
To
to_unsigned(From from) {
  assert(from >= 0);
  if constexpr (sizeof(From) > sizeof(To))
    assert(from <= std::numeric_limits<To>::max());
  return static_cast<To>(from);
}

template <std::signed_integral To, std::unsigned_integral From>
To
to_signed(From from) {
  if constexpr (sizeof(From) >= sizeof(To))
    assert(from <= static_cast<From>(std::numeric_limits<To>::max()));
  return static_cast<To>(from);
}

template <std::unsigned_integral To, std::unsigned_integral From>
  requires(sizeof(To) < sizeof(From))
To
to_smaller(From from) {
  assert(from <= static_cast<From>(std::numeric_limits<To>::max()));
  return static_cast<To>(from);
}

} // namespace insider

#endif
