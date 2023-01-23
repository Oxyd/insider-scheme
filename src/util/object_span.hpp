#ifndef INSIDER_UTIL_OBJECT_SPAN_HPP
#define INSIDER_UTIL_OBJECT_SPAN_HPP

#include "object.hpp"
#include "object_conversions.hpp"

#include <fmt/format.h>

#include <span>
#include <stdexcept>

namespace insider {

using object_span = std::span<ptr<> const>;

inline void
require_arg_count(object_span args, std::size_t min) {
  if (args.size() < min)
    throw std::runtime_error{fmt::format(
      "Expected at least {} arguments, got {}",
      min, args.size()
    )};
}

inline void
require_arg_count(object_span args, std::size_t min, std::size_t max) {
  if (args.size() < min)
    throw std::runtime_error{fmt::format(
      "Expected at least {} arguments, got {}",
      min, args.size()
    )};
  if (args.size() > max)
    throw std::runtime_error{fmt::format(
      "Expected at most {} arguments, got {}",
      max, args.size()
    )};
}

template <typename T>
auto
require_arg(object_span args, std::size_t i) {
  if (args.size() <= i)
    throw std::runtime_error{fmt::format(
      "Expected at least {} arguments, got {}",
      i + 1, args.size()
    )};
  else
    return expect<T>(args[i]);
}

template <typename T>
auto
optional_arg(object_span args, std::size_t i, value_type_for_t<T> def) {
  if (args.size() <= i)
    return def;
  else
    return expect<T>(args[i]);
}

} // namespace insider

#endif
