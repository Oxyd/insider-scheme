#ifndef INSIDER_RUNTIME_INTEGER_HPP
#define INSIDER_RUNTIME_INTEGER_HPP

#include "object.hpp"

#include <bit>
#include <cstdint>
#include <type_traits>

namespace insider {

// A signed, fixed size integer.
class integer {
public:
  using value_type = std::int64_t;
  using representation_type = std::uint64_t;

  static constexpr std::size_t storage_width = 64;
  static constexpr std::size_t value_width = 63;
  static constexpr value_type max = (value_type{1} << (value_width - 1)) - 1;
  static constexpr value_type min = -max - 1;

  integer() = default;
  integer(value_type value) : value_{value} { }

  value_type
  value() const { return value_; }

  void
  set_value(value_type v) { value_ = v; }

private:
  value_type value_ = 0;
};

inline integer
ptr_to_integer(ptr<> x) {
  assert(is_fixnum(x));
  return integer{static_cast<integer::value_type>(tagged_payload(x)) >> 1};
}

inline ptr<>
integer_to_ptr(integer i) {
  assert(
    static_cast<word_type>(i.value()) >> (integer::storage_width - 2) == 0b00
    || static_cast<word_type>(i.value()) >> (integer::storage_width - 2) == 0b11
  );

  return immediate_to_ptr(static_cast<word_type>(i.value() << 1) | 1);
}

inline std::size_t
integer_hash(integer i) { return std::hash<integer::value_type>{}(i.value()); }

template <typename T>
bool
in_fixnum_range(T value) {
  if constexpr (std::is_signed_v<T>)
    return value >= integer::min && value <= integer::max;
  else
    return value <= integer::max;
}

inline std::uint64_t
integer_representation(integer i) {
  return std::bit_cast<integer::representation_type>(i.value());
}

} // namespace insider

#endif
