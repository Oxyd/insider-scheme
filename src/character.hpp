#ifndef INSIDER_CHARACTER_HPP
#define INSIDER_CHARACTER_HPP

#include "object.hpp"

#include <cstdint>
#include <vector>

namespace insider {

// Unicode codepoint
class character {
public:
  using value_type = char32_t;

  explicit
  character(value_type c) : value_{c} { }

  value_type
  value() const { return value_; }

  std::size_t
  hash() const { return std::hash<value_type>{}(value_); }

private:
  value_type value_;
};

inline character
ptr_to_character(ptr<> x) {
  assert(is_character(x));
  return character{static_cast<character::value_type>(tagged_payload(x)) >> 2};
}

inline ptr<>
character_to_ptr(character c) {
  return immediate_to_ptr((static_cast<word_type>(c.value()) << 2) | 0b10);
}

inline std::size_t
character_hash(character c) { return std::hash<character::value_type>{}(c.value()); }

bool
is_numeric(character);

ptr<>
digit_value(context&, character);

bool
is_alphabetic(character);

bool
is_upper_case(character);

bool
is_lower_case(character);

bool
is_white_space(character);

character
upcase(character);

character
downcase(character);

character
foldcase(character);

template <typename F>
void
to_utf8(character c, F const& f) {
  char32_t value = c.value();
  if (value <= 0x7F)
    f(static_cast<std::uint8_t>(value));
  else if (value < 0x7FF) {
    f(static_cast<std::uint8_t>((value >> 6) | 0b11000000));
    f(static_cast<std::uint8_t>((value & 0b111111) | 0b10000000));
  } else if (value < 0xFFFF) {
    f(static_cast<std::uint8_t>((value >> 12) | 0b11100000));
    f(static_cast<std::uint8_t>(((value >> 6) & 0b111111) | 0b10000000));
    f(static_cast<std::uint8_t>((value & 0b111111) | 0b10000000));
  } else if (value < 0x10FFFF) {
    f(static_cast<std::uint8_t>((value >> 18) | 0b11110000));
    f(static_cast<std::uint8_t>(((value >> 12) & 0b111111) | 0b10000000));
    f(static_cast<std::uint8_t>(((value >> 6) & 0b111111) | 0b10000000));
    f(static_cast<std::uint8_t>((value & 0b111111) | 0b10000000));
  } else
    throw std::runtime_error{"Invalid value for UTF-8"};
}

std::vector<std::uint8_t>
to_utf8_copy(character);

std::size_t
utf8_code_point_byte_length(std::uint8_t first_byte);

} // namespace insider

#endif
