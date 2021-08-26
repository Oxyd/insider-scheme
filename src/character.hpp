#ifndef INSIDER_CHARACTER_HPP
#define INSIDER_CHARACTER_HPP

#include "object.hpp"

#include <cassert>
#include <cstdint>
#include <vector>

namespace insider {

// Unicode codepoint
class character {
public:
  using value_type = char32_t;

  explicit
  character(value_type c) : value_{c} { }

  explicit
  character(char c) : value_{static_cast<value_type>(c)} { }

  explicit
  character(int value) : value_{static_cast<value_type>(value)} { }

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
  return character{static_cast<character::value_type>(tagged_payload(x) >> 2)};
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

// The length in bytes of a code-point after conversion to UTF-8.
std::size_t
utf32_code_point_byte_length(char32_t);

template <typename F>
void
to_utf8(character c, F&& f) {
  char32_t value = c.value();
  if (value <= 0x7F)
    f(static_cast<char>(value));
  else if (value < 0x7FF) {
    f(static_cast<char>((value >> 6) | 0b11000000));
    f(static_cast<char>((value & 0b111111) | 0b10000000));
  } else if (value < 0xFFFF) {
    f(static_cast<char>((value >> 12) | 0b11100000));
    f(static_cast<char>(((value >> 6) & 0b111111) | 0b10000000));
    f(static_cast<char>((value & 0b111111) | 0b10000000));
  } else if (value < 0x10FFFF) {
    f(static_cast<char>((value >> 18) | 0b11110000));
    f(static_cast<char>(((value >> 12) & 0b111111) | 0b10000000));
    f(static_cast<char>(((value >> 6) & 0b111111) | 0b10000000));
    f(static_cast<char>((value & 0b111111) | 0b10000000));
  } else
    throw std::runtime_error{"Invalid value for UTF-8"};
}

std::string
to_utf8(character);

std::string
to_utf8(std::u32string const&);

std::size_t
utf8_code_point_byte_length(char first_byte);

struct from_utf8_result {
  char32_t    code_point;
  std::size_t length;
};

template <typename It>
from_utf8_result
from_utf8(It begin, It end) {
  assert(begin != end);

  std::size_t length = utf8_code_point_byte_length(*begin);
  if (static_cast<std::size_t>(std::distance(begin, end)) < length)
    throw std::runtime_error{"Premature end of UTF-8 stream"};

  if (length == 1)
    return {static_cast<char32_t>(*begin), 1};
  else if (length == 2)
    return {static_cast<char32_t>(((*begin & 0b0001'1111) << 6)
                                  | (*(begin + 1) & 0b0011'1111)),
            2};
  else if (length == 3)
    return {static_cast<char32_t>(((*begin & 0b0000'1111) << 12)
                                  | ((*(begin + 1) & 0b0011'1111) << 6)
                                  | (*(begin + 2) & 0b0011'1111)),
            3};
  else if (length == 4)
    return {static_cast<char32_t>(((*begin & 0b0000'0111) << 18)
                                  | ((*(begin + 1) & 0b0011'1111) << 12)
                                  | ((*(begin + 2) & 0b0011'1111) << 6)
                                  | (*(begin + 3) & 0b0011'1111)),
            4};
  else {
    assert(false);
    return {};
  }
}

inline from_utf8_result
from_utf8(std::string_view sv) {
  return from_utf8(sv.begin(), sv.end());
}

} // namespace insider

#endif
