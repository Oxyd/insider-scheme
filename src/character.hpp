#ifndef INSIDER_CHARACTER_HPP
#define INSIDER_CHARACTER_HPP

#include "object.hpp"

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

} // namespace insider

#endif
