#ifndef INSIDER_CODE_POINT_PROPERTIES_HPP
#define INSIDER_CODE_POINT_PROPERTIES_HPP

#include <array>
#include <cstdint>
#include <optional>

namespace insider {

namespace code_point_attribute {
  static constexpr std::uint32_t numeric        = 1 << 0;
  static constexpr std::uint32_t lower_case     = 1 << 1;
  static constexpr std::uint32_t upper_case     = 1 << 2;
  static constexpr std::uint32_t alphabetic     = 1 << 3;
  static constexpr std::uint32_t white_space    = 1 << 4;
  static constexpr std::uint32_t cased_letter   = 1 << 5;
  static constexpr std::uint32_t case_ignorable = 1 << 6;
}

struct code_point_properties {
  char32_t        code_point;
  std::uint32_t   attributes;
  int             digit_value;
  char32_t        simple_uppercase;
  char32_t        simple_lowercase;
  char32_t        simple_case_folding;
  char32_t const* complex_uppercase;
};

inline bool
has_attribute(code_point_properties prop, std::uint32_t c) {
  return (prop.attributes & c) != 0;
}

std::optional<code_point_properties>
find_properties(char32_t);

std::size_t
codepoint_hash(char32_t);

#include "code_point_properties_forward.inc"

constexpr char32_t uppercase_sigma = 0x3a3;
constexpr char32_t lowercase_medial_sigma = 0x3c3;
constexpr char32_t lowercase_final_sigma = 0x3c2;

} // namespace insider

#endif
