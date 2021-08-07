#ifndef INSIDER_CODE_POINT_PROPERTIES_HPP
#define INSIDER_CODE_POINT_PROPERTIES_HPP

#include <array>
#include <cstdint>
#include <optional>

namespace insider {

namespace code_point_category {
  static constexpr std::uint32_t numeric    = 1 << 0;
  static constexpr std::uint32_t lower_case = 1 << 1;
  static constexpr std::uint32_t upper_case = 1 << 2;
  static constexpr std::uint32_t alphabetic = 1 << 3;
  static constexpr std::uint32_t white_space = 1 << 4;
}

struct code_point_properties {
  char32_t      code_point;
  std::uint32_t category;
  int           digit_value;
};

inline bool
has_category(code_point_properties prop, std::uint32_t c) {
  return (prop.category & c) != 0;
}

std::optional<code_point_properties>
find_properties(char32_t);

std::size_t
codepoint_hash(char32_t);

#include "code_point_properties_forward.inc"

} // namespace insider

#endif
