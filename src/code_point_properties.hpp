#ifndef INSIDER_CODE_POINT_PROPERTIES_HPP
#define INSIDER_CODE_POINT_PROPERTIES_HPP

#include <array>
#include <cstdint>
#include <optional>

namespace insider {

enum class code_point_category : std::uint32_t {
  numeric = 0,
  alphabetic = 1
};

struct code_point_properties {
  char32_t            code_point;
  code_point_category category;
  int                 digit_value;
};

std::optional<code_point_properties>
find_properties(char32_t);

std::size_t
codepoint_hash(char32_t);

#include "code_point_properties_forward.inc"

} // namespace insider

#endif
