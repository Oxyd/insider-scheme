#ifndef INSIDER_CODE_POINT_PROPERTIES_HPP
#define INSIDER_CODE_POINT_PROPERTIES_HPP

#include <optional>

namespace insider {

enum class code_point_category {
  numeric = 0
};

struct code_point_properties {
  char32_t            code_point;
  code_point_category category;
};

std::optional<code_point_properties>
find_properties(char32_t);

} // namespace insider

#endif
