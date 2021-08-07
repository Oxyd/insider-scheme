#include "code_point_properties.hpp"

#include <array>

namespace insider {

#include "code_point_properties_table.inc"

std::optional<code_point_properties>
find_properties(char32_t c) {
  for (code_point_properties prop : code_points)
    if (prop.code_point == c)
      return prop;

  return std::nullopt;
}

} // namespace insider
