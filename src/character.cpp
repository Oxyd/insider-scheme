#include "character.hpp"

#include "code_point_properties.hpp"

namespace insider {

bool
is_numeric(character c) {
  if (auto prop = find_properties(c.value()))
    return prop->category == code_point_category::numeric;
  else
    return false;
}

} // namespace insider
