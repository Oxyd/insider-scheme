#include "character.hpp"

#include "basic_types.hpp"
#include "code_point_properties.hpp"
#include "context.hpp"
#include "integer.hpp"

namespace insider {

bool
is_numeric(character c) {
  if (auto prop = find_properties(c.value()))
    return prop->category == code_point_category::numeric;
  else
    return false;
}

ptr<>
digit_value(context& ctx, character c) {
  if (auto prop = find_properties(c.value()))
    if (prop->category == code_point_category::numeric)
      return integer_to_ptr(integer{prop->digit_value});
  return ctx.constants->f.get();
}

bool
is_alphabetic(character c) {
  if (auto prop = find_properties(c.value()))
    return prop->category == code_point_category::alphabetic;
  else
    return false;
}

} // namespace insider
