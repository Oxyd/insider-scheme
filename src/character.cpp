#include "character.hpp"

#include "basic_types.hpp"
#include "code_point_properties.hpp"
#include "context.hpp"
#include "integer.hpp"

namespace insider {

static bool
has_category(character c, std::uint32_t category) {
  if (auto prop = find_properties(c.value()))
    return has_category(*prop, category);
  else
    return false;
}

bool
is_numeric(character c) {
  return has_category(c, code_point_category::numeric);
}

ptr<>
digit_value(context& ctx, character c) {
  if (auto prop = find_properties(c.value()))
    if (has_category(*prop, code_point_category::numeric))
      return integer_to_ptr(integer{prop->digit_value});
  return ctx.constants->f.get();
}

bool
is_alphabetic(character c) {
  return has_category(c, code_point_category::alphabetic);
}

bool
is_upper_case(character c) {
  return has_category(c, code_point_category::upper_case);
}

bool
is_lower_case(character c) {
  return has_category(c, code_point_category::lower_case);
}

bool
is_white_space(character c) {
  return has_category(c, code_point_category::white_space);
}

template <auto Prop>
character
find_property_value(character c) {
  if (auto prop = find_properties(c.value()))
    if ((*prop).*Prop != 0)
      return character{(*prop).*Prop};
  return c;
}

character
upcase(character c) {
  return find_property_value<&code_point_properties::simple_uppercase>(c);
}

character
downcase(character c) {
  return find_property_value<&code_point_properties::simple_lowercase>(c);
}

character
foldcase(character c) {
  return find_property_value<&code_point_properties::simple_case_folding>(c);
}

} // namespace insider
