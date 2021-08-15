#include "character.hpp"

#include "basic_types.hpp"
#include "code_point_properties.hpp"
#include "context.hpp"
#include "integer.hpp"

#include <fmt/format.h>

#include <stdexcept>

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

std::string
to_utf8_copy(character c) {
  std::string result;
  result.reserve(4);
  to_utf8(c, [&] (char x) { result.push_back(x); });
  return result;
}

std::size_t
utf8_code_point_byte_length(std::uint8_t first_byte) {
  if ((first_byte & 0b1000'0000) == 0)
    return 1;
  else if ((first_byte & 0b1110'0000) == 0b1100'0000)
    return 2;
  else if ((first_byte & 0b1111'0000) == 0b1110'0000)
    return 3;
  else if ((first_byte & 0b1111'1000) == 0b1111'0000)
    return 4;
  else
    throw std::runtime_error{fmt::format("Invalid initial byte in UTF-8 encoding: {}", first_byte)};
}

} // namespace insider
