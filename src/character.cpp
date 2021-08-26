#include "character.hpp"

#include "basic_types.hpp"
#include "code_point_properties.hpp"
#include "context.hpp"
#include "integer.hpp"

#include <fmt/format.h>

#include <stdexcept>

namespace insider {

static bool
has_attribute(char32_t c, std::uint32_t attribute) {
  if (auto prop = find_properties(c))
    return has_attribute(*prop, attribute);
  else
    return false;
}

bool
is_numeric(char32_t c) {
  return has_attribute(c, code_point_attribute::numeric);
}

ptr<>
digit_value(context& ctx, char32_t c) {
  if (auto prop = find_properties(c))
    if (has_attribute(*prop, code_point_attribute::numeric))
      return integer_to_ptr(integer{prop->digit_value});
  return ctx.constants->f.get();
}

bool
is_alphabetic(char32_t c) {
  return has_attribute(c, code_point_attribute::alphabetic);
}

bool
is_upper_case(char32_t c) {
  return has_attribute(c, code_point_attribute::upper_case);
}

bool
is_lower_case(char32_t c) {
  return has_attribute(c, code_point_attribute::lower_case);
}

bool
is_white_space(char32_t c) {
  return has_attribute(c, code_point_attribute::white_space);
}

template <auto Prop>
char32_t
find_property_value(char32_t c) {
  if (auto prop = find_properties(c))
    if ((*prop).*Prop != 0)
      return (*prop).*Prop;
  return c;
}

char32_t
upcase(char32_t c) {
  return find_property_value<&code_point_properties::simple_uppercase>(c);
}

char32_t
downcase(char32_t c) {
  return find_property_value<&code_point_properties::simple_lowercase>(c);
}

char32_t
foldcase(char32_t c) {
  return find_property_value<&code_point_properties::simple_case_folding>(c);
}

std::size_t
utf32_code_point_byte_length(char32_t c) {
  if (c <= 0x7F)
    return 1;
  else if (c <= 0x7FF)
    return 2;
  else if (c <= 0xFFFF)
    return 3;
  else if (c <= 0x10FFFF)
    return 4;
  else
    throw std::runtime_error{"Invalid code point"};
}

std::string
to_utf8(char32_t c) {
  std::string result;
  result.reserve(4);
  to_utf8(c, [&] (char x) { result.push_back(x); });
  return result;
}

std::string
to_utf8(std::u32string const& s) {
  std::string result;
  result.reserve(s.length());
  for (char32_t c : s)
    to_utf8(c, [&] (char byte) { result.push_back(byte); });
  return result;
}

std::size_t
utf8_code_point_byte_length(char first_byte) {
  if ((first_byte & 0b1000'0000) == 0)
    return 1;
  else if ((first_byte & 0b1110'0000) == 0b1100'0000)
    return 2;
  else if ((first_byte & 0b1111'0000) == 0b1110'0000)
    return 3;
  else if ((first_byte & 0b1111'1000) == 0b1111'0000)
    return 4;
  else
    throw std::runtime_error{fmt::format("Invalid initial byte in UTF-8 encoding: {}",
                                         static_cast<uint32_t>(first_byte))};
}

} // namespace insider
