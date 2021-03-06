#include "character.hpp"

#include "context.hpp"
#include "runtime/basic_types.hpp"
#include "runtime/code_point_properties.hpp"
#include "runtime/integer.hpp"
#include "util/define_procedure.hpp"

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
  return ctx.constants->f;
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
char_upcase(char32_t c) {
  return find_property_value<&code_point_properties::simple_uppercase>(c);
}

char32_t
char_downcase(char32_t c) {
  return find_property_value<&code_point_properties::simple_lowercase>(c);
}

char32_t
char_foldcase(char32_t c) {
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

bool
is_initial_byte(char byte) {
  return (byte & 0b1100'0000) != 0b1000'0000;
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
    throw std::runtime_error{
      fmt::format("Invalid initial byte in UTF-8 encoding: {}",
                  static_cast<uint32_t>(first_byte))
    };
}

static integer
char_to_integer(char32_t c) {
  return integer{c};
}

static char32_t
integer_to_char(integer i) {
  return static_cast<char32_t>(i.value());
}

void
export_character(context& ctx, ptr<module_> result) {
  define_procedure<is_alphabetic>(ctx, "char-alphabetic?", result);
  define_procedure<is_numeric>(ctx, "char-numeric?", result);
  define_procedure<is_white_space>(ctx, "char-whitespace?", result);
  define_procedure<is_upper_case>(ctx, "char-upper-case?", result);
  define_procedure<is_lower_case>(ctx, "char-lower-case?", result);
  define_procedure<digit_value>(ctx, "digit-value", result);
  define_procedure<char_to_integer>(ctx, "char->integer", result);
  define_procedure<integer_to_char>(ctx, "integer->char", result);
  define_procedure<char_upcase>(ctx, "char-upcase", result);
  define_procedure<char_downcase>(ctx, "char-downcase", result);
  define_procedure<char_foldcase>(ctx, "char-foldcase", result);
}

} // namespace insider
