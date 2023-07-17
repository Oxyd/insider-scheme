#include "character.hpp"

#include "context.hpp"
#include "runtime/basic_types.hpp"
#include "runtime/code_point_properties.hpp"
#include "runtime/integer.hpp"
#include "util/define_procedure.hpp"

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

static integer
char_to_integer(char32_t c) {
  return integer{c};
}

static char32_t
integer_to_char(integer i) {
  return static_cast<char32_t>(i.value());
}

template <auto Compare>
static ptr<>
char_compare(context& ctx, object_span args) {
  if (args.size() < 2)
    throw std::runtime_error{"Expected at least 2 arguments"};

  char32_t lhs = expect<char32_t>(args[0]);
  for (std::size_t i = 1; i < args.size(); ++i) {
    char32_t rhs = expect<char32_t>(args[i]);
    if (!Compare(lhs, rhs))
      return ctx.constants->f;

    lhs = rhs;
  }

  return ctx.constants->t;
}

static constexpr auto char_eq = char_compare<[] (char32_t lhs, char32_t rhs) {
  return lhs == rhs;
}>;

static constexpr auto char_lt = char_compare<[] (char32_t lhs, char32_t rhs) {
  return lhs < rhs;
}>;

static constexpr auto char_le = char_compare<[] (char32_t lhs, char32_t rhs) {
  return lhs <= rhs;
}>;

static constexpr auto char_gt = char_compare<[] (char32_t lhs, char32_t rhs) {
  return (lhs > rhs); // Parentheses are a workaround for GCC bug.
}>;

static constexpr auto char_ge = char_compare<[] (char32_t lhs, char32_t rhs) {
  return lhs >= rhs;
}>;

template <auto Compare>
static ptr<>
char_compare_ci(context& ctx, object_span args) {
  if (args.size() < 2)
    throw std::runtime_error{"Expected at least 2 arguments"};

  char32_t lhs = char_foldcase(expect<char32_t>(args[0]));
  for (std::size_t i = 1; i < args.size(); ++i) {
    char32_t rhs = char_foldcase(expect<char32_t>(args[i]));
    if (!Compare(lhs, rhs))
      return ctx.constants->f;

    lhs = rhs;
  }

  return ctx.constants->t;
}

static constexpr auto char_ci_eq = char_compare_ci<
  [] (char32_t lhs, char32_t rhs) {
    return lhs == rhs;
  }
>;

static constexpr auto char_ci_lt = char_compare_ci<
  [] (char32_t lhs, char32_t rhs) {
    return lhs < rhs;
  }
>;

static constexpr auto char_ci_le = char_compare_ci<
  [] (char32_t lhs, char32_t rhs) {
    return lhs <= rhs;
  }
>;

static constexpr auto char_ci_gt = char_compare_ci<
  [] (char32_t lhs, char32_t rhs) {
    return (lhs > rhs); // Parentheses are a workaround for GCC bug.
  }
>;

static constexpr auto char_ci_ge = char_compare<
  [] (char32_t lhs, char32_t rhs) {
    return lhs >= rhs;
  }
>;

void
export_character(context& ctx, ptr<module_> result) {
  define_constant_evaluable_procedure<is_alphabetic>(ctx, "char-alphabetic?",
                                                     result);
  define_constant_evaluable_procedure<is_numeric>(ctx, "char-numeric?", result);
  define_constant_evaluable_procedure<is_white_space>(ctx, "char-whitespace?",
                                                      result);
  define_constant_evaluable_procedure<is_upper_case>(ctx, "char-upper-case?",
                                                     result);
  define_constant_evaluable_procedure<is_lower_case>(ctx, "char-lower-case?",
                                                     result);
  define_constant_evaluable_procedure<digit_value>(ctx, "digit-value", result);
  define_constant_evaluable_procedure<char_to_integer>(ctx, "char->integer",
                                                       result);
  define_constant_evaluable_procedure<integer_to_char>(ctx, "integer->char",
                                                       result);
  define_constant_evaluable_procedure<char_upcase>(ctx, "char-upcase", result);
  define_constant_evaluable_procedure<char_downcase>(ctx, "char-downcase",
                                                     result);
  define_constant_evaluable_procedure<char_foldcase>(ctx, "char-foldcase",
                                                     result);
  define_constant_evaluable_raw_procedure<char_eq>(ctx, "char=?", result);
  define_constant_evaluable_raw_procedure<char_lt>(ctx, "char<?", result);
  define_constant_evaluable_raw_procedure<char_le>(ctx, "char<=?", result);
  define_constant_evaluable_raw_procedure<char_gt>(ctx, "char>?", result);
  define_constant_evaluable_raw_procedure<char_ge>(ctx, "char>=?", result);
  define_constant_evaluable_raw_procedure<char_ci_eq>(ctx, "char-ci=?", result);
  define_constant_evaluable_raw_procedure<char_ci_lt>(ctx, "char-ci<?", result);
  define_constant_evaluable_raw_procedure<char_ci_le>(ctx, "char-ci<=?", result);
  define_constant_evaluable_raw_procedure<char_ci_gt>(ctx, "char-ci>?", result);
  define_constant_evaluable_raw_procedure<char_ci_ge>(ctx, "char-ci>=?", result);
}

} // namespace insider
