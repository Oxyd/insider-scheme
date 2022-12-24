#include "read.hpp"

#include "compiler/source_location.hpp"
#include "io/char_categories.hpp"
#include "io/port.hpp"
#include "io/reader_stream.hpp"
#include "runtime/basic_types.hpp"
#include "runtime/character.hpp"
#include "runtime/numeric.hpp"
#include "runtime/string.hpp"
#include "runtime/syntax.hpp"
#include "util/define_procedure.hpp"
#include "vm/vm.hpp"

#include <fmt/format.h>

#include <variant>

namespace insider {

namespace {
  struct end { };
  struct left_paren { };
  struct right_paren { };
  struct octothorpe_left_paren { };
  struct octothorpe_u8 { };
  struct dot { };

  struct generic_literal {
    ptr<> value;
  };

  struct boolean_literal {
    bool value;
  };

  struct void_literal { };

  struct default_value_literal { };

  struct identifier {
    std::string value;
  };

  struct keyword_literal {
    std::string value;
  };

  struct quote { };
  struct backquote { };
  struct comma { };
  struct comma_at { };
  struct octothorpe_quote { };
  struct octothorpe_backquote { };
  struct octothorpe_comma { };
  struct octothorpe_comma_at { };

  struct datum_label_definition {
    std::string label;
  };

  struct datum_label_reference {
    std::string label;
  };

  struct token {
    using value_type = std::variant<
      end,
      left_paren,
      right_paren,
      octothorpe_left_paren,
      octothorpe_u8,
      dot,
      generic_literal,
      boolean_literal,
      void_literal,
      default_value_literal,
      identifier,
      keyword_literal,
      quote,
      backquote,
      comma,
      comma_at,
      octothorpe_quote,
      octothorpe_backquote,
      octothorpe_comma,
      octothorpe_comma_at,
      datum_label_definition,
      datum_label_reference
    >;

    value_type      value;
    source_location location;
  };
} // anonymous namespace

read_error::read_error(std::string const& message, source_location const& loc)
  : translatable_runtime_error{fmt::format("{}: Read error: {}", format_location(loc), message)}
{ }

ptr<>
read_error::translate(context& ctx) const {
  return make<scheme_error>(ctx, what());
}


static bool
intraline_whitespace(char32_t c) {
  return c == ' ' || c == '\t';
}

static void
skip_whitespace(reader_stream& stream) {
  std::optional<char32_t> c = stream.peek();

  while (c && (whitespace(*c) || *c == ';')) {
    while (c && whitespace(*c))
      c = advance_and_peek(stream);

    if (c == U';')
      while ((c = stream.read()) && *c != '\n')
        ;

    c = stream.peek();
  }
}

static void
skip_intraline_whitespace(reader_stream& stream) {
  std::optional<char32_t> c = stream.peek();
  while (c && intraline_whitespace(*c))
    c = advance_and_peek(stream);
}

static std::u32string
read_until_delimiter(reader_stream& stream) {
  std::u32string result;
  while (stream.peek() && !delimiter(*stream.peek()))
    result += *stream.read();

  return result;
}

static bool
digit(char32_t c, unsigned base = 10) {
  switch (base) {
  case 2: return c >= '0' && c <= '1';
  case 8: return c >= '0' && c <= '7';
  case 10: return c >= '0' && c <= '9';
  case 16: return (c >= '0' && c <= '9')
                  || (c >= 'a' && c <= 'f')
                  || (c >= 'A' && c <= 'F');
  }

  assert(false);
  return false;
}

static char32_t
require_char(reader_stream& stream) {
  std::optional<char32_t> result = stream.read();
  if (!result)
    throw read_error{"Unexpected end of input", stream.location()};
  return *result;
}

static void
consume(reader_stream& stream, [[maybe_unused]] char32_t expected) {
  [[maybe_unused]] char32_t c = require_char(stream);
  assert(c == expected);
}

static void
expect(reader_stream& stream, char32_t expected) {
  source_location loc = stream.location();
  char32_t c = require_char(stream);
  if (c != expected)
    throw read_error{fmt::format("Unexpected character: {}, expected {}", to_utf8(c), to_utf8(expected)), loc};
}

static void
expect_either(reader_stream& stream, char32_t expected1, char32_t expected2) {
  source_location loc = stream.location();
  char32_t c = require_char(stream);
  if (c != expected1 && c != expected2)
    throw read_error{fmt::format("Unexpected character: {}, expected {} or {}",
                                 to_utf8(c), to_utf8(expected1), to_utf8(expected2)),
                     loc};
}

namespace {
  struct number_parse_mode {
    enum class exactness_mode {
      no_change,
      make_inexact,
      make_exact
    };

    unsigned base = 10;
    exactness_mode exactness = exactness_mode::no_change;
  };
}

static std::string
read_digits(reader_stream& stream, unsigned base) {
  std::optional<char32_t> c = stream.peek();

  std::string result;
  while (c && digit(*c, base)) {
    result += static_cast<char>(*c);
    c = advance_and_peek(stream);
  }

  return result;
}

static char
read_sign(reader_stream& stream) {
  auto cp = stream.make_checkpoint();
  auto c = stream.read();
  if (c == U'+' || c == U'-') {
    cp.commit();
    return static_cast<char>(*c);
  } else
    return '+';
}

static ptr<>
read_fraction(context& ctx, number_parse_mode mode,
              std::string const& numerator_digits, reader_stream& stream,
              source_location const& loc) {
  std::string denominator_digits = read_digits(stream, mode.base);
  if (denominator_digits.empty())
    throw read_error{"Invalid fraction literal", loc};

  return normalize_fraction(ctx, make<fraction>(ctx,
                                                read_integer(ctx, numerator_digits, mode.base),
                                                read_integer(ctx, denominator_digits, mode.base)));
}

static bool
can_begin_decimal_suffix(reader_stream& stream) {
  return stream.peek() == U'e' || stream.peek() == U'E';
}

static std::string
read_suffix(reader_stream& stream, source_location const& loc) {
  // <suffix> -> <empty>
  //           | e <sign> <digit 10>+
  //
  // <sign> -> <empty> | + | -

  using namespace std::literals;

  if (can_begin_decimal_suffix(stream)) {
    stream.read();
    std::string result = "e"s;
    result += read_sign(stream);
    result += read_digits(stream, 10);
    if (result.size() == 2)
      throw read_error{"Invalid numeric suffix", loc};
    return result;
  } else
    return ""s;
}

static double
string_to_double(std::string const& s, source_location const& loc) {
  // XXX: This would ideally use std::from_chars. But, as of 2020, GCC does not
  // implement that, violating C++17. Alternatives include std::stod and
  // std::atof, but those parse the string according to the currently installed
  // global locale. Many things could be said about std::istringstream, but at
  // least it can be imbued with a locale without messing with the global
  // locale. Thus, that's what we're going with.

  std::istringstream is{s};
  is.imbue(std::locale{"C"});

  double result;
  is >> result;

  if (!is || is.peek() != std::istringstream::traits_type::eof())
    throw read_error{fmt::format("Invalid floating point literal: {}", s), loc};

  return result;
}

static ptr<>
string_to_floating_point(context& ctx, std::string const& s,
                         source_location const& loc) {
  return make<floating_point>(ctx, string_to_double(s, loc));
}

static void
check_mode_for_decimal(number_parse_mode mode, source_location const& loc) {
  if (mode.base != 10)
    throw read_error{"Decimal number literals can only use base 10", loc};
}

static int
suffix_to_exponent(context& ctx, std::string const& suffix) {
  if (!suffix.empty()) {
    assert(suffix.length() > 2);
    assert(suffix[0] == 'e' || suffix[0] == 'E');
    assert(suffix[1] == '+' || suffix[1] == '-');

    int sign = suffix[1] == '+' ? 1 : -1;
    return static_cast<int>(sign * expect<integer>(read_integer(ctx, suffix.substr(2))).value());
  } else
    return 0;
}

static ptr<>
string_to_exact(context& ctx,
                std::string const& whole_part,
                std::string const& fractional_part,
                std::string const& suffix) {
  ptr<> numerator = read_integer(ctx, whole_part + fractional_part);
  int exponent = -static_cast<int>(fractional_part.length());
  exponent += suffix_to_exponent(ctx, suffix);

  if (exponent > 0)
    return multiply(ctx, numerator, integer_power<10>(ctx, exponent));
  else if (exponent < 0)
    return divide(ctx, numerator, integer_power<10>(ctx, -exponent));
  else
    return numerator;
}

static ptr<>
string_to_decimal(context& ctx, number_parse_mode mode,
                  std::string const& whole_part,
                  std::string const& fractional_part,
                  std::string const& suffix,
                  source_location const& loc) {
  using namespace std::literals;

  if (mode.exactness != number_parse_mode::exactness_mode::make_exact)
    return string_to_floating_point(
      ctx, whole_part + "."s + fractional_part + suffix, loc
    );
  else
    return string_to_exact(ctx, whole_part, fractional_part, suffix);
}

static ptr<>
read_decimal(context& ctx, number_parse_mode mode,
             std::string const& whole_part, reader_stream& stream,
             source_location const& loc) {
  check_mode_for_decimal(mode, loc);

  consume(stream, '.');
  std::string fractional_part = read_digits(stream, 10);

  if (whole_part.empty() && fractional_part.empty())
    return {};

  return string_to_decimal(ctx, mode, whole_part, fractional_part,
                           read_suffix(stream, loc),
                           loc);
}

static ptr<>
read_ureal(context& ctx, number_parse_mode mode, reader_stream& stream,
           source_location const& loc) {
  // <ureal R> -> <uinteger R>
  //            | <uinteger R> / <uinteger R>
  //            | <decimal R>
  //
  // <decimal 10> -> <uinteger 10> <suffix>
  //               | . <digit 10>+ <suffix>
  //               | <digit 10>+ . <digit 10>* <suffix>
  //
  // <uinteger R> -> <digit R>+

  using namespace std::literals;

  if (std::string digits = read_digits(stream, mode.base); !digits.empty()) {
    auto next = stream.peek();
    if (next == U'/') {
      consume(stream, '/');
      return read_fraction(ctx, mode, digits, stream, loc);
    } else if (next == U'.')
      return read_decimal(ctx, mode, digits, stream, loc);
    else if (can_begin_decimal_suffix(stream)) {
      check_mode_for_decimal(mode, loc);
      return string_to_decimal(ctx, mode, digits, ""s,
                               read_suffix(stream, loc), loc);
    } else
      return read_integer(ctx, digits, mode.base);
  } else if (stream.peek() == U'.')
    return read_decimal(ctx, mode, ""s, stream, loc);
  else
    return {};
}

static ptr<>
read_signed_real(context& ctx, number_parse_mode mode, reader_stream& stream,
                 source_location const& loc) {
  // <sign> <ureal R>

  auto cp = stream.make_checkpoint();
  char sign = read_sign(stream);
  if (ptr<> magnitude = read_ureal(ctx, mode, stream, loc)) {
    cp.commit();

    if (sign == '-')
      return multiply(ctx, magnitude, integer_to_ptr(-1));
    else
      return magnitude;
  }

  return {};
}

static bool
match_sequence_ci(reader_stream& stream, std::string const& expected) {
  auto cp = stream.make_checkpoint();
  std::locale c_locale{"C"};

  for (char c : expected) {
    auto actual = stream.read();
    if (!actual || std::tolower(static_cast<char>(*actual), c_locale) != c)
      return false;
  }

  cp.commit();
  return true;
}

static ptr<>
read_infnan(context& ctx, reader_stream& stream) {
  if (match_sequence_ci(stream, "+inf.0"))
    return make<floating_point>(ctx, floating_point::positive_infinity);
  else if (match_sequence_ci(stream, "-inf.0"))
    return make<floating_point>(ctx, floating_point::negative_infinity);
  else if (match_sequence_ci(stream, "+nan.0"))
    return make<floating_point>(ctx, floating_point::positive_nan);
  else if (match_sequence_ci(stream, "-nan.0"))
    return make<floating_point>(ctx, floating_point::negative_nan);
  else
    return {};
}

static ptr<>
read_real_preserve_exactness(context& ctx, number_parse_mode mode,
                             reader_stream& stream,
                             source_location const& loc) {
  // <real R> -> <sign> <ureal R>
  //           | <infnan>

  if (ptr<> result = read_signed_real(ctx, mode, stream, loc))
    return result;
  else
    return read_infnan(ctx, stream);
}

static void
throw_if_exact_non_rational(ptr<> value, number_parse_mode mode,
                            source_location const& loc) {
  if (mode.exactness != number_parse_mode::exactness_mode::make_exact)
    return;

  if (is_nan(value))
    throw read_error{"Can't make exact NaN", loc};
  else if (is_infinite(value))
    throw read_error{"Can't make exact infinity", loc};
}

static ptr<>
change_exactness(context& ctx, number_parse_mode mode, ptr<> value,
                 source_location const& loc) {
  if (mode.exactness == number_parse_mode::exactness_mode::make_inexact)
    return inexact(ctx, value);
  else {
    throw_if_exact_non_rational(value, mode, loc);
    return value;
  }
}

static ptr<>
read_real(context& ctx, number_parse_mode mode, reader_stream& stream,
          source_location const& loc) {
  if (ptr<> result = read_real_preserve_exactness(ctx, mode, stream, loc))
    return change_exactness(ctx, mode, result, loc);
  else
    return {};
}

static ptr<>
make_integer_constant(context& ctx, number_parse_mode mode,
                      integer::value_type value) {
  if (mode.exactness == number_parse_mode::exactness_mode::make_inexact)
    return inexact(ctx, integer_to_ptr(value));
  else
    return integer_to_ptr(value);
}

static ptr<>
read_imaginary_part(context& ctx, number_parse_mode mode, ptr<> real,
                    reader_stream& stream, source_location const& loc) {
  if (auto imag = read_real(ctx, mode, stream, loc)) {
    expect_either(stream, 'i', 'I');
    return make_rectangular(ctx, real, imag);
  } else {
    auto sign = stream.read();
    assert(sign == U'+' || sign == U'-');

    expect_either(stream, 'i', 'I');
    return make_rectangular(ctx, real,
                            sign == U'+'
                            ? make_integer_constant(ctx, mode, 1)
                            : make_integer_constant(ctx, mode, -1));
  }
}

static ptr<>
read_angle(context& ctx, number_parse_mode mode, ptr<> magnitude,
           reader_stream& stream, source_location const& loc) {
  consume(stream, '@');
  if (ptr<> angle = read_real(ctx, mode, stream, loc)) {
    if (is_exactly_equal_to(angle, 0))
      return magnitude;
    else
      return make_polar(ctx, magnitude, angle);
  } else
    throw read_error{"Invalid polar complex literal", loc};
}

static ptr<>
read_complex_after_first_part(context& ctx, number_parse_mode mode, ptr<> real,
                             reader_stream& stream, source_location const& loc) {
  if (stream.peek() == U'+' || stream.peek() == U'-')
    return read_imaginary_part(ctx, mode, real, stream, loc);
  else if (stream.peek() == U'@')
    return read_angle(ctx, mode, real, stream, loc);
  else if (stream.peek() == U'i' || stream.peek() == U'I') {
    stream.read();
    return make_rectangular(ctx, make_integer_constant(ctx, mode, 0), real);
  } else
    return real;
}

static ptr<>
read_sign_followed_by_imaginary_unit(context& ctx, number_parse_mode mode,
                                     reader_stream& stream) {
  auto cp = stream.make_checkpoint();

  auto sign = stream.read();
  assert(sign == U'+' || sign == U'-');

  auto imaginary_unit = stream.read();
  if (imaginary_unit == U'i' || imaginary_unit == U'I') {
    cp.commit();
    return make_rectangular(ctx, make_integer_constant(ctx, mode, 0),
                            sign == U'+'
                            ? make_integer_constant(ctx, mode, 1)
                            : make_integer_constant(ctx, mode, -1));
  } else
    return {};
}

static ptr<>
read_complex(context& ctx, number_parse_mode mode, reader_stream& stream,
             source_location const& loc) {
  // <complex R> -> <real R>
  //              | <real R> @ <real R>
  //              | <real R> + <ureal R> i | <real R> - <ureal R> i
  //              | <real R> + i | <real R> - i | <real R> <infnan> i
  //              | + <ureal R> i | - <ureal R> i
  //              | <infnan> i | +i | -i

  if (auto x = read_real(ctx, mode, stream, loc))
    return read_complex_after_first_part(ctx, mode, x, stream, loc);
  else if (stream.peek() == U'+' || stream.peek() == U'-')
    return read_sign_followed_by_imaginary_unit(ctx, mode, stream);
  else
    return {};
}

static std::optional<unsigned>
read_radix(reader_stream& stream) {
  // <radix 2> -> #b
  // <radix 8> -> #o
  // <radix 10> -> <empty> | #d
  // <radix 16> -> #x

  auto cp = stream.make_checkpoint();

  if (stream.read() != U'#')
    return {};

  auto r = stream.read();
  if (r != U'b' && r != U'o' && r != U'd' && r != U'x')
    return {};

  cp.commit();
  switch (*r) {
  case U'b': return 2;
  case U'o': return 8;
  case U'd': return 10;
  case U'x': return 16;
  }

  assert(false);
  return {};
}

static std::optional<number_parse_mode::exactness_mode>
read_exactness(reader_stream& stream) {
  // <exactness> -> <empty> | #i | #e

  auto cp = stream.make_checkpoint();

  if (stream.read() != U'#')
    return {};

  auto e = stream.read();
  if (e != U'i' && e != U'e')
    return {};

  cp.commit();
  switch (*e) {
  case U'i':
    return number_parse_mode::exactness_mode::make_inexact;
  case U'e':
    return number_parse_mode::exactness_mode::make_exact;
  }

  assert(false);
  return {};
}

static number_parse_mode
read_number_prefix(reader_stream& stream, unsigned default_radix) {
  // <prefix R> -> <radix R> <exactness>
  //             | <exactness> <radix R>

  if (auto radix = read_radix(stream)) {
    auto exactness = read_exactness(stream);
    return {*radix,
            exactness
              ? *exactness
              : number_parse_mode::exactness_mode::no_change};
  } else if (auto exactness = read_exactness(stream)) {
    auto radix = read_radix(stream);
    return {radix ? *radix : default_radix, *exactness};
  } else
    return number_parse_mode{default_radix,
                             number_parse_mode::exactness_mode::no_change};
}

static void
throw_if_no_delimiter_after_number(reader_stream& stream,
                                   source_location const& loc) {
  auto c = stream.peek();
  if (c && !delimiter(*c))
    throw read_error{fmt::format("{} unexpected following a numeric literal",
                                 to_utf8(*c)), loc};
}

static ptr<>
read_number(context& ctx, reader_stream& stream, source_location const& loc,
            unsigned default_base = 10) {
  // <num R> -> <prefix R> <complex R>

  if (auto result = read_complex(ctx, read_number_prefix(stream, default_base),
                                 stream, loc)) {
    throw_if_no_delimiter_after_number(stream, loc);
    return result;
  } else
    return {};
}

static std::optional<token>
read_numeric_literal(context& ctx, reader_stream& stream) {
  source_location loc = stream.location();
  auto cp = stream.make_checkpoint();
  if (ptr<> value = read_number(ctx, stream, loc)) {
    cp.commit();
    return token{generic_literal{value}, loc};
  } else
    return std::nullopt;
}

static std::u32string
read_hexdigits(reader_stream& stream) {
  std::u32string result;
  while (stream.peek() && digit(*stream.peek(), 16))
    result += *stream.read();
  return result;
}

static char32_t
read_character_from_hexdigits(context& ctx, std::u32string const& digits) {
  auto value = expect<integer>(read_integer(ctx, to_utf8(digits), 16));
  return static_cast<char32_t>(value.value());
}

static token
read_character(context& ctx, reader_stream& stream) {
  static std::unordered_map<std::u32string, char32_t> const character_names{
    {U"alarm",     '\x07'},
    {U"backspace", '\x08'},
    {U"delete",    '\x7F'},
    {U"escape",    '\x1B'},
    {U"newline",   '\x0A'},
    {U"null",      '\x00'},
    {U"return",    '\x0D'},
    {U"space",     ' '},
    {U"tab",       '\x09'}
  };

  std::optional<char32_t> c = stream.read();
  if (!c)
    throw read_error{"Unexpected end of input", stream.location()};

  if (*c != 'x') {
    source_location loc = stream.location();
    if (!is_alphabetic(*c))
      return {generic_literal{character_to_ptr(*c)}, loc};

    std::u32string literal = *c + read_until_delimiter(stream);
    if (literal.size() == 1)
      return {generic_literal{character_to_ptr(literal[0])}, loc};
    else {
      if (stream.fold_case)
        literal = string_foldcase(literal);

      if (auto it = character_names.find(literal); it != character_names.end())
        return {generic_literal{character_to_ptr(it->second)}, loc};
      else
        throw read_error{fmt::format("Unknown character literal #\\{}",
                                     to_utf8(literal)), loc};
    }
  }
  else {
    source_location loc = stream.location();
    std::u32string digits = read_hexdigits(stream);

    if (!digits.empty())
      return {
        generic_literal{
          character_to_ptr(read_character_from_hexdigits(ctx, digits))
        },
        loc
      };
    else
      return {generic_literal{character_to_ptr('x')}, loc};
  }
}

static token
read_special_literal(context& ctx, reader_stream& stream) {
  std::optional<char32_t> c = stream.peek();
  if (!c)
    throw read_error{"Unexpected end of input", stream.location()};

  if (*c == '\\') {
    stream.read();
    return read_character(ctx, stream);
  }

  source_location loc = stream.location();
  std::u32string literal = read_until_delimiter(stream);

  if (literal == U"t" || literal == U"true")
    return {boolean_literal{true}, loc};
  else if (literal == U"f" || literal == U"false")
    return {boolean_literal{false}, loc};
  else if (literal == U"void")
    return {void_literal{}, loc};
  else if (literal == U"default-value")
    return {default_value_literal{}, loc};
  else
    throw read_error{fmt::format("Invalid literal: {}", to_utf8(literal)),
                     loc};
}

static std::string
read_identifier_contents(reader_stream& stream) {
  std::u32string value = read_until_delimiter(stream);

  if (stream.fold_case)
    value = string_foldcase(value);

  return to_utf8(value);
}

static token
read_identifier(reader_stream& stream) {
  source_location loc = stream.location();
  return {identifier{read_identifier_contents(stream)}, loc};
}

static char32_t
read_hex_escape(context& ctx, reader_stream& stream) {
  std::u32string digits = read_hexdigits(stream);
  expect(stream, ';');
  return read_character_from_hexdigits(ctx, digits);
}

static char32_t
read_common_escape(context& ctx, reader_stream& stream) {
  source_location loc = stream.location();
  char32_t escape = require_char(stream);

  switch (escape) {
  case 'a': return '\a';
  case 'b': return '\b';
  case 'n': return '\n';
  case 'r': return '\r';
  case 't': return '\t';
  case '"': return '"';
  case '\\': return '\\';
  case '|': return '|';
  case 'x': return read_hex_escape(ctx, stream);
  default:
    throw read_error{fmt::format("Unrecognised escape sequence \\{}",
                                 to_utf8(escape)), loc};
  }
}

static std::optional<char32_t>
read_string_escape(context& ctx, reader_stream& stream) {
  source_location loc = stream.location();
  auto cp = stream.make_checkpoint();
  char32_t escape = require_char(stream);
  if (escape == '\n') {
    cp.commit();

    skip_intraline_whitespace(stream);
    return {};
  } else if (intraline_whitespace(escape)) {
    cp.commit();

    skip_intraline_whitespace(stream);
    if (std::optional<char32_t> after_whitespace = stream.read())
      if (*after_whitespace == '\n') {
        skip_intraline_whitespace(stream);
        return {};
      }

    throw read_error{fmt::format("Unrecognised escape sequence \\{}",
                                 to_utf8(escape)), loc};
  } else {
    cp.revert();
    return read_common_escape(ctx, stream);
  }
}

static std::string
read_verbatim_identifier_contents(context& ctx, reader_stream& stream) {
  // The opening | was consumed before calling this function.

  std::u32string value;

  std::optional<char32_t> c = stream.read();
  while (c && *c != '|') {
    if (*c == '\\')
      value += read_common_escape(ctx, stream);
    else
      value += *c;

    c = stream.read();
  }

  if (!c)
    throw read_error{"Unexpected end of input", stream.location()};

  return to_utf8(value);
}

static token
read_verbatim_identifier(context& ctx, reader_stream& stream) {
  source_location loc = stream.location();
  return {identifier{read_verbatim_identifier_contents(ctx, stream)}, loc};
}

static token
read_string_literal(context& ctx, reader_stream& stream) {
  // The opening " was consumed before calling this function.

  source_location loc = stream.location();

  std::string result;
  while (true) {
    char32_t c = require_char(stream);
    if (c == '"')
      break;

    if (c == '\\') {
      if (auto escape = read_string_escape(ctx, stream))
        to_utf8(*escape, [&] (char byte) { result.push_back(byte); });
    } else
      to_utf8(c, [&] (char byte) { result.push_back(byte); });
  }

  return {generic_literal{make<string>(ctx, std::move(result))}, loc};
}

static token
read_token_after_comma(reader_stream& stream, source_location const& loc) {
  std::optional<char32_t> c = stream.peek();
  if (c && *c == '@') {
    stream.read();
    return {comma_at{}, loc};
  }
  else
    return {comma{}, loc};
}

static token
read_token_after_period(reader_stream& stream) {
  source_location loc = stream.location();
  auto cp = stream.make_checkpoint();
  expect(stream, '.');

  std::optional<char32_t> c = stream.peek();
  if (!c)
    throw read_error{"Unexpected end of input", stream.location()};

  if (delimiter(*c)) {
    cp.commit();
    return {dot{}, loc};
  } else {
    cp.revert();
    return read_identifier(stream);
  }
}

static std::string
read_datum_label_value(reader_stream& stream) {
  std::string result;

  std::optional<char32_t> c = stream.peek();
  while (c && digit(*c)) {
    to_utf8(*stream.read(), [&] (char byte) { result.push_back(byte); });
    c = stream.peek();
  }

  return result;
}

static token
read_datum_label(reader_stream& stream, source_location const& loc) {
  std::string label = read_datum_label_value(stream);

  std::optional<char32_t> c = stream.read();
  if (!c)
    throw read_error{"Unexpected end of input", stream.location()};

  if (*c == '=')
    return {datum_label_definition{std::move(label)}, loc};
  else if (*c == '#')
    return {datum_label_reference{std::move(label)}, loc};
  else
    throw read_error{"Unexpected character after datum label",
                     stream.location()};
}

static token
read_token(context& ctx, reader_stream& stream);

using datum_labels = std::unordered_map<std::string, ptr<>>;

static ptr<>
read(context& ctx, token first_token, reader_stream& stream,
     bool read_syntax, datum_labels& labels,
     std::optional<std::string> const& defining_label = {});

static token
read_datum_comment(context& ctx, reader_stream& stream) {
  consume(stream, ';');
  datum_labels labels;
  read(ctx, read_token(ctx, stream), stream, false, labels); // Discard

  return read_token(ctx, stream);
}

static token
read_block_comment(context& ctx, reader_stream& stream) {
  consume(stream, '|');

  unsigned nesting_level = 1;
  while (nesting_level > 0) {
    char32_t c = require_char(stream);
    if (c == '|') {
      char32_t next = require_char(stream);
      if (next == '#')
        --nesting_level;
    } else if (c == '#') {
      char32_t next = require_char(stream);
      if (next == '|')
        ++nesting_level;
    }
  }

  return read_token(ctx, stream);
}

static token
read_directive(context& ctx, reader_stream& stream) {
  consume(stream, '!');

  source_location loc = stream.location();
  std::u32string directive = read_until_delimiter(stream);
  if (directive == U"fold-case")
    stream.fold_case = true;
  else if (directive == U"no-fold-case")
    stream.fold_case = false;
  else
    throw read_error{fmt::format("Invalid directive: {}", to_utf8(directive)),
                     loc};

  return read_token(ctx, stream);
}

static token
read_verbatim_keyword(context& ctx, reader_stream& stream,
                      source_location const& loc) {
  consume(stream, '|');
  return {keyword_literal{read_verbatim_identifier_contents(ctx, stream)}, loc};
}

static token
read_keyword(context& ctx, reader_stream& stream, source_location const& loc) {
  consume(stream, ':');
  if (stream.peek() == U'|')
    return read_verbatim_keyword(ctx, stream, loc);
  else
    return {keyword_literal{read_identifier_contents(stream)}, loc};
}

static token
read_token_after_octothorpe(context& ctx, reader_stream& stream,
                            source_location const& loc) {
  std::optional<char32_t> c = stream.peek();
  if (!c)
    throw read_error{"Unexpected end of input", stream.location()};
  else if (*c == '\'') {
    stream.read();
    return {octothorpe_quote{}, loc};
  } else if (*c == '`') {
    stream.read();
    return {octothorpe_backquote{}, loc};
  } else if (*c == ',') {
    stream.read();
    c = stream.peek();
    if (*c == '@') {
      stream.read();
      return {octothorpe_comma_at{}, loc};
    } else
      return {octothorpe_comma{}, loc};
  }
  else if (*c == '(')
    return {octothorpe_left_paren{}, loc};
  else if (digit(*c))
    return read_datum_label(stream, loc);
  else if (*c == ';')
    return read_datum_comment(ctx, stream);
  else if (*c == '|')
    return read_block_comment(ctx, stream);
  else if (*c == '!')
    return read_directive(ctx, stream);
  else if (*c == ':')
    return read_keyword(ctx, stream, loc);
  else if (*c == 'u') {
    consume(stream, 'u');
    expect(stream, '8');
    return {octothorpe_u8{}, loc};
  } else
    return read_special_literal(ctx, stream);
}

static bool
can_begin_identifier(char32_t c) {
  return c != '(' && c != ')' && c != '\'' && c != '`' && c != ',' && c != '#'
         && c != '"' && c != '|';
}

static token
read_after_delimiter(context& ctx, reader_stream& stream) {
  source_location loc = stream.location();
  char32_t delimiter = require_char(stream);
  switch (delimiter) {
  case '(': return {left_paren{}, loc};
  case ')': return {right_paren{}, loc};
  case '\'': return {quote{}, loc};
  case '`': return {backquote{}, loc};
  case ',': return read_token_after_comma(stream, loc);
  case '"': return read_string_literal(ctx, stream);
  case '|': return read_verbatim_identifier(ctx, stream);
  default:
    assert(false);
    return {};
  }
}

template <auto OtherParser>
static token
read_number_or(context& ctx, reader_stream& stream) {
  if (auto numeric_token = read_numeric_literal(ctx, stream))
    return *numeric_token;
  else
    return OtherParser(stream);
}

static token
require_numeric_literal(context& ctx, reader_stream& stream) {
  source_location loc = stream.location();

  if (auto numeric_token = read_numeric_literal(ctx, stream))
    return *numeric_token;
  else
    throw read_error{"Expected numeric literal", loc};
}

static token
read_after_octothorpe(context& ctx, reader_stream& stream) {
  source_location loc = stream.location();
  if (auto numeric_token = read_numeric_literal(ctx, stream))
    return *numeric_token;
  else {
    consume(stream, '#');
    return read_token_after_octothorpe(ctx, stream, loc);
  }
}

static token
read_token(context& ctx, reader_stream& stream) {
  skip_whitespace(stream);

  source_location loc = stream.location();
  std::optional<char32_t> c = stream.peek();

  if (!c)
    return {end{}, loc};
  else if (*c == '.')
    return read_number_or<read_token_after_period>(ctx, stream);
  else if (*c == '+' || *c == '-')
    return read_number_or<read_identifier>(ctx, stream);
  else if (digit(*c))
    return require_numeric_literal(ctx, stream);
  else if (can_begin_identifier(*c))
    return read_identifier(stream);
  else if (*c == '#')
    return read_after_octothorpe(ctx, stream);
  else
    return read_after_delimiter(ctx, stream);
}

static ptr<>
wrap(context& ctx, ptr<> value, source_location const& loc, bool read_syntax) {
  if (!value)
    return nullptr;

  if (read_syntax)
    return make<syntax>(ctx, value, loc);
  else
    return value;
}

static ptr<>
unwrap(ptr<> value) {
  if (auto stx = match<syntax>(value))
    return stx->get_expression_without_update();
  else
    return value;
}

static ptr<>
find_datum_label_reference(datum_labels const& labels, std::string const& label,
                           source_location const& ref_location) {
  if (auto it = labels.find(label); it != labels.end()) {
    assert(it->second);
    return it->second;
  } else
    throw read_error{fmt::format("Unknown datum label: {}", label), ref_location};
}

static ptr<>
read_and_wrap(context& ctx, token const& first_token, reader_stream& stream,
              bool read_syntax, datum_labels& labels) {
  return wrap(ctx,
              read(ctx, first_token, stream, read_syntax, labels),
              first_token.location, read_syntax);
}

static void
define_label(datum_labels& labels, std::string const& label, ptr<> value) {
  if (auto it = labels.find(label); it != labels.end())
    it->second = value;
  else
    labels.emplace(label, value);
}

static void
define_label(datum_labels& labels,
             std::optional<std::string> const& defining_label,
             ptr<> value) {
  if (defining_label)
    define_label(labels, *defining_label, value);
}

static ptr<>
read_list(context& ctx, reader_stream& stream, bool read_syntax,
          datum_labels& labels,
          std::optional<std::string> const& defining_label) {
  token t = read_token(ctx, stream);
  if (std::holds_alternative<end>(t.value))
    throw read_error{"Unterminated list", t.location};
  else if (std::holds_alternative<dot>(t.value))
    throw read_error{"Unexpected . token", t.location};
  else if (std::holds_alternative<right_paren>(t.value))
    return ctx.constants->null;

  ptr<pair> result = make<pair>(ctx, ctx.constants->null, ctx.constants->null);
  define_label(labels, defining_label, result);

  result->set_car(ctx.store, read_and_wrap(ctx, t, stream, read_syntax, labels));
  ptr<pair> tail = result;

  t = read_token(ctx, stream);
  while (!std::holds_alternative<end>(t.value)
         && !std::holds_alternative<right_paren>(t.value)
         && !std::holds_alternative<dot>(t.value)) {
    ptr<pair> new_tail = make<pair>(ctx,
                                    read_and_wrap(ctx, t, stream, read_syntax,
                                                  labels),
                                    ctx.constants->null);
    tail->set_cdr(ctx.store, new_tail);
    tail = new_tail;

    t = read_token(ctx, stream);
  }

  if (std::holds_alternative<end>(t.value))
    throw read_error{"Unterminated list", t.location};
  else if (std::holds_alternative<dot>(t.value)) {
    t = read_token(ctx, stream);
    ptr<> cdr = read_and_wrap(ctx, t, stream, read_syntax, labels);
    tail->set_cdr(ctx.store, cdr);

    t = read_token(ctx, stream);
    if (!std::holds_alternative<right_paren>(t.value))
      throw read_error{"Too many elements after .", t.location};
  }

  assert(std::holds_alternative<right_paren>(t.value));
  return result;
}

static bool
valid_bytevector_element(ptr<> e) {
  if (auto i = match<integer>(e))
    return i->value() >= std::numeric_limits<bytevector::element_type>::min()
           && i->value() <= std::numeric_limits<bytevector::element_type>::max();
  else
    return false;
}

static std::vector<ptr<>>
read_vector_elements(context& ctx, reader_stream& stream, bool read_syntax,
                     datum_labels& labels, bool bytevector = false) {
  consume(stream, '(');

  std::vector<ptr<>> elements;

  token t = read_token(ctx, stream);
  while (!std::holds_alternative<end>(t.value)
         && !std::holds_alternative<right_paren>(t.value)) {
    source_location loc = stream.location();
    elements.push_back(read_and_wrap(ctx, t, stream, read_syntax, labels));

    if (bytevector) {
      elements.back() = unwrap(elements.back());
      if (!valid_bytevector_element(elements.back()))
        throw read_error{"Invalid bytevector element", loc};
    }

    t = read_token(ctx, stream);
  }

  if (std::holds_alternative<end>(t.value))
    throw read_error{"Unterminated vector", t.location};

  return elements;
}

static void
replace_value(context& ctx, ptr<vector> v, ptr<> from, ptr<> to) {
  std::vector<ptr<>> stack{v};
  while (!stack.empty()) {
    ptr<> top = stack.back();
    stack.pop_back();

    if (auto p = match<pair>(top)) {
      if (p->car() == from)
        p->set_car(ctx.store, to);
      else
        stack.push_back(p->car());

      if (p->cdr() == from)
        p->set_cdr(ctx.store, to);
      else
        stack.push_back(p->cdr());
    } else if (auto v = match<vector>(top)) {
      for (std::size_t i = 0; i < v->size(); ++i) {
        if (v->ref(i) == from)
          v->set(ctx.store, i, to);
        else
          stack.push_back(v->ref(i));
      }
    } else
      assert(top != from);
  }
}

static ptr<>
read_vector(context& ctx, reader_stream& stream, bool read_syntax,
            datum_labels& labels,
            std::optional<std::string> const& defining_label) {
  ptr<> dummy_vector;
  if (defining_label) {
    // We need to add the vector to labels now so it can be referenced from
    // inside the vector's elements. At the same time, to create the vector we
    // need to read the elements to know how many there are. Quite the pickle.
    //
    // We'll create an empty vector, put it in labels, read the elements, then
    // create the result and recurse through all its elements replacing the
    // dummy empty vector value with the real one.

    dummy_vector = make<vector>(ctx, 0, ctx.constants->void_);
    define_label(labels, *defining_label, dummy_vector);
  }

  std::vector<ptr<>> elements = read_vector_elements(ctx, stream, read_syntax,
                                                     labels);
  ptr<vector> result = make<vector>(ctx, elements.size(), ctx.constants->void_);
  for (std::size_t i = 0; i < elements.size(); ++i)
    result->set(ctx.store, i, elements[i]);

  if (defining_label) {
    replace_value(ctx, result, dummy_vector, result);
    labels[*defining_label] = result;
  }

  return result;
}

static ptr<>
read_bytevector(context& ctx, reader_stream& stream, bool read_syntax,
                datum_labels& labels,
                std::optional<std::string> const& defining_label) {
  source_location loc = stream.location();
  std::vector<ptr<>> elements = read_vector_elements(ctx, stream, read_syntax,
                                                     labels, true);

  auto bv = make<bytevector>(ctx, elements.size());
  for (std::size_t i = 0; i < elements.size(); ++i)
    bv->set(
      i,
      static_cast<bytevector::element_type>(assume<integer>(elements[i]).value())
    );

  if (defining_label)
    labels.emplace(*defining_label, bv);

  return bv;
}

static ptr<>
read_shortcut(context& ctx, reader_stream& stream, token const& shortcut_token,
              std::string const& shortcut, std::string const& expansion,
              bool read_syntax, datum_labels& labels,
              std::optional<std::string> const& defining_label) {
  token t = read_token(ctx, stream);
  if (std::holds_alternative<end>(t.value))
    throw read_error{fmt::format("Expected token after {}", shortcut),
                     t.location};

  ptr<pair> result = cons(ctx,
                          wrap(ctx, ctx.intern(expansion),
                               shortcut_token.location, read_syntax),
                          ctx.constants->null);
  define_label(labels, defining_label, result);

  ptr<> body = read_and_wrap(ctx, t, stream, read_syntax, labels);
  result->set_cdr(ctx.store, cons(ctx, body, ctx.constants->null));

  return result;
}

static ptr<>
define_label_for_atomic_value(ptr<> value, datum_labels& labels,
                              std::optional<std::string> const& defining_label) {
  if (defining_label)
    define_label(labels, *defining_label, value);

  return value;
}

static ptr<>
read(context& ctx, token first_token, reader_stream& stream,
     bool read_syntax, datum_labels& labels,
     std::optional<std::string> const& defining_label) {
  if (std::holds_alternative<end>(first_token.value))
    return {};
  else if (std::holds_alternative<left_paren>(first_token.value))
    return read_list(ctx, stream, read_syntax, labels, defining_label);
  else if (std::holds_alternative<octothorpe_left_paren>(first_token.value))
    return read_vector(ctx, stream, read_syntax, labels, defining_label);
  else if (std::holds_alternative<octothorpe_u8>(first_token.value))
    return read_bytevector(ctx, stream, read_syntax, labels, defining_label);
  else if (std::holds_alternative<quote>(first_token.value))
    return read_shortcut(ctx, stream, first_token, "'", "quote", read_syntax,
                         labels, defining_label);
  else if (std::holds_alternative<octothorpe_quote>(first_token.value))
    return read_shortcut(ctx, stream, first_token, "#'", "syntax", read_syntax,
                         labels, defining_label);
  else if (std::holds_alternative<backquote>(first_token.value))
    return read_shortcut(ctx, stream, first_token, "`", "quasiquote",
                         read_syntax, labels, defining_label);
  else if (std::holds_alternative<octothorpe_backquote>(first_token.value))
    return read_shortcut(ctx, stream, first_token, "#`", "quasisyntax",
                         read_syntax, labels, defining_label);
  else if (std::holds_alternative<comma>(first_token.value))
    return read_shortcut(ctx, stream, first_token, ",", "unquote", read_syntax,
                         labels, defining_label);
  else if (std::holds_alternative<octothorpe_comma>(first_token.value))
    return read_shortcut(ctx, stream, first_token, "#,", "unsyntax", read_syntax,
                         labels, defining_label);
  else if (std::holds_alternative<comma_at>(first_token.value))
    return read_shortcut(ctx, stream, first_token, ",@", "unquote-splicing",
                         read_syntax, labels, defining_label);
  else if (std::holds_alternative<octothorpe_comma_at>(first_token.value))
    return read_shortcut(ctx, stream, first_token, "#,@", "unsyntax-splicing",
                         read_syntax, labels, defining_label);
  else if (generic_literal* lit
             = std::get_if<generic_literal>(&first_token.value))
    return define_label_for_atomic_value(lit->value, labels, defining_label);
  else if (identifier* i = std::get_if<identifier>(&first_token.value))
    return define_label_for_atomic_value(ctx.intern(i->value), labels,
                                         defining_label);
  else if (keyword_literal* k = std::get_if<keyword_literal>(&first_token.value))
    return define_label_for_atomic_value(ctx.intern_keyword(k->value), labels,
                                         defining_label);
  else if (boolean_literal* b = std::get_if<boolean_literal>(&first_token.value))
    return define_label_for_atomic_value(
      b->value ? ctx.constants->t : ctx.constants->f,
      labels,
      defining_label
    );
  else if (std::holds_alternative<void_literal>(first_token.value))
    return define_label_for_atomic_value(ctx.constants->void_, labels,
                                         defining_label);
  else if (std::holds_alternative<default_value_literal>(first_token.value))
    return define_label_for_atomic_value(ctx.constants->default_value, labels,
                                         defining_label);
  else if (std::holds_alternative<dot>(first_token.value))
    throw read_error{"Unexpected . token", first_token.location};
  else if (std::holds_alternative<right_paren>(first_token.value))
    throw read_error{"Unexpected ) token", first_token.location};
  else if (datum_label_definition* dldef
             = std::get_if<datum_label_definition>(&first_token.value))
    return read(ctx, read_token(ctx, stream), stream, read_syntax, labels,
                dldef->label);
  else if (datum_label_reference* dlref
             = std::get_if<datum_label_reference>(&first_token.value))
    return find_datum_label_reference(labels, dlref->label, first_token.location);

  assert(false); // Unimplemented token
  throw read_error{"Unimplemented token", first_token.location};
}

static ptr<>
read_optional(context& ctx, ptr<textual_input_port> stream) {
  reader_stream s{track(ctx, stream)};
  datum_labels labels;
  return read(ctx, read_token(ctx, s), s, false, labels);
}

ptr<>
read(context& ctx, ptr<textual_input_port> stream) {
  if (ptr<> result = read_optional(ctx, stream))
    return result;
  else
    return ctx.constants->eof;
}

ptr<>
read(context& ctx, std::string s) {
  unique_port_handle<ptr<textual_input_port>> h{open_input_string(ctx, std::move(s))};
  return read(ctx, *h);
}

static ptr<syntax>
read_syntax_optional(context& ctx, reader_stream& s) {
  datum_labels labels;
  return assume<syntax>(
    read_and_wrap(ctx, read_token(ctx, s), s, true, labels)
  );
}

static ptr<>
read_syntax(context& ctx, reader_stream& s) {
  if (auto stx = read_syntax_optional(ctx, s))
    return stx;
  else
    return ctx.constants->eof;
}

ptr<>
read_syntax(context& ctx, ptr<textual_input_port> stream) {
  reader_stream s{track(ctx, stream)};
  return read_syntax(ctx, s);
}

ptr<>
read_syntax(context& ctx, std::string s) {
  unique_port_handle<ptr<textual_input_port>> h{
    open_input_string(ctx, std::move(s))
  };
  return read_syntax(ctx, *h);
}

std::vector<ptr<>>
read_multiple(context& ctx, ptr<textual_input_port> in) {
  std::vector<ptr<>> result;
  while (ptr<> elem = read_optional(ctx, in))
    result.push_back(elem);

  return result;
}

std::vector<ptr<>>
read_multiple(context& ctx, std::string s) {
  unique_port_handle<ptr<textual_input_port>> h{
    open_input_string(ctx, std::move(s))
  };
  return read_multiple(ctx, *h);
}

static std::vector<ptr<syntax>>
read_syntax_multiple(context& ctx, reader_stream& stream) {
  std::vector<ptr<syntax>> result;
  while (ptr<syntax> elem = read_syntax_optional(ctx, stream))
    result.push_back(elem);

  return result;
}

std::vector<ptr<syntax>>
read_syntax_multiple(context& ctx, ptr<textual_input_port> p) {
  reader_stream in{track(ctx, p)};
  return read_syntax_multiple(ctx, in);
}

std::vector<ptr<syntax>>
read_syntax_multiple_ci(context& ctx, ptr<textual_input_port> p) {
  reader_stream in{track(ctx, p)};
  in.fold_case = true;
  return read_syntax_multiple(ctx, in);
}

std::vector<ptr<syntax>>
read_syntax_multiple(context& ctx, std::string s) {
  unique_port_handle<ptr<textual_input_port>> h{
    open_input_string(ctx, std::move(s))
  };
  return read_syntax_multiple(ctx, *h);
}

ptr<>
string_to_number(context& ctx, std::string const& s, unsigned base) {
  if (base != 2 && base != 8 && base != 10 && base != 16)
    throw std::runtime_error{"Invalid base"};

  try {
    auto port = open_input_string(ctx, s);
    reader_stream stream{track(ctx, port)};

    if (ptr<> result = read_number(ctx, stream, stream.location(), base)) {
      if (!stream.read())
        return result;
      else
        return ctx.constants->f;
    } else
      return ctx.constants->f;
  } catch (read_error const&) {
    return ctx.constants->f;
  }
}

static ptr<>
read_syntax_multiple_proc(context& ctx, ptr<textual_input_port> p) {
  return make_list_from_range(ctx, read_syntax_multiple(ctx, p));
}

static ptr<>
read_syntax_multiple_ci_proc(context& ctx, ptr<textual_input_port> p) {
  return make_list_from_range(ctx, read_syntax_multiple_ci(ctx, p));
}

void
export_read(context& ctx, ptr<module_> result) {
  auto default_input_port = make<textual_input_port>(
    ctx, std::make_unique<stdin_source>(), "<stdin>"
  );
  ctx.constants->current_input_port_tag
    = create_parameter_tag(ctx, default_input_port);

  define_top_level(ctx, "current-input-port-tag", result, true,
                   ctx.constants->current_input_port_tag);
  define_procedure<
    static_cast<ptr<> (*)(context&, ptr<textual_input_port>)>(read)
  >(
    ctx, "read", result, get_current_textual_input_port
  );
  define_procedure<
    static_cast<ptr<> (*)(context&, ptr<textual_input_port>)>(read_syntax)
  >(
    ctx, "read-syntax", result, get_current_textual_input_port
  );
  define_procedure<read_syntax_multiple_proc>(
    ctx, "read-syntax-multiple", result, get_current_textual_input_port
  );
  define_procedure<read_syntax_multiple_ci_proc>(
    ctx,
    "read-syntax-multiple-ci",
    result,
    get_current_textual_input_port
  );
  define_procedure<string_to_number>(ctx, "string->number", result,
                                     [] (context&) { return 10; });
  define_procedure<&read_error::scheme_error::message>(
    ctx, "read-error-message", result
  );
}

} // namespace insider
