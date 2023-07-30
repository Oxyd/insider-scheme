#include "io/tokenizer.hpp"

#include "compiler/source_location.hpp"
#include "context.hpp"
#include "io/char_categories.hpp"
#include "io/read.hpp"
#include "io/reader_stream.hpp"
#include "runtime/character.hpp"
#include "runtime/numeric.hpp"

#include <format>

namespace insider {

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
    throw read_error{std::format("Unexpected character: {}, expected {}",
                                 to_utf8(c), to_utf8(expected)),
                     loc};
}

static void
expect_either(reader_stream& stream, char32_t expected1, char32_t expected2) {
  source_location loc = stream.location();
  char32_t c = require_char(stream);
  if (c != expected1 && c != expected2)
    throw read_error{std::format("Unexpected character: {}, expected {} or {}",
                                 to_utf8(c),
                                 to_utf8(expected1),
                                 to_utf8(expected2)),
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

  return normalize_fraction(
    ctx,
    make<fraction>(ctx,
                   read_integer(ctx, numerator_digits, mode.base),
                   read_integer(ctx, denominator_digits, mode.base))
  );
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
    throw read_error{std::format("Invalid floating point literal: {}", s), loc};

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
    auto magnitude
      = expect<integer>(read_integer(ctx, suffix.substr(2))).value();
    return static_cast<int>(sign * magnitude);
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
    throw read_error{std::format("{} unexpected following a numeric literal",
                                 to_utf8(*c)), loc};
}

ptr<>
read_number(context& ctx, reader_stream& stream, source_location const& loc,
            unsigned default_base) {
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
    return token{token::generic_literal{value}, loc};
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
      return {token::generic_literal{character_to_ptr(*c)}, loc};

    std::u32string literal = *c + read_until_delimiter(stream);
    if (literal.size() == 1)
      return {token::generic_literal{character_to_ptr(literal[0])}, loc};
    else {
      if (stream.fold_case)
        literal = string_foldcase(literal);

      if (auto it = character_names.find(literal); it != character_names.end())
        return {token::generic_literal{character_to_ptr(it->second)}, loc};
      else
        throw read_error{std::format("Unknown character literal #\\{}",
                                     to_utf8(literal)), loc};
    }
  }
  else {
    source_location loc = stream.location();
    std::u32string digits = read_hexdigits(stream);

    if (!digits.empty())
      return {
        token::generic_literal{
          character_to_ptr(read_character_from_hexdigits(ctx, digits))
        },
        loc
      };
    else
      return {token::generic_literal{character_to_ptr('x')}, loc};
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
    return {token::boolean_literal{true}, loc};
  else if (literal == U"f" || literal == U"false")
    return {token::boolean_literal{false}, loc};
  else if (literal == U"void")
    return {token::void_literal{}, loc};
  else if (literal == U"default-value")
    return {token::default_value_literal{}, loc};
  else
    throw read_error{std::format("Invalid literal: {}", to_utf8(literal)),
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
  return {token::identifier{read_identifier_contents(stream)}, loc};
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
    throw read_error{std::format("Unrecognised escape sequence \\{}",
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

    throw read_error{std::format("Unrecognised escape sequence \\{}",
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
  return {token::identifier{read_verbatim_identifier_contents(ctx, stream)},
          loc};
}

static void
append(std::string& s, char32_t c) {
  to_utf8(c, [&] (char byte) { s.push_back(byte); });
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
        append(result, *escape);
    } else
      append(result, c);
  }

  return {token::generic_literal{make<string>(ctx, std::move(result))}, loc};
}

static token
read_token_after_comma(reader_stream& stream, source_location const& loc) {
  std::optional<char32_t> c = stream.peek();
  if (c && *c == '@') {
    stream.read();
    return {token::unquote_splicing{}, loc};
  }
  else
    return {token::unquote{}, loc};
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
    return {token::dot{}, loc};
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
    return {token::datum_label_definition{std::move(label)}, loc};
  else if (*c == '#')
    return {token::datum_label_reference{std::move(label)}, loc};
  else
    throw read_error{"Unexpected character after datum label",
                     stream.location()};
}

static token
read_datum_comment(reader_stream& stream) {
  consume(stream, ';');
  return {token::datum_comment{}, stream.location()};
}

static token
read_token(context& ctx, reader_stream& stream);

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
    throw read_error{std::format("Invalid directive: {}", to_utf8(directive)),
                     loc};

  return read_token(ctx, stream);
}

static token
read_verbatim_keyword(context& ctx, reader_stream& stream,
                      source_location const& loc) {
  consume(stream, '|');
  return {token::keyword_literal{read_verbatim_identifier_contents(ctx, stream)},
          loc};
}

static token
read_keyword(context& ctx, reader_stream& stream, source_location const& loc) {
  consume(stream, ':');
  if (stream.peek() == U'|')
    return read_verbatim_keyword(ctx, stream, loc);
  else
    return {token::keyword_literal{read_identifier_contents(stream)}, loc};
}

static std::u32string
read_raw_string_delimiter(reader_stream& stream) {
  std::u32string result;
  while (stream.peek() != U'(') {
    source_location loc = stream.location();
    char32_t c = require_char(stream);

    if (whitespace(c) || delimiter(c) || c == U'\\')
      throw read_error{"Invalid character in raw string delimiter", loc};

    result += c;
  }
  return result;
}

static bool
consume_string(reader_stream& stream, std::u32string const& sequence) {
  for (char32_t c : sequence)
    if (stream.read() != c)
      return false;
  return true;
}

static bool
end_of_raw_string_literal(reader_stream& stream, std::u32string const& delim) {
  auto cp = stream.make_checkpoint();

  if (stream.read() == U')'
      && consume_string(stream, delim)
      && stream.read() == U'"') {
    cp.commit();
    return true;
  } else
    return false;
}

static token
read_raw_string_literal(context& ctx, reader_stream& stream) {
  source_location loc = stream.location();

  consume(stream, 'R');
  expect(stream, '"');
  std::u32string delim = read_raw_string_delimiter(stream);
  expect(stream, '(');

  std::string result;
  while (!end_of_raw_string_literal(stream, delim))
    append(result, require_char(stream));

  return {token::generic_literal{make<string>(ctx, std::move(result))}, loc};
}

static token
read_token_after_octothorpe(context& ctx, reader_stream& stream,
                            source_location const& loc) {
  std::optional<char32_t> c = stream.peek();
  if (!c)
    throw read_error{"Unexpected end of input", stream.location()};
  else if (*c == '\'') {
    stream.read();
    return {token::syntax{}, loc};
  } else if (*c == '`') {
    stream.read();
    return {token::quasisyntax{}, loc};
  } else if (*c == ',') {
    stream.read();
    c = stream.peek();
    if (c == U'@') {
      stream.read();
      return {token::unsyntax_splicing{}, loc};
    } else
      return {token::unsyntax{}, loc};
  }
  else if (*c == '(') {
    stream.read();
    return {token::begin_vector{}, loc};
  } else if (digit(*c))
    return read_datum_label(stream, loc);
  else if (*c == ';')
    return read_datum_comment(stream);
  else if (*c == '|')
    return read_block_comment(ctx, stream);
  else if (*c == '!')
    return read_directive(ctx, stream);
  else if (*c == ':')
    return read_keyword(ctx, stream, loc);
  else if (*c == 'u') {
    consume(stream, 'u');
    expect(stream, '8');
    expect(stream, '(');
    return {token::begin_bytevector{}, loc};
  } else if (*c == 'R')
    return read_raw_string_literal(ctx, stream);
  else
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
  case '(': return {token::left_paren{}, loc};
  case ')': return {token::right_paren{}, loc};
  case '\'': return {token::quote{}, loc};
  case '`': return {token::quasiquote{}, loc};
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
    return {token::end{}, loc};
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

tokenizer::tokenizer(context& ctx, reader_stream& stream)
  : ctx_{ctx}
  , stream_{stream}
  , current_{read_token(ctx, stream)}
{ }

void
tokenizer::advance() {
  current_ = read_token(ctx_, stream_);
}

source_location
tokenizer::location() const {
  return stream_.location();
}

} // namespace insider
