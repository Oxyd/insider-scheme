#include "io.hpp"

#include <fmt/format.h>

#include <sstream>
#include <variant>

namespace game::scm {

struct end { };
struct left_paren { };
struct right_paren { };

struct integer_literal {
  integer::storage_type value;
};

struct boolean_literal {
  bool value;
};

struct identifier {
  std::string value;
};

using token = std::variant<
  end,
  left_paren,
  right_paren,
  integer_literal,
  boolean_literal,
  identifier
>;

parse_error::parse_error(std::string const& message)
  : std::runtime_error{fmt::format("Parse error: {}", message)}
{ }

static constexpr auto eof = std::char_traits<char>::eof();

static bool
whitespace(char c) {
  return c == ' ' || c == '\n' || c == '\t';
}

static bool
delimiter(char c) {
  return whitespace(c)
         || c == '(' || c == ')'
         || c == '\'' || c == '"' || c == '`'
         || c == '#'
         ;
}

static void
skip_whitespace(std::istream& stream) {
  char c = stream.peek();
  while (c != eof && whitespace(c)) {
    stream.get();
    c = stream.peek();
  }
}

static std::string
read_until_delimiter(std::istream& stream) {
  std::string result;
  while (stream.peek() != eof && !delimiter(stream.peek()))
    result += stream.get();

  return result;
}

static bool
digit(char c) {
  return c >= '0' && c <= '9';
}

static unsigned
digit_value(char c) {
  assert(digit(c));
  return c - '0';
}

static integer_literal
read_integer_literal(std::istream& stream, bool negative = false) {
  integer::storage_type result = 0;
  char c = stream.peek();

  assert(c != eof);
  if (c == '-' || c == '+') {
    negative = c == '-';
    stream.get();
    c = stream.peek();
  }

  constexpr integer::storage_type overflow_divisor
    = std::numeric_limits<integer::storage_type>::max() / 10;
  constexpr integer::storage_type overflow_remainder
    = std::numeric_limits<integer::storage_type>::max() % 10;

  while (c != eof && digit(c)) {
    if (result > overflow_divisor
        || (result == overflow_divisor && digit_value(c) > overflow_remainder))
      throw parse_error{"Integer literal overflow"};

    result *= 10;
    result += digit_value(c);

    stream.get();
    c = stream.peek();
  }

  if (result > static_cast<integer::storage_type>(std::numeric_limits<integer::value_type>::max())
               + (negative ? 1 : 0))
    throw parse_error{"Integer literal overflow"};

  if (negative)
    result = ~result + 1;

  return integer_literal{result};
}

static token
read_special_literal(std::istream& stream) {
  char c = stream.peek();
  if (c == eof)
    throw parse_error{"Unexpected end of input"};

  switch (c) {
  case 't':
  case 'f': {
    // These can be either #t or #f, or #true or #false.
    std::string literal = read_until_delimiter(stream);

    if (literal != "t" && literal != "f" && literal != "true" && literal != "false")
      throw parse_error{fmt::format("Invalid literal: {}", literal)};

    if (c == 't')
      return boolean_literal{true};
    else
      return boolean_literal{false};
  }

  default:
    throw parse_error{"Unimplemented"};
  }
}

static identifier
read_identifier(std::istream& stream, std::string prefix = "") {
  std::string value = std::move(prefix);
  while (stream.peek() != eof && !delimiter(stream.peek()))
    value += stream.get();

  return identifier{std::move(value)};
}

static token
read_token(std::istream& stream) {
  skip_whitespace(stream);

  char c = stream.peek();
  if (c == eof)
    return end{};

  if (c == '(') {
    stream.get();
    return left_paren{};
  } else if (c == ')') {
    stream.get();
    return right_paren{};
  }
  else if (c == '+' || c == '-') {
    // This can begin either a number (like -2) or a symbol (like + -- the
    // addition function).

    char initial = c;
    stream.get();
    bool negative = false;
    if (initial == '-')
      negative = true;
    else
      assert(initial == '+');

    c = stream.peek();
    if (c == eof)
      return identifier{std::string(1, initial)};

    if (digit(c))
      return read_integer_literal(stream, negative);
    else
      return read_identifier(stream, std::string(1, initial));
  }
  else if (digit(c))
    return read_integer_literal(stream);
  else if (c == '#') {
    stream.get();
    c = stream.peek();
    if (c == eof)
      throw parse_error{"Unexpected end of input"};
    else if (c == '$')
      return read_identifier(stream, "#");
    else
      return read_special_literal(stream);
  } else
    return read_identifier(stream);
}

static generic_ptr
read(token first_token, std::istream& stream);

static generic_ptr
read_list(std::istream& stream) {
  token t = read_token(stream);
  if (std::holds_alternative<end>(t))
    throw parse_error{"Unterminated list"};
  else if (std::holds_alternative<right_paren>(t))
    return null();

  ptr<pair> result = make<pair>(read(t, stream), null());
  ptr<pair> tail = result;

  t = read_token(stream);
  while (!std::holds_alternative<end>(t) && !std::holds_alternative<right_paren>(t)) {
    ptr<pair> new_tail = make<pair>(read(t, stream), null());
    tail->set_cdr(new_tail);
    tail = new_tail;

    t = read_token(stream);
  }

  if (std::holds_alternative<end>(t))
    throw parse_error{"Unterminated list"};

  assert(std::holds_alternative<right_paren>(t));
  return result;
}

generic_ptr
read(token first_token, std::istream& stream) {
  if (std::holds_alternative<end>(first_token))
    return {};
  else if (std::holds_alternative<left_paren>(first_token))
    return read_list(stream);
  else if (integer_literal* i = std::get_if<integer_literal>(&first_token))
    return make<integer>(i->value);
  else if (identifier* i = std::get_if<identifier>(&first_token))
    return intern(i->value);
  else if (boolean_literal* b = std::get_if<boolean_literal>(&first_token))
    return b->value ? thread_context().constants->t : thread_context().constants->f;

  throw parse_error{"Probably unimplemented"};
}

generic_ptr
read(std::istream& stream) {
  return read(read_token(stream), stream);
}

generic_ptr
read(std::string const& s) {
  std::istringstream is(s);
  return read(is);
}

} // namespace game::scm
