#include "read.hpp"

#include "basic_types.hpp"
#include "character.hpp"
#include "input_stream.hpp"
#include "numeric.hpp"
#include "port.hpp"
#include "source_location.hpp"
#include "string.hpp"

#include <fmt/format.h>

#include <variant>

namespace insider {

namespace {
  struct end { };
  struct left_paren { };
  struct right_paren { };
  struct hash_left_paren { };
  struct dot { };

  struct generic_literal {
    ptr<> value;
  };

  struct boolean_literal {
    bool value;
  };

  struct void_literal { };

  struct identifier {
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
      hash_left_paren,
      dot,
      generic_literal,
      boolean_literal,
      void_literal,
      identifier,
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
  : std::runtime_error{fmt::format("{}: Read error: {}", format_location(loc), message)}
{ }

static bool
whitespace(char32_t c) {
  return c == ' ' || c == '\n' || c == '\t';
}

static bool
intraline_whitespace(char32_t c) {
  return c == ' ' || c == '\t';
}

static bool
delimiter(char32_t c) {
  return whitespace(c)
         || c == '(' || c == ')'
         || c == '\'' || c == '"' || c == '`'
         || c == '#'
         ;
}

static void
skip_whitespace(input_stream& stream) {
  std::optional<char32_t> c = stream.peek_character();

  while (c && (whitespace(*c) || *c == ';')) {
    while (c && whitespace(*c))
      c = stream.advance_and_peek_character();

    if (*c == ';')
      while ((c = stream.read_character()) && *c != '\n')
        ;

    c = stream.peek_character();
  }
}

static void
skip_intraline_whitespace(input_stream& stream) {
  std::optional<char32_t> c = stream.peek_character();
  while (c && intraline_whitespace(*c))
    c = stream.advance_and_peek_character();
}

static std::u32string
read_until_delimiter(input_stream& stream) {
  std::u32string result;
  while (stream.peek_character() && !delimiter(*stream.peek_character()))
    result += *stream.read_character();

  return result;
}

static bool
digit(char32_t c) {
  return c >= '0' && c <= '9';
}

static bool
hexdigit(char32_t c) {
  return digit(c) || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F');
}

static token
read_numeric_literal(context& ctx, input_stream& stream) {
  source_location loc = stream.current_location();
  return {generic_literal{read_number(ctx, stream)}, loc};
}

static std::u32string
read_hexdigits(input_stream& stream) {
  std::u32string result;
  while (stream.peek_character() && hexdigit(*stream.peek_character()))
    result += *stream.read_character();
  return result;
}

static char32_t
read_character_from_hexdigits(context& ctx, std::u32string const& digits) {
  auto value = expect<integer>(read_integer(ctx, digits, 16));
  return static_cast<char32_t>(value.value());
}

static token
read_character(context& ctx, input_stream& stream) {
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

  std::optional<char32_t> c = stream.read_character();
  if (!c)
    throw read_error{"Unexpected end of input", stream.current_location()};

  if (*c != 'x') {
    source_location loc = stream.current_location();
    if (!is_alphabetic(*c))
      return {generic_literal{character_to_ptr(*c)}, loc};

    std::u32string literal = *c + read_until_delimiter(stream);
    if (literal.size() == 1)
      return {generic_literal{character_to_ptr(literal[0])}, loc};
    else {
      if (stream.fold_case())
        literal = foldcase(literal);

      if (auto it = character_names.find(literal); it != character_names.end())
        return {generic_literal{character_to_ptr(it->second)}, loc};
      else
        throw read_error{fmt::format("Unknown character literal #\\{}", to_utf8(literal)), loc};
    }
  }
  else {
    source_location loc = stream.current_location();
    std::u32string digits = read_hexdigits(stream);

    if (!digits.empty())
      return {generic_literal{character_to_ptr(read_character_from_hexdigits(ctx, digits))}, loc};
    else
      return {generic_literal{character_to_ptr('x')}, loc};
  }
}

static token
read_special_literal(context& ctx, input_stream& stream) {
  std::optional<char32_t> c = stream.peek_character();
  if (!c)
    throw read_error{"Unexpected end of input", stream.current_location()};

  source_location loc = stream.current_location();

  switch (*c) {
  case 't':
  case 'f': {
    // These can be either #t or #f, or #true or #false.
    std::u32string literal = read_until_delimiter(stream);

    if (literal != U"t" && literal != U"f" && literal != U"true" && literal != U"false")
      throw read_error{fmt::format("Invalid literal: {}", to_utf8(literal)), loc};

    if (*c == 't')
      return {boolean_literal{true}, loc};
    else
      return {boolean_literal{false}, loc};
  }

  case 'v': {
    std::u32string literal = read_until_delimiter(stream);
    if (literal != U"void")
      throw read_error{fmt::format("Invalid literal: {}", to_utf8(literal)), loc};

    return {void_literal{}, loc};
  }

  case '\\': {
    stream.read_character();
    return read_character(ctx, stream);
  }

  default:
    throw read_error{"Unimplemented", loc};
  }
}

static char32_t
require_char(input_stream& stream) {
  std::optional<char32_t> result = stream.read_character();
  if (!result)
    throw read_error{"Unexpected end of input", stream.current_location()};
  return *result;
}

static void
consume(input_stream& stream, char32_t expected) {
  char32_t c = require_char(stream);
  assert(c == expected);
}

static void
expect(input_stream& stream, char32_t expected) {
  source_location loc = stream.current_location();
  char32_t c = require_char(stream);
  if (c != expected)
    throw read_error{fmt::format("Unexpected character: {}, expected {}", to_utf8(c), to_utf8(expected)), loc};
}

static token
read_identifier(input_stream& stream) {
  source_location loc = stream.current_location();
  std::u32string value;
  while (stream.peek_character() && !delimiter(*stream.peek_character()))
    value += *stream.read_character();

  if (stream.fold_case())
    value = foldcase(value);

  return {identifier{to_utf8(value)}, loc};
}

static char32_t
read_hex_escape(context& ctx, input_stream& stream) {
  std::u32string digits = read_hexdigits(stream);
  expect(stream, ';');
  return read_character_from_hexdigits(ctx, digits);
}

static char32_t
read_common_escape(context& ctx, input_stream& stream) {
  source_location loc = stream.current_location();
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
    throw read_error{fmt::format("Unrecognised escape sequence \\{}", to_utf8(escape)), loc};
  }
}

static std::optional<char32_t>
read_string_escape(context& ctx, input_stream& stream) {
  source_location loc = stream.current_location();
  char32_t escape = require_char(stream);
  if (escape == '\n') {
    skip_intraline_whitespace(stream);
    return {};
  } else if (intraline_whitespace(escape)) {
    skip_intraline_whitespace(stream);
    if (std::optional<char32_t> after_whitespace = stream.read_character())
      if (*after_whitespace == '\n') {
        skip_intraline_whitespace(stream);
        return {};
      }

    throw read_error{fmt::format("Unrecognised escape sequence \\{}", to_utf8(escape)), loc};
  } else {
    stream.put_back(escape);
    return read_common_escape(ctx, stream);
  }
}

static token
read_verbatim_identifier(context& ctx, input_stream& stream) {
  // The opening | was consumed before calling this function.

  source_location loc = stream.current_location();
  std::u32string value;

  std::optional<char32_t> c = stream.read_character();
  while (c && *c != '|') {
    if (*c == '\\')
      value += read_common_escape(ctx, stream);
    else
      value += *c;

    c = stream.read_character();
  }

  if (!c)
    throw read_error{"Unexpected end of input", stream.current_location()};

  return {identifier{to_utf8(value)}, loc};
}

static token
read_string_literal(context& ctx, input_stream& stream) {
  // The opening " was consumed before calling this function.

  source_location loc = stream.current_location();

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
read_token_after_comma(input_stream& stream, source_location loc) {
  std::optional<char32_t> c = stream.peek_character();
  if (c && *c == '@') {
    stream.read_character();
    return {comma_at{}, loc};
  }
  else
    return {comma{}, loc};
}

static token
read_token_after_period(context& ctx, input_stream& stream, source_location loc) {
  std::optional<char32_t> c = stream.peek_character();
  if (!c)
    throw read_error{"Unexpected end of input", stream.current_location()};

  if (delimiter(*c))
    return {dot{}, loc};
  else {
    stream.put_back(U'.');

    if (digit(*c))
      return read_numeric_literal(ctx, stream);
    else
      return read_identifier(stream);
  }
}

static token
read_identifier_after_plus_or_minus(context& ctx, input_stream& stream, source_location loc) {
  token id = read_identifier(stream);
  std::string const& id_value = std::get<identifier>(id.value).value;

  // Infinities and NaNs look like identifiers, but they're numbers.

  std::string value;
  std::locale c_locale{"C"};
  std::transform(id_value.begin(), id_value.end(), std::back_inserter(value),
                 [&] (char c) { return std::tolower(c, c_locale); });

  if (value == "+inf.0")
    return {generic_literal{make<floating_point>(ctx, floating_point::positive_infinity)}, loc};
  else if (value == "-inf.0")
    return {generic_literal{make<floating_point>(ctx, floating_point::negative_infinity)}, loc};
  else if (value == "+nan.0")
    return {generic_literal{make<floating_point>(ctx, floating_point::positive_nan)}, loc};
  else if (value == "-nan.0")
    return {generic_literal{make<floating_point>(ctx, floating_point::negative_nan)}, loc};

  return id;
}

static token
read_token_after_plus_or_minus(context& ctx, char32_t initial, input_stream& stream, source_location loc) {
  // This can begin either a number (like -2) or a symbol (like + -- the
  // addition function).

  std::optional<char32_t> c = stream.peek_character();
  if (!c)
    return {identifier{std::string(1, initial)}, loc};

  stream.put_back(initial);
  if (digit(*c) || *c == '.')
    return read_numeric_literal(ctx, stream);
  else
    return read_identifier_after_plus_or_minus(ctx, stream, loc);
}

static std::string
read_datum_label_value(input_stream& stream) {
  std::string result;

  std::optional<char32_t> c = stream.peek_character();
  while (c && digit(*c)) {
    to_utf8(*stream.read_character(), [&] (char byte) { result.push_back(byte); });
    c = stream.peek_character();
  }

  return result;
}

static token
read_datum_label(input_stream& stream, source_location loc) {
  std::string label = read_datum_label_value(stream);

  std::optional<char32_t> c = stream.read_character();
  if (!c)
    throw read_error{"Unexpected end of input", stream.current_location()};

  if (*c == '=')
    return {datum_label_definition{std::move(label)}, loc};
  else if (*c == '#')
    return {datum_label_reference{std::move(label)}, loc};
  else
    throw read_error{"Unexpected character after datum label", stream.current_location()};
}

static token
read_token(context& ctx, input_stream& stream);

using datum_labels = std::unordered_map<std::string, tracked_ptr<>>;

static ptr<>
read(context& ctx, token first_token, input_stream& stream, bool read_syntax,
     datum_labels& labels, std::optional<std::string> defining_label = {});

static token
read_datum_comment(context& ctx, input_stream& stream) {
  consume(stream, ';');
  datum_labels labels;
  read(ctx, read_token(ctx, stream), stream, false, labels); // Discard

  return read_token(ctx, stream);
}

static token
read_block_comment(context& ctx, input_stream& stream) {
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
read_directive(context& ctx, input_stream& stream) {
  consume(stream, '!');

  source_location loc = stream.current_location();
  std::u32string directive = read_until_delimiter(stream);
  if (directive == U"fold-case")
    stream.enable_fold_case();
  else if (directive == U"no-fold-case")
    stream.disable_fold_case();
  else
    throw read_error{fmt::format("Invalid directive: {}", to_utf8(directive)), loc};

  return read_token(ctx, stream);
}

static token
read_token_after_octothorpe(context& ctx, input_stream& stream, source_location loc) {
  std::optional<char32_t> c = stream.peek_character();
  if (!c)
    throw read_error{"Unexpected end of input", stream.current_location()};
  else if (*c == '\'') {
    stream.read_character();
    return {octothorpe_quote{}, loc};
  } else if (*c == '`') {
    stream.read_character();
    return {octothorpe_backquote{}, loc};
  } else if (*c == ',') {
    stream.read_character();
    c = stream.peek_character();
    if (*c == '@') {
      stream.read_character();
      return {octothorpe_comma_at{}, loc};
    } else
      return {octothorpe_comma{}, loc};
  } else if (*c == '$') {
    stream.put_back('#');
    return read_identifier(stream);
  }
  else if (*c == '(')
    return {hash_left_paren{}, loc};
  else if (digit(*c))
    return read_datum_label(stream, loc);
  else if (*c == ';')
    return read_datum_comment(ctx, stream);
  else if (*c == '|')
    return read_block_comment(ctx, stream);
  else if (*c == '!')
    return read_directive(ctx, stream);
  else
    return read_special_literal(ctx, stream);
}

static token
read_token(context& ctx, input_stream& stream) {
  skip_whitespace(stream);

  source_location loc = stream.current_location();
  std::optional<char32_t> c = stream.read_character();
  if (!c)
    return {end{}, loc};

  if (*c == '(')
    return {left_paren{}, loc};
  else if (*c == ')')
    return {right_paren{}, loc};
  else if (*c == '\'')
    return {quote{}, loc};
  else if (*c == '`')
    return {backquote{}, loc};
  else if (*c == ',')
    return read_token_after_comma(stream, loc);
  else if (*c == '.')
    return read_token_after_period(ctx, stream, loc);
  else if (*c == '+' || *c == '-')
    return read_token_after_plus_or_minus(ctx, *c, stream, loc);
  else if (digit(*c)) {
    stream.put_back(*c);
    return read_numeric_literal(ctx, stream);
  }
  else if (*c == '#')
    return read_token_after_octothorpe(ctx, stream, loc);
  else if (*c == '"')
    return read_string_literal(ctx, stream);
  else if (*c == '|')
    return read_verbatim_identifier(ctx, stream);
  else {
    stream.put_back(*c);
    return read_identifier(stream);
  }
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
find_datum_label_reference(datum_labels const& labels, std::string const& label, source_location ref_location) {
  if (auto it = labels.find(label); it != labels.end()) {
    assert(it->second);
    return it->second.get();
  } else
    throw read_error{fmt::format("Unknown datum label: {}", label), ref_location};
}

static ptr<>
read_and_wrap(context& ctx, token first_token, input_stream& stream, bool read_syntax, datum_labels& labels) {
  return wrap(ctx,
              read(ctx, first_token, stream, read_syntax, labels),
              first_token.location, read_syntax);
}

static void
define_label(context& ctx, datum_labels& labels, std::string const& label, ptr<> value) {
  if (auto it = labels.find(label); it != labels.end())
    it->second = track(ctx, value);
  else
    labels.emplace(label, track(ctx, value));
}

static void
define_label(context& ctx, datum_labels& labels, std::optional<std::string> const& defining_label, ptr<> value) {
  if (defining_label)
    define_label(ctx, labels, *defining_label, value);
}

static ptr<>
read_list(context& ctx, input_stream& stream, bool read_syntax, datum_labels& labels,
          std::optional<std::string> const& defining_label) {
  token t = read_token(ctx, stream);
  if (std::holds_alternative<end>(t.value))
    throw read_error{"Unterminated list", t.location};
  else if (std::holds_alternative<dot>(t.value))
    throw read_error{"Unexpected . token", t.location};
  else if (std::holds_alternative<right_paren>(t.value))
    return ctx.constants->null.get();

  ptr<pair> result = make<pair>(ctx, ctx.constants->null.get(), ctx.constants->null.get());
  define_label(ctx, labels, defining_label, result);

  result->set_car(ctx.store, read_and_wrap(ctx, t, stream, read_syntax, labels));
  ptr<pair> tail = result;

  t = read_token(ctx, stream);
  while (!std::holds_alternative<end>(t.value)
         && !std::holds_alternative<right_paren>(t.value)
         && !std::holds_alternative<dot>(t.value)) {
    ptr<pair> new_tail = make<pair>(ctx,
                                    read_and_wrap(ctx, t, stream, read_syntax, labels),
                                    ctx.constants->null.get());
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

static std::vector<ptr<>>
read_vector_elements(context& ctx, input_stream& stream, bool read_syntax, datum_labels& labels) {
  stream.read_character(); // Consume (

  std::vector<ptr<>> elements;

  token t = read_token(ctx, stream);
  while (!std::holds_alternative<end>(t.value) && !std::holds_alternative<right_paren>(t.value)) {
    elements.push_back(read_and_wrap(ctx, t, stream, read_syntax, labels));
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
read_vector(context& ctx, input_stream& stream, bool read_syntax, datum_labels& labels,
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

    dummy_vector = make<vector>(ctx, ctx, 0);
    define_label(ctx, labels, *defining_label, dummy_vector);
  }

  std::vector<ptr<>> elements = read_vector_elements(ctx, stream, read_syntax, labels);
  ptr<vector> result = make<vector>(ctx, ctx, elements.size());
  for (std::size_t i = 0; i < elements.size(); ++i)
    result->set(ctx.store, i, elements[i]);

  if (defining_label) {
    replace_value(ctx, result, dummy_vector, result);
    labels[*defining_label] = track(ctx, result);
  }

  return result;
}

static ptr<>
read_shortcut(context& ctx, input_stream& stream, token shortcut_token,
              std::string const& shortcut, std::string const& expansion,
              bool read_syntax, datum_labels& labels,
              std::optional<std::string> const& defining_label) {
  token t = read_token(ctx, stream);
  if (std::holds_alternative<end>(t.value))
    throw read_error{fmt::format("Expected token after {}", shortcut), t.location};

  ptr<pair> result = cons(ctx,
                          wrap(ctx, ctx.intern(expansion), shortcut_token.location, read_syntax),
                          ctx.constants->null.get());
  define_label(ctx, labels, defining_label, result);

  ptr<> body = read_and_wrap(ctx, t, stream, read_syntax, labels);
  result->set_cdr(ctx.store, cons(ctx, body, ctx.constants->null.get()));

  return result;
}

static ptr<>
define_label_for_atomic_value(context& ctx, ptr<> value, datum_labels& labels,
                              std::optional<std::string> const& defining_label) {
  if (defining_label)
    define_label(ctx, labels, *defining_label, value);

  return value;
}

static ptr<>
read(context& ctx, token first_token, input_stream& stream, bool read_syntax,
     datum_labels& labels, std::optional<std::string> defining_label) {
  if (std::holds_alternative<end>(first_token.value))
    return {};
  else if (std::holds_alternative<left_paren>(first_token.value))
    return read_list(ctx, stream, read_syntax, labels, defining_label);
  else if (std::holds_alternative<hash_left_paren>(first_token.value))
    return read_vector(ctx, stream, read_syntax, labels, defining_label);
  else if (std::holds_alternative<quote>(first_token.value))
    return read_shortcut(ctx, stream, first_token, "'", "quote", read_syntax, labels, defining_label);
  else if (std::holds_alternative<octothorpe_quote>(first_token.value))
    return read_shortcut(ctx, stream, first_token, "#'", "syntax", read_syntax, labels, defining_label);
  else if (std::holds_alternative<backquote>(first_token.value))
    return read_shortcut(ctx, stream, first_token, "`", "quasiquote", read_syntax, labels, defining_label);
  else if (std::holds_alternative<octothorpe_backquote>(first_token.value))
    return read_shortcut(ctx, stream, first_token, "#`", "quasisyntax", read_syntax, labels, defining_label);
  else if (std::holds_alternative<comma>(first_token.value))
    return read_shortcut(ctx, stream, first_token, ",", "unquote", read_syntax, labels, defining_label);
  else if (std::holds_alternative<octothorpe_comma>(first_token.value))
    return read_shortcut(ctx, stream, first_token, "#,", "unsyntax", read_syntax, labels, defining_label);
  else if (std::holds_alternative<comma_at>(first_token.value))
    return read_shortcut(ctx, stream, first_token, ",@", "unquote-splicing", read_syntax, labels, defining_label);
  else if (std::holds_alternative<octothorpe_comma_at>(first_token.value))
    return read_shortcut(ctx, stream, first_token, "#,@", "unsyntax-splicing", read_syntax, labels, defining_label);
  else if (generic_literal* lit = std::get_if<generic_literal>(&first_token.value))
    return define_label_for_atomic_value(ctx, lit->value, labels, defining_label);
  else if (identifier* i = std::get_if<identifier>(&first_token.value))
    return define_label_for_atomic_value(ctx, ctx.intern(i->value), labels, defining_label);
  else if (boolean_literal* b = std::get_if<boolean_literal>(&first_token.value))
    return define_label_for_atomic_value(ctx,
                                         b->value ? ctx.constants->t.get() : ctx.constants->f.get(),
                                         labels,
                                         defining_label);
  else if (std::holds_alternative<void_literal>(first_token.value))
    return define_label_for_atomic_value(ctx, ctx.constants->void_.get(), labels, defining_label);
  else if (std::holds_alternative<dot>(first_token.value))
    throw read_error{"Unexpected . token", first_token.location};
  else if (std::holds_alternative<right_paren>(first_token.value))
    throw read_error{"Unexpected ) token", first_token.location};
  else if (datum_label_definition* dldef = std::get_if<datum_label_definition>(&first_token.value))
    return read(ctx, read_token(ctx, stream), stream, read_syntax, labels, dldef->label);
  else if (datum_label_reference* dlref = std::get_if<datum_label_reference>(&first_token.value))
    return find_datum_label_reference(labels, dlref->label, first_token.location);

  assert(false); // Unimplemented token
  throw read_error{"Unimplemented token", first_token.location};
}

ptr<>
read(context& ctx, ptr<textual_input_port> stream) {
  input_stream s{stream};
  datum_labels labels;
  return read(ctx, read_token(ctx, s), s, false, labels);
}

ptr<>
read(context& ctx, std::string s) {
  return read(ctx, make_string_input_port(ctx, std::move(s)));
}

static ptr<syntax>
read_syntax(context& ctx, input_stream& s) {
  datum_labels labels;
  if (ptr<> result = read_and_wrap(ctx, read_token(ctx, s), s, true, labels))
    return assume<syntax>(result);
  else
    return nullptr;
}

ptr<syntax>
read_syntax(context& ctx, ptr<textual_input_port> stream) {
  input_stream s{stream};
  return read_syntax(ctx, s);
}

ptr<syntax>
read_syntax(context& ctx, std::string s) {
  return read_syntax(ctx, make_string_input_port(ctx, std::move(s)));
}

std::vector<tracked_ptr<>>
read_multiple(context& ctx, ptr<textual_input_port> in) {
  std::vector<tracked_ptr<>> result;
  while (ptr<> elem = read(ctx, in))
    result.push_back(track(ctx, elem));

  return result;
}

std::vector<tracked_ptr<>>
read_multiple(context& ctx, std::string s) {
  return read_multiple(ctx, make_string_input_port(ctx, std::move(s)));
}

std::vector<tracked_ptr<syntax>>
read_syntax_multiple(context& ctx, ptr<textual_input_port> p) {
  input_stream in{p};
  std::vector<tracked_ptr<syntax>> result;
  while (ptr<syntax> elem = read_syntax(ctx, in))
    result.push_back(track(ctx, elem));

  return result;
}

std::vector<tracked_ptr<syntax>>
read_syntax_multiple(context& ctx, std::string s) {
  return read_syntax_multiple(ctx, make_string_input_port(ctx, std::move(s)));
}

} // namespace insider
