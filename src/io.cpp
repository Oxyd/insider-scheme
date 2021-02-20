#include "io.hpp"

#include "numeric.hpp"

#include <fmt/format.h>

#include <algorithm>
#include <limits>
#include <locale>
#include <sstream>
#include <variant>
#include <vector>

namespace insider {

namespace {
  struct end { };
  struct left_paren { };
  struct right_paren { };
  struct hash_left_paren { };
  struct dot { };

  struct generic_literal {
    object* value;
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
  struct hash_quote { };
  struct hash_backquote { };
  struct hash_comma { };
  struct hash_comma_at { };

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
      hash_quote,
      hash_backquote,
      hash_comma,
      hash_comma_at
    >;

    value_type      value;
    source_location location;
  };
} // anonymous namespace

parse_error::parse_error(std::string const& message, source_location const& loc)
  : std::runtime_error{fmt::format("{}: Parse error: {}", format_location(loc), message)}
{ }

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
skip_whitespace(input_stream& stream) {
  std::optional<char> c = stream.peek_char();

  while (c && (whitespace(*c) || *c == ';')) {
    while (c && whitespace(*c))
      c = stream.advance_and_peek_char();

    if (c == ';')
      while (stream.read_char() != '\n')
        ;

    c = stream.peek_char();
  }
}

static std::string
read_until_delimiter(input_stream& stream) {
  std::string result;
  while (stream.peek_char() && !delimiter(*stream.peek_char()))
    result += *stream.read_char();

  return result;
}

static bool
digit(char c) {
  return c >= '0' && c <= '9';
}

static bool
hexdigit(char c) {
  return digit(c) || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F');
}

static token
read_numeric_literal(context& ctx, input_stream& stream) {
  source_location loc = stream.current_location();
  return {generic_literal{read_number(ctx, stream)}, loc};
}

static token
read_character(context& ctx, input_stream& stream) {
  static std::unordered_map<std::string, char> const character_names{
    {"alarm",     '\x07'},
    {"backspace", '\x08'},
    {"delete",    '\x7F'},
    {"escape",    '\x1B'},
    {"newline",   '\x0A'},
    {"null",      '\x00'},
    {"return",    '\x0D'},
    {"space",     ' '},
    {"tab",       '\x09'}
  };

  if (stream.peek_char() != 'x') {
    source_location loc = stream.current_location();
    if (!std::isalpha(*stream.peek_char(), std::locale{"C"}))
      return {generic_literal{make<character>(ctx, *stream.read_char())}, loc};

    std::string literal = read_until_delimiter(stream);
    if (literal.size() == 1)
      return {generic_literal{make<character>(ctx, literal[0])}, loc};
    else if (auto it = character_names.find(literal); it != character_names.end())
      return {generic_literal{make<character>(ctx, it->second)}, loc};
    else
      throw parse_error{fmt::format("Unknown character literal #\\{}", literal), loc};
  }
  else {
    stream.read_char();
    source_location loc = stream.current_location();

    std::string literal;
    while (stream.peek_char() && hexdigit(*stream.peek_char()))
      literal += *stream.read_char();

    return {generic_literal{make<character>(ctx,
                                            static_cast<char>(expect<integer>(read_integer(ctx, literal, 16)).value()))},
            loc};
  }
}

static token
read_special_literal(context& ctx, input_stream& stream) {
  std::optional<char> c = stream.peek_char();
  if (!c)
    throw parse_error{"Unexpected end of input", stream.current_location()};

  source_location loc = stream.current_location();

  switch (*c) {
  case 't':
  case 'f': {
    // These can be either #t or #f, or #true or #false.
    std::string literal = read_until_delimiter(stream);

    if (literal != "t" && literal != "f" && literal != "true" && literal != "false")
      throw parse_error{fmt::format("Invalid literal: {}", literal), loc};

    if (c == 't')
      return {boolean_literal{true}, loc};
    else
      return {boolean_literal{false}, loc};
  }

  case 'v': {
    std::string literal = read_until_delimiter(stream);
    if (literal != "void")
      throw parse_error{fmt::format("Invalid literal: {}", literal), loc};

    return {void_literal{}, loc};
  }

  case '\\': {
    stream.read_char();
    return read_character(ctx, stream);
  }

  default:
    throw parse_error{"Unimplemented", loc};
  }
}

static token
read_identifier(input_stream& stream) {
  source_location loc = stream.current_location();
  std::string value;
  while (stream.peek_char() && !delimiter(*stream.peek_char()))
    value += *stream.read_char();

  return {identifier{std::move(value)}, loc};
}

static token
read_string_literal(context& ctx, input_stream& stream) {
  // The opening " was consumed before calling this function.

  source_location loc = stream.current_location();

  std::string result;
  while (true) {
    std::optional<char> c = stream.read_char();
    if (!c)
      throw parse_error{"Unexpected end of input", stream.current_location()};

    if (*c == '"')
      break;

    if (*c == '\\') {
      std::optional<char> escape = stream.read_char();
      if (!escape)
        throw parse_error{"Unexpected end of input", stream.current_location()};

      switch (*escape) {
      case 'a': result += '\a'; break;
      case 'n': result += '\n'; break;
      case 'r': result += '\r'; break;
      case '"': result += '"'; break;
      case '\\': result += '\\'; break;
      case '|': result += '|'; break;
      default:
        // XXX: Support \ at end of line and \xXXXX.
        throw parse_error{fmt::format("Unrecognised escape sequence \\{}", *escape), loc};
      }
    }
    else
      result += *c;
  }

  return {generic_literal{make_string(ctx, result)}, loc};
}

static token
read_token(context& ctx, input_stream& stream) {
  skip_whitespace(stream);

  source_location loc = stream.current_location();
  std::optional<char> c = stream.read_char();
  if (!c)
    return {end{}, loc};

  if (*c == '(') {
    return {left_paren{}, loc};
  } else if (*c == ')') {
    return {right_paren{}, loc};
  }
  else if (*c == '\'') {
    return {quote{}, loc};
  }
  else if (*c == '`') {
    return {backquote{}, loc};
  }
  else if (*c == ',') {
    c = stream.peek_char();
    if (c && *c == '@') {
      stream.read_char();
      return {comma_at{}, loc};
    }
    else
      return {comma{}, loc};
  }
  else if (*c == '.') {
    c = stream.peek_char();
    if (!c)
      throw parse_error{"Unexpected end of input", stream.current_location()};

    if (delimiter(*c))
      return {dot{}, loc};
    else {
      stream.put_back('.');

      if (digit(*c))
        return read_numeric_literal(ctx, stream);
      else
        return read_identifier(stream);
    }
  }
  else if (*c == '+' || *c == '-') {
    // This can begin either a number (like -2) or a symbol (like + -- the
    // addition function).

    char initial = *c;

    c = stream.peek_char();
    if (!c)
      return {identifier{std::string(1, initial)}, loc};

    stream.put_back(initial);
    if (digit(*c) || *c == '.')
      return read_numeric_literal(ctx, stream);
    else {
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
  }
  else if (digit(*c)) {
    stream.put_back(*c);
    return read_numeric_literal(ctx, stream);
  }
  else if (*c == '#') {
    c = stream.peek_char();
    if (!c)
      throw parse_error{"Unexpected end of input", stream.current_location()};
    else if (*c == '\'') {
      stream.read_char();
      return {hash_quote{}, loc};
    } else if (*c == '`') {
      stream.read_char();
      return {hash_backquote{}, loc};
    } else if (*c == ',') {
      stream.read_char();
      c = stream.peek_char();
      if (c == '@') {
        stream.read_char();
        return {hash_comma_at{}, loc};
      } else
        return {hash_comma{}, loc};
    } else if (*c == '$') {
      stream.put_back('#');
      return read_identifier(stream);
    }
    else if (*c == '(')
      return {hash_left_paren{}, loc};
    else
      return read_special_literal(ctx, stream);
  }
  else if (*c == '"')
    return read_string_literal(ctx, stream);
  else {
    stream.put_back(*c);
    return read_identifier(stream);
  }
}

static object*
wrap(context& ctx, object* value, source_location const& loc, bool read_syntax) {
  if (!value)
    return nullptr;

  if (read_syntax)
    return make<syntax>(ctx, value, loc);
  else
    return value;
}

static object*
read(context& ctx, token first_token, input_stream& stream, bool read_syntax);

static object*
read_and_wrap(context& ctx, token first_token, input_stream& stream, bool read_syntax) {
  return wrap(ctx, read(ctx, first_token, stream, read_syntax), first_token.location, read_syntax);
}

static object*
read_list(context& ctx, input_stream& stream, bool read_syntax) {
  token t = read_token(ctx, stream);
  if (std::holds_alternative<end>(t.value))
    throw parse_error{"Unterminated list", t.location};
  else if (std::holds_alternative<dot>(t.value))
    throw parse_error{"Unexpected . token", t.location};
  else if (std::holds_alternative<right_paren>(t.value))
    return ctx.constants->null.get();

  pair* result = make<pair>(ctx, read_and_wrap(ctx, t, stream, read_syntax), ctx.constants->null.get());
  pair* tail = result;

  t = read_token(ctx, stream);
  while (!std::holds_alternative<end>(t.value)
         && !std::holds_alternative<right_paren>(t.value)
         && !std::holds_alternative<dot>(t.value)) {
    pair* new_tail = make<pair>(ctx, read_and_wrap(ctx, t, stream, read_syntax), ctx.constants->null.get());
    tail->set_cdr(ctx.store, new_tail);
    tail = new_tail;

    t = read_token(ctx, stream);
  }

  if (std::holds_alternative<end>(t.value))
    throw parse_error{"Unterminated list", t.location};
  else if (std::holds_alternative<dot>(t.value)) {
    t = read_token(ctx, stream);
    object* cdr = read_and_wrap(ctx, t, stream, read_syntax);
    tail->set_cdr(ctx.store, cdr);

    t = read_token(ctx, stream);
    if (!std::holds_alternative<right_paren>(t.value))
      throw parse_error{"Too many elements after .", t.location};
  }

  assert(std::holds_alternative<right_paren>(t.value));
  return result;
}

static object*
read_vector(context& ctx, input_stream& stream, bool read_syntax) {
  stream.read_char(); // Consume (

  std::vector<object*> elements;

  token t = read_token(ctx, stream);
  while (!std::holds_alternative<end>(t.value) && !std::holds_alternative<right_paren>(t.value)) {
    elements.push_back(read_and_wrap(ctx, t, stream, read_syntax));
    t = read_token(ctx, stream);
  }

  if (std::holds_alternative<end>(t.value))
    throw parse_error{"Unterminated vector", t.location};

  vector* result = make<vector>(ctx, ctx, elements.size());
  for (std::size_t i = 0; i < elements.size(); ++i)
    result->set(ctx.store, i, elements[i]);

  return result;
}

static object*
read_shortcut(context& ctx, input_stream& stream, token shortcut_token,
              std::string const& shortcut, std::string const& expansion,
              bool read_syntax) {
  token t = read_token(ctx, stream);
  if (std::holds_alternative<end>(t.value))
    throw parse_error{fmt::format("Expected token after {}", shortcut), t.location};

  return make_list(ctx, wrap(ctx, ctx.intern(expansion), shortcut_token.location, read_syntax),
                   read_and_wrap(ctx, t, stream, read_syntax));
}

static object*
read(context& ctx, token first_token, input_stream& stream, bool read_syntax) {
  if (std::holds_alternative<end>(first_token.value))
    return {};
  else if (std::holds_alternative<left_paren>(first_token.value))
    return read_list(ctx, stream, read_syntax);
  else if (std::holds_alternative<hash_left_paren>(first_token.value))
    return read_vector(ctx, stream, read_syntax);
  else if (std::holds_alternative<quote>(first_token.value))
    return read_shortcut(ctx, stream, first_token, "'", "quote", read_syntax);
  else if (std::holds_alternative<hash_quote>(first_token.value))
    return read_shortcut(ctx, stream, first_token, "#'", "syntax", read_syntax);
  else if (std::holds_alternative<backquote>(first_token.value))
    return read_shortcut(ctx, stream, first_token, "`", "quasiquote", read_syntax);
  else if (std::holds_alternative<hash_backquote>(first_token.value))
    return read_shortcut(ctx, stream, first_token, "#`", "quasisyntax", read_syntax);
  else if (std::holds_alternative<comma>(first_token.value))
    return read_shortcut(ctx, stream, first_token, ",", "unquote", read_syntax);
  else if (std::holds_alternative<hash_comma>(first_token.value))
    return read_shortcut(ctx, stream, first_token, "#,", "unsyntax", read_syntax);
  else if (std::holds_alternative<comma_at>(first_token.value))
    return read_shortcut(ctx, stream, first_token, ",@", "unquote-splicing", read_syntax);
  else if (std::holds_alternative<hash_comma_at>(first_token.value))
    return read_shortcut(ctx, stream, first_token, "#,@", "unsyntax-splicing", read_syntax);
  else if (generic_literal* lit = std::get_if<generic_literal>(&first_token.value))
    return lit->value;
  else if (identifier* i = std::get_if<identifier>(&first_token.value))
    return ctx.intern(i->value);
  else if (boolean_literal* b = std::get_if<boolean_literal>(&first_token.value))
    return b->value ? ctx.constants->t.get() : ctx.constants->f.get();
  else if (std::holds_alternative<void_literal>(first_token.value))
    return ctx.constants->void_.get();
  else if (std::holds_alternative<dot>(first_token.value))
    throw parse_error{"Unexpected . token", first_token.location};
  else if (std::holds_alternative<right_paren>(first_token.value))
    throw parse_error{"Unexpected ) token", first_token.location};

  throw parse_error{"Probably unimplemented", first_token.location};
}

object*
read(context& ctx, port* stream) {
  input_stream s{stream};
  return read(ctx, read_token(ctx, s), s, false);
}

object*
read(context& ctx, std::string s) {
  auto port = make<insider::port>(ctx, std::move(s), true, false);
  return read(ctx, port);
}

syntax*
read_syntax(context& ctx, port* stream) {
  input_stream s{stream};
  if (object* result = read_and_wrap(ctx, read_token(ctx, s), s, true))
    return assume<syntax>(result);
  else
    return nullptr;
}

syntax*
read_syntax(context& ctx, std::string s) {
  auto port = make<insider::port>(ctx, std::move(s), true, false);
  return read_syntax(ctx, port);
}

std::vector<generic_tracked_ptr>
read_multiple(context& ctx, port* in) {
  std::vector<generic_tracked_ptr> result;
  while (object* elem = read(ctx, in))
    result.push_back(track(ctx, elem));

  return result;
}

std::vector<generic_tracked_ptr>
read_multiple(context& ctx, std::string s) {
  auto port = make<insider::port>(ctx, std::move(s), true, false);
  return read_multiple(ctx, port);
}

std::vector<tracked_ptr<syntax>>
read_syntax_multiple(context& ctx, port* in) {
  std::vector<tracked_ptr<syntax>> result;
  while (syntax* elem = read_syntax(ctx, in))
    result.push_back(track(ctx, elem));

  return result;
}

std::vector<tracked_ptr<syntax>>
read_syntax_multiple(context& ctx, std::string s) {
  auto port = make<insider::port>(ctx, std::move(s), true, false);
  return read_syntax_multiple(ctx, port);
}

static void
write_string(string* s, port* out) {
  out->write_char('"');
  for (char c : s->value())
    if (c == '"')
      out->write_string(R"(\")");
    else if (c == '\\')
      out->write_string(R"(\\)");
    else
      out->write_char(c);
  out->write_char('"');
}

static void
write_char(character* c, port* out) {
  out->write_string(R"(#\)");
  out->write_char(c->value());
}

static void
output_primitive(context& ctx, object* datum, port* out, bool display) {
  if (datum == ctx.constants->null.get())
    out->write_string("()");
  else if (datum == ctx.constants->void_.get())
    out->write_string("#void");
  else if (datum == ctx.constants->t.get())
    out->write_string("#t");
  else if (datum == ctx.constants->f.get())
    out->write_string("#f");
  else if (auto sym = match<symbol>(datum))
    out->write_string(sym->value());
  else if (auto str = match<string>(datum)) {
    if (display)
      out->write_string(str->value());
    else
      write_string(str, out);
  } else if (auto c = match<character>(datum)) {
    if (display)
      out->write_char(c->value());
    else
      write_char(c, out);
  } else if (is_number(datum)) {
    write_number(ctx, datum, out);
  } else if (auto stx = match<syntax>(datum)) {
    out->write_string("#<syntax ");
    out->write_string(format_location(stx->location()));
    out->write_char(' ');
    write_simple(ctx, syntax_to_datum(ctx, stx), out);
    out->write_string(">");
  } else if (auto env = match<scope>(datum)) {
    out->write_string(fmt::format("#env@{}", static_cast<void*>(env)));
  } else if (auto proc = match<procedure>(datum)) {
    if (proc->name)
      out->write_string(fmt::format("<procedure {}>", *proc->name));
    else
      out->write_string("<lambda>");
  } else if (auto core = match<core_form_type>(datum)) {
    out->write_string(fmt::format("<core form {}>", core->name));
  } else
    out->write_string(fmt::format("<{}>", object_type_name(datum)));
}

static void
output_simple(context& ctx, object* datum, port* out, bool display) {
  struct record {
    object*     datum;
    std::size_t written = 0;
    bool        omit_parens = false;
  };
  std::vector<record> stack{{datum}};

  while (!stack.empty()) {
    record& top = stack.back();

    if (auto pair = match<insider::pair>(top.datum)) {
      switch (top.written) {
      case 0:
        if (!top.omit_parens)
          out->write_char('(');
        ++top.written;
        stack.push_back({car(pair)});
        break;

      case 1:
        ++top.written;

        if (is<insider::pair>(cdr(pair))) {
          out->write_char(' ');
          stack.push_back({cdr(pair), 0, true});
        } else if (cdr(pair) == ctx.constants->null.get()) {
          if (!top.omit_parens)
            out->write_char(')');
          stack.pop_back();
        } else {
          out->write_string(" . ");
          stack.push_back({cdr(pair)});
        }
        break;

      case 2:
        if (!top.omit_parens)
          out->write_char(')');
        stack.pop_back();
      }
    }
    else if (auto vec = match<vector>(top.datum)) {
      if (top.written == 0) {
        out->write_string("#(");
      }

      if (top.written == vec->size()) {
        out->write_char(')');
        stack.pop_back();
        continue;
      }

      if (top.written != 0 && top.written != vec->size())
        out->write_char(' ');

      std::size_t index = top.written++;
      stack.push_back({vec->ref(index)});
    }
    else {
      output_primitive(ctx, top.datum, out, display);
      stack.pop_back();
    }
  }
}

void
write_simple(context& ctx, object* datum, port* out) {
  output_simple(ctx, datum, out, false);
}

void
display(context& ctx, object* datum, port* out) {
  output_simple(ctx, datum, out, true);
}

std::string
datum_to_string(context& ctx, object* datum) {
  auto p = make<port>(ctx, "", false, true);
  write_simple(ctx, datum, p);
  return p->get_string();
}

} // namespace insider
