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

struct end { };
struct left_paren { };
struct right_paren { };
struct hash_left_paren { };
struct dot { };

struct generic_literal {
  generic_ptr value;
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

using token = std::variant<
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
  comma_at
>;

parse_error::parse_error(std::string const& message)
  : std::runtime_error{fmt::format("Parse error: {}", message)}
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
skip_whitespace(ptr<port> const& stream) {
  std::optional<char> c = stream->peek_char();

  while (c && (whitespace(*c) || *c == ';')) {
    while (c && whitespace(*c)) {
      stream->read_char();
      c = stream->peek_char();
    }

    if (c == ';')
      while (stream->read_char() != '\n')
        ;

    c = stream->peek_char();
  }
}

static std::string
read_until_delimiter(ptr<port> const& stream) {
  std::string result;
  while (stream->peek_char() && !delimiter(*stream->peek_char()))
    result += *stream->read_char();

  return result;
}

static bool
digit(char c) {
  return c >= '0' && c <= '9';
}

static generic_literal
read_numeric_literal(context& ctx, ptr<port> const& stream) {
  return generic_literal{read_number(ctx, stream)};
}

static token
read_special_literal(ptr<port> const& stream) {
  std::optional<char> c = stream->peek_char();
  if (!c)
    throw parse_error{"Unexpected end of input"};

  switch (*c) {
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

  case 'v': {
    std::string literal = read_until_delimiter(stream);
    if (literal != "void")
      throw parse_error{fmt::format("Invalid literal: {}", literal)};

    return void_literal{};
  }

  default:
    throw parse_error{"Unimplemented"};
  }
}

static identifier
read_identifier(ptr<port> const& stream) {
  std::string value;
  while (stream->peek_char() && !delimiter(*stream->peek_char()))
    value += *stream->read_char();

  return identifier{std::move(value)};
}

static generic_literal
read_string_literal(context& ctx, ptr<port> const& stream) {
  // The opening " was consumed before calling this function.

  std::string result;
  while (true) {
    std::optional<char> c = stream->read_char();
    if (!c)
      throw parse_error{"Unexpected end of input"};

    if (*c == '"')
      break;

    if (*c == '\\') {
      std::optional<char> escape = stream->read_char();
      if (!escape)
        throw parse_error{"Unexpected end of input"};

      switch (*escape) {
      case 'a': result += '\a'; break;
      case 'n': result += '\n'; break;
      case 'r': result += '\r'; break;
      case '"': result += '"'; break;
      case '\\': result += '\\'; break;
      case '|': result += '|'; break;
      default:
        // XXX: Support \ at end of line and \xXXXX.
        throw parse_error{fmt::format("Unrecognised escape sequence \\{}", *escape)};
      }
    }
    else
      result += *c;
  }

  return generic_literal{make_string(ctx, result)};
}

static token
read_token(context& ctx, ptr<port> const& stream) {
  skip_whitespace(stream);

  std::optional<char> c = stream->read_char();
  if (!c)
    return end{};

  if (*c == '(') {
    return left_paren{};
  } else if (*c == ')') {
    return right_paren{};
  }
  else if (*c == '\'') {
    return quote{};
  }
  else if (*c == '`') {
    return backquote{};
  }
  else if (*c == ',') {
    c = stream->peek_char();
    if (c && *c == '@') {
      stream->read_char();
      return comma_at{};
    }
    else
      return comma{};
  }
  else if (*c == '.') {
    c = stream->peek_char();
    if (!c)
      throw parse_error{"Unexpected end of input"};

    if (delimiter(*c))
      return dot{};
    else {
      stream->put_back('.');

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

    c = stream->peek_char();
    if (!c)
      return identifier{std::string(1, initial)};

    stream->put_back(initial);
    if (digit(*c) || *c == '.')
      return read_numeric_literal(ctx, stream);
    else {
      identifier id = read_identifier(stream);

      // Infinities and NaNs look like identifiers, but they're numbers.

      std::string value;
      std::locale c_locale{"C"};
      std::transform(id.value.begin(), id.value.end(), std::back_inserter(value),
                     [&] (char c) { return std::tolower(c, c_locale); });

      if (value == "+inf.0")
        return generic_literal{make<floating_point>(ctx, floating_point::positive_infinity)};
      else if (value == "-inf.0")
        return generic_literal{make<floating_point>(ctx, floating_point::negative_infinity)};
      else if (value == "+nan.0")
        return generic_literal{make<floating_point>(ctx, floating_point::positive_nan)};
      else if (value == "-nan.0")
        return generic_literal{make<floating_point>(ctx, floating_point::negative_nan)};

      return id;
    }
  }
  else if (digit(*c)) {
    stream->put_back(*c);
    return read_numeric_literal(ctx, stream);
  }
  else if (*c == '#') {
    c = stream->peek_char();
    if (!c)
      throw parse_error{"Unexpected end of input"};
    else if (*c == '$') {
      stream->put_back('#');
      return read_identifier(stream);
    }
    else if (*c == '(')
      return hash_left_paren{};
    else
      return read_special_literal(stream);
  }
  else if (*c == '"')
    return read_string_literal(ctx, stream);
  else {
    stream->put_back(*c);
    return read_identifier(stream);
  }
}

static generic_ptr
read(context& ctx, token first_token, ptr<port> const& stream);

static generic_ptr
read_list(context& ctx, ptr<port> const& stream) {
  token t = read_token(ctx, stream);
  if (std::holds_alternative<end>(t))
    throw parse_error{"Unterminated list"};
  else if (std::holds_alternative<dot>(t))
    throw parse_error{"Unexpected . token"};
  else if (std::holds_alternative<right_paren>(t))
    return ctx.constants->null;

  ptr<pair> result = make<pair>(ctx, read(ctx, t, stream), ctx.constants->null);
  ptr<pair> tail = result;

  t = read_token(ctx, stream);
  while (!std::holds_alternative<end>(t)
         && !std::holds_alternative<right_paren>(t)
         && !std::holds_alternative<dot>(t)) {
    ptr<pair> new_tail = make<pair>(ctx, read(ctx, t, stream), ctx.constants->null);
    tail->set_cdr(new_tail);
    tail = new_tail;

    t = read_token(ctx, stream);
  }

  if (std::holds_alternative<end>(t))
    throw parse_error{"Unterminated list"};
  else if (std::holds_alternative<dot>(t)) {
    generic_ptr cdr = read(ctx, read_token(ctx, stream), stream);
    tail->set_cdr(cdr);

    t = read_token(ctx, stream);
    if (!std::holds_alternative<right_paren>(t))
      throw parse_error{"Too many elements after ."};
  }

  assert(std::holds_alternative<right_paren>(t));
  return result;
}

static generic_ptr
read_vector(context& ctx, ptr<port> const& stream) {
  stream->read_char(); // Consume (

  std::vector<generic_ptr> elements;

  token t = read_token(ctx, stream);
  while (!std::holds_alternative<end>(t) && !std::holds_alternative<right_paren>(t)) {
    elements.push_back(read(ctx, t, stream));
    t = read_token(ctx, stream);
  }

  if (std::holds_alternative<end>(t))
    throw parse_error{"Unterminated vector"};

  ptr<vector> result = make<vector>(ctx, elements.size());
  for (std::size_t i = 0; i < elements.size(); ++i)
    result->set(i, elements[i]);

  return result;
}

static generic_ptr
read_shortcut(context& ctx, ptr<port> const& stream,
              std::string const& shortcut, std::string const& expansion) {
  token t = read_token(ctx, stream);
  if (std::holds_alternative<end>(t))
    throw parse_error{fmt::format("Expected token after {}", shortcut)};

  return make_list(ctx, ctx.intern(expansion), read(ctx, t, stream));
}

static generic_ptr
read(context& ctx, token first_token, ptr<port> const& stream) {
  if (std::holds_alternative<end>(first_token))
    return {};
  else if (std::holds_alternative<left_paren>(first_token))
    return read_list(ctx, stream);
  else if (std::holds_alternative<hash_left_paren>(first_token))
    return read_vector(ctx, stream);
  else if (std::holds_alternative<quote>(first_token))
    return read_shortcut(ctx, stream, "'", "quote");
  else if (std::holds_alternative<backquote>(first_token))
    return read_shortcut(ctx, stream, "`", "quasiquote");
  else if (std::holds_alternative<comma>(first_token))
    return read_shortcut(ctx, stream, ",", "unquote");
  else if (std::holds_alternative<comma_at>(first_token))
    return read_shortcut(ctx, stream, ",@", "unquote-splicing");
  else if (generic_literal* lit = std::get_if<generic_literal>(&first_token))
    return lit->value;
  else if (identifier* i = std::get_if<identifier>(&first_token))
    return ctx.intern(i->value);
  else if (boolean_literal* b = std::get_if<boolean_literal>(&first_token))
    return b->value ? ctx.constants->t : ctx.constants->f;
  else if (std::holds_alternative<void_literal>(first_token))
    return ctx.constants->void_;
  else if (std::holds_alternative<dot>(first_token))
    throw parse_error{"Unexpected . token"};
  else if (std::holds_alternative<right_paren>(first_token))
    throw parse_error{"Unexpected ) token"};

  throw parse_error{"Probably unimplemented"};
}

generic_ptr
read(context& ctx, ptr<port> const& stream) {
  return read(ctx, read_token(ctx, stream), stream);
}

generic_ptr
read(context& ctx, std::string s) {
  auto port = make<insider::port>(ctx, std::move(s), true, false);
  return read(ctx, port);
}

std::vector<generic_ptr>
read_multiple(context& ctx, ptr<port> const& in) {
  std::vector<generic_ptr> result;
  while (generic_ptr elem = read(ctx, in))
    result.push_back(elem);

  return result;
}

std::vector<generic_ptr>
read_multiple(context& ctx, std::string s) {
  auto port = make<insider::port>(ctx, std::move(s), true, false);
  return read_multiple(ctx, port);
}

static void
write_string(ptr<string> const& s, ptr<port> const& out) {
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
write_char(ptr<character> const& c, ptr<port> const& out) {
  out->write_string(R"(#\)");
  out->write_char(c->value());
}

static void
write_primitive(context& ctx, generic_ptr const& datum, ptr<port> const& out) {
  if (datum == ctx.constants->null)
    out->write_string("()");
  else if (datum == ctx.constants->void_)
    out->write_string("#void");
  else if (datum == ctx.constants->t)
    out->write_string("#t");
  else if (datum == ctx.constants->f)
    out->write_string("#f");
  else if (auto sym = match<symbol>(datum))
    out->write_string(sym->value());
  else if (auto str = match<string>(datum))
    write_string(str, out);
  else if (auto c = match<character>(datum))
    write_char(c, out);
  else if (is_number(datum))
    write_number(ctx, datum, out);
  else if (auto sc = match<syntactic_closure>(datum)) {
    out->write_string("#syntactic-closure(");
    write_simple(ctx, syntactic_closure_environment(sc), out);
    out->write_char(' ');

    std::vector<ptr<symbol>> free = syntactic_closure_free(sc);
    out->write_char('(');

    for (auto it = free.begin(); it != free.end(); ++it) {
      if (it != free.begin())
        out->write_char(' ');

      out->write_string((**it).value());
    }

    out->write_string(") ");

    write_simple(ctx, syntactic_closure_expression(sc), out);
    out->write_char(')');
  } else if (auto env = match<environment>(datum)) {
    out->write_string(fmt::format("#env@{}", static_cast<void*>(env.get())));
  } else
    out->write_string(typeid(*datum).name());
}

void
write_simple(context& ctx, generic_ptr const& datum, ptr<port> const& out) {
  struct record {
    generic_ptr datum;
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
        } else if (cdr(pair) == ctx.constants->null) {
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
      } else if (top.written == vec->size()) {
        out->write_char(')');
        stack.pop_back();
        continue;
      }
      else
        out->write_char(' ');

      std::size_t index = top.written++;
      stack.push_back({vector_ref(vec, index)});
    }
    else {
      write_primitive(ctx, top.datum, out);
      stack.pop_back();
    }
  }
}

} // namespace insider
