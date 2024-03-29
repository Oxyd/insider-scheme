#include "write.hpp"

#include "compiler/source_location.hpp"
#include "context.hpp"
#include "io/char_categories.hpp"
#include "io/port.hpp"
#include "runtime/basic_types.hpp"
#include "runtime/compare.hpp"
#include "runtime/numeric.hpp"
#include "runtime/records.hpp"
#include "runtime/string.hpp"
#include "runtime/symbol.hpp"
#include "runtime/syntax.hpp"
#include "util/define_procedure.hpp"
#include "util/list_iterator.hpp"
#include "vm/vm.hpp"

#include <fmt/format.h>

#include <algorithm>
#include <cctype>
#include <iomanip>
#include <limits>
#include <locale>
#include <optional>
#include <ranges>
#include <sstream>
#include <stdexcept>
#include <string>
#include <variant>
#include <vector>

namespace insider {

static void
write_string(ptr<string> s, ptr<textual_output_port> out) {
  out->write('"');
  for (char c : s->value())
    if (c == '"')
      out->write(R"(\")");
    else if (c == '\\')
      out->write(R"(\\)");
    else if (c == '\n')
      out->write(R"(\n)");
    else if (c == '\r')
      out->write(R"(\r)");
    else
      out->write_utf8(c);
  out->write('"');
}

static void
write_escaped_char(char32_t c, ptr<textual_output_port> out) {
  static std::unordered_map<char32_t, std::string> const character_names{
    {'\x07', "alarm"},
    {'\x08', "backspace"},
    {'\x7F', "delete"},
    {'\x1B', "escape"},
    {'\x0A', "newline"},
    {'\x00', "null"},
    {'\x0D', "return"},
    {' ',    "space"},
    {'\x09', "tab"}
  };

  out->write(R"(#\)");

  if (auto named = character_names.find(c); named != character_names.end())
    out->write(named->second);
  else
    out->write(c);
}

static void
write_number(context& ctx, ptr<> value, ptr<textual_output_port> out,
             unsigned base = 10);

static char
digit_to_letter(unsigned d) {
  if (d <= 9)
    return static_cast<char>('0' + d);
  else if (d <= 16)
    return static_cast<char>('a' + (d - 10));

  assert(false);
  return {};
}

template <typename T>
static void
write_small_magnitude(std::string& buffer, T n, unsigned base) {
  while (n > 0) {
    T quot = n / base;
    T rem = n % base;
    buffer.push_back(digit_to_letter(static_cast<unsigned>(rem)));
    n = quot;
  }
}

static void
write_small(integer value, ptr<textual_output_port> out, unsigned base) {
  if (value.value() == 0) {
    out->write('0');
    return;
  }

  if (value.value() < 0)
    out->write('-');

  std::string buffer;
  integer::value_type n = value.value() >= 0 ? value.value() : -value.value();
  write_small_magnitude(buffer, n, base);

  for (auto c = buffer.rbegin(), e = buffer.rend(); c != e; ++c)
    out->write(static_cast<char32_t>(*c));
}

static void
write_big(context& ctx, ptr<big_integer> value, ptr<textual_output_port> out,
          unsigned base) {
  if (value->zero()) {
    out->write('0');
    return;
  }

  ptr<> v = value;
  if (!value->positive()) {
    out->write('-');
    v = negate(ctx, value);
  }

  std::string buffer;
  while (is<big_integer>(v)) {
    auto [quot, rem] = quotient_remainder(ctx, v, integer_to_ptr(base));
    buffer.push_back(
      digit_to_letter(static_cast<unsigned>(assume<integer>(rem).value()))
    );
    v = quot;
  }

  write_small_magnitude(buffer, assume<integer>(v).value(), base);

  for (auto c = buffer.rbegin(), e = buffer.rend(); c != e; ++c)
    out->write(static_cast<char32_t>(*c));
}

static void
write_fraction(context& ctx, ptr<fraction> value, ptr<textual_output_port> out,
               unsigned base) {
  write_number(ctx, value->numerator(), out, base);
  out->write('/');
  write_number(ctx, value->denominator(), out, base);
}

static bool
maybe_write_infinite_float(ptr<floating_point> value,
                           ptr<textual_output_port> out) {
  if (value->value == floating_point::positive_infinity) {
    out->write("+inf.0");
    return true;
  } else if (value->value == floating_point::negative_infinity) {
    out->write("-inf.0");
    return true;
  } else if (std::isnan(value->value)) {
    if (std::signbit(value->value))
      out->write('-');
    else
      out->write('+');

    out->write("nan.0");
    return true;
  }
  return false;
}

static std::string
maybe_append_dot_zero(std::string const& s) {
  if (s.find('.') == std::string::npos && s.find('e') == std::string::npos)
    return s + ".0";
  else
    return s;
}

static std::string
finite_float_to_string(ptr<floating_point> value, int width) {
  std::ostringstream os;
  os.imbue(std::locale{"C"});

  os << std::setprecision(width) << value->value;
  return maybe_append_dot_zero(os.str());
}

static bool
lost_precision(floating_point::value_type value, std::string const& str) {
  std::istringstream is{str};
  is.imbue(std::locale{"C"});

  floating_point::value_type read_back;
  is >> read_back;

  return !is || value != read_back;
}

static void
write_finite_float(ptr<floating_point> value, ptr<textual_output_port> out) {
  // It's hard to predict how much precision we're going to need, so instead
  // we'll just go from 15 to 17 until we get a precise result.

  for (int width = 15; width <= 17; ++width) {
    std::string candidate = finite_float_to_string(value, width);
    if (!lost_precision(value->value, candidate)) {
      out->write(candidate);
      return;
    }
  }

  assert(false);
}

static void
write_float(ptr<floating_point> value, ptr<textual_output_port> out) {
  bool written = maybe_write_infinite_float(value, out);
  if (!written)
    write_finite_float(value, out);
}

template <int I>
static bool
is_exact_equal(ptr<> x) {
  return is_exact_integer(x) && is<integer>(x)
         && assume<integer>(x).value() == I;
}

static void
write_complex(context& ctx, ptr<complex> z, ptr<textual_output_port> out,
              unsigned base) {
  if (!is_exact_equal<0>(z->real()))
    write_number(ctx, z->real(), out, base);

  if (is_exact_equal<1>(z->imaginary()))
    out->write("+i");
  else if (is_exact_equal<-1>(z->imaginary()))
    out->write("-i");
  else {
    if (!is_negative(z->imaginary()) && !is_exact_equal<0>(z->real())
        && is_finite(z->imaginary()))
      out->write('+');

    write_number(ctx, z->imaginary(), out, base);
    out->write('i');
  }
}

static void
write_number(context& ctx, ptr<> value, ptr<textual_output_port> out,
             unsigned base) {
  if (auto s = match<integer>(value))
    write_small(*s, out, base);
  else if (auto b = match<big_integer>(value))
    write_big(ctx, b, out, base);
  else if (auto q = match<fraction>(value))
    write_fraction(ctx, q, out, base);
  else if (auto f = match<floating_point>(value))
    write_float(f, out);
  else if (auto z = match<complex>(value))
    write_complex(ctx, z, out, base);
  else
    assert(false);
}

static bool
is_number_prefix_character(char c) {
  return c == '.' || c == '-' || c == '+';
}

static bool
has_number_as_prefix(std::string const& sym_value) {
  assert(!sym_value.empty());
  std::string s;
  std::ranges::copy(
    sym_value | std::views::transform([] (char c) {
      return static_cast<char>(std::tolower(c));
    }),
    std::back_inserter(s)
  );

  return std::isdigit(s.front())
         || (is_number_prefix_character(s.front()) && std::isdigit(s[2]))
         || s.starts_with("+i") || s.starts_with("-i")
         || s.starts_with("+nan.0") || s.starts_with("-nan.0")
         || s.starts_with("+inf.0") || s.starts_with("-inf.0");
}

static bool
symbol_contains_character_that_requires_pipes(std::string const& s) {
  return std::ranges::any_of(
    s,
    [] (char c) {
      return control(c) || delimiter(c) || c == '\\' || c == '|';
    }
  );
}

static bool
symbol_requires_pipes(std::string const& sym) {
  using namespace std::literals;
  return sym.empty()
         || sym == "."s
         || has_number_as_prefix(sym)
         || symbol_contains_character_that_requires_pipes(sym);
}

static bool
keyword_requires_pipes(std::string const& kw) {
  using namespace std::literals;
  return kw.empty()
         || kw == "."s
         || symbol_contains_character_that_requires_pipes(kw);
}

static std::string
escape_symbol(std::string const& s) {
  std::string result;
  result.reserve(s.size());
  for (char c : s) {
    if (c == '|' || c == '\\')
      result += '\\';
    result += c;
  }
  return result;
}

static void
write_symbol(ptr<textual_output_port> const& out, ptr<symbol> sym) {
  if (symbol_requires_pipes(sym->value()))
    out->write(fmt::format("|{}|", escape_symbol(sym->value())));
  else
    out->write(sym->value());
}

static void
write_keyword(ptr<textual_output_port> out, ptr<keyword> kw) {
  if (keyword_requires_pipes(kw->value()))
    out->write(fmt::format("#:|{}|", escape_symbol(kw->value())));
  else
    out->write(fmt::format("#:{}", kw->value()));
}

static void
write_primitive(context& ctx, ptr<> datum, ptr<textual_output_port> out) {
  if (datum == ctx.constants->null)
    out->write("()");
  else if (datum == ctx.constants->void_)
    out->write("#void");
  else if (datum == ctx.constants->t)
    out->write("#t");
  else if (datum == ctx.constants->f)
    out->write("#f");
  else if (datum == ctx.constants->default_value)
    out->write("#default-value");
  else if (is_number(datum))
    write_number(ctx, datum, out);
  else if (auto sym = match<symbol>(datum))
    write_symbol(out, sym);
  else if (auto kw = match<keyword>(datum))
    write_keyword(out, kw);
  else if (auto bv = match<bytevector>(datum)) {
    out->write("#u8(");
    for (std::size_t i = 0; i < bv->size(); ++i) {
      if (i != 0)
        out->write(' ');
      write_small(integer{bv->ref(i)}, out, 10);
    }
    out->write(")");
  } else if (auto stx = match<syntax>(datum)) {
    out->write("#<syntax ");
    out->write(format_location(stx->location()));
    out->write(' ');
    write_simple(ctx, syntax_to_datum(ctx, stx), out);
    out->write(">");
  } else if (auto proto = match<procedure_prototype>(datum))
    out->write(fmt::format("<procedure prototype {}>", proto->info.name));
  else if (auto np = match<native_procedure>(datum)) {
    out->write(fmt::format("<native procedure {}>", np->name));
  } else if (auto proc = match<procedure>(datum))
    out->write(fmt::format("<procedure {}>", proc->prototype()->info.name));
  else if (auto core = match<core_form_type>(datum)) {
    out->write(fmt::format("<core form {}>", core->name));
  } else if (auto e = match<error>(datum)) {
    out->write(fmt::format("<error: {} {}>", e->message(ctx)->value(),
                           datum_to_string(ctx, e->irritants(ctx))));
  } else if (auto fe = match<file_error>(datum)) {
    out->write(fmt::format("<file error: {}>", fe->message()));
  } else if (auto v = match<values_tuple>(datum)) {
    out->write("<values");
    for (std::size_t i = 0; i < v->size(); ++i) {
      out->write(' ');
      write_simple(ctx, v->ref(i), out);
    }
    out->write(">");
  } else if (auto inst = match<record_instance>(datum)) {
    auto type = inst->type();
    out->write(fmt::format("<{} ", type->name()->value()));

    for (std::size_t i = 0; i < type->num_fields(); ++i) {
      if (i != 0)
        out->write(' ');
      write_simple(ctx, inst->ref(i), out);
    }

    out->write('>');
  } else
    out->write(fmt::format("<{}>", object_type_name(datum)));
}

static void
write_atomic(context& ctx, ptr<> datum, ptr<textual_output_port> out) {
  if (auto str = match<string>(datum))
    write_string(str, out);
  else if (auto c = match<char32_t>(datum))
    write_escaped_char(*c, out);
  else
    write_primitive(ctx, datum, out);
}

static void
display_list(context& ctx, ptr<> lst, ptr<textual_output_port> out) {
  bool first = true;
  for (ptr<> elem : list_range{lst}) {
    if (!first)
      out->write(' ');

    display(ctx, elem, out);
    first = false;
  }
}

static void
display_error(context& ctx, ptr<error> e, ptr<textual_output_port> out) {
  out->write(e->message(ctx)->value());

  if (e->irritants(ctx) != ctx.constants->null) {
    out->write(": ");
    display_list(ctx, e->irritants(ctx), out);
  }
}

static void
display_atomic(context& ctx, ptr<> datum, ptr<textual_output_port> out) {
  if (auto str = match<string>(datum))
    out->write(str->value());
  else if (auto c = match<char32_t>(datum))
    out->write(*c);
  else if (auto e = match<error>(datum))
    display_error(ctx, e, out);
  else
    write_primitive(ctx, datum, out);
}

namespace {
  class output_datum_labels {
  public:
    bool
    is_shared(ptr<> x) const { return labels_.count(x) != 0u; }

    void
    mark_shared(ptr<> x) { labels_.emplace(x, std::optional<std::size_t>{}); }

    bool
    has_label(ptr<> x) const {
      if (auto it = labels_.find(x); it != labels_.end())
        return it->second.has_value();
      else
        return false;
    }

    std::size_t
    get_or_assign_label(ptr<> x) {
      auto& l = labels_[x];
      if (!l)
        l.emplace(next_label_++);

      return *l;
    }

  private:
    eq_unordered_map<ptr<>, std::optional<std::size_t>> labels_;
    std::size_t next_label_ = 0;
  };

  struct find_shared_result {
    output_datum_labels labels;
    bool                has_cycle{};
  };
}

static bool
is_shareable(ptr<> x) {
  return is<pair>(x) || is<vector>(x);
}

static find_shared_result
find_shared(ptr<> datum) {
  output_datum_labels labels;
  eq_unordered_set<ptr<>> seen;
  eq_unordered_set<ptr<>> open;
  bool cycle = false;

  struct record {
    ptr<> datum;
    bool entered = false;
  };
  std::vector<record> stack{{datum}};

  auto enter = [&] (record& current) {
    current.entered = true;
    open.emplace(current.datum);
  };

  auto leave = [&] (record& current) {
    open.erase(current.datum);
  };

  while (!stack.empty()) {
    record& current = stack.back();

    if (current.entered) {
      leave(current);
      stack.pop_back();
      continue;
    }

    if (is_shareable(current.datum)) {
      if (seen.count(current.datum)) {
        labels.mark_shared(current.datum);

        if (open.count(current.datum))
          cycle = true;

        stack.pop_back();
        continue;
      }

      seen.emplace(current.datum);
    }

    if (auto p = match<pair>(current.datum)) {
      enter(current);

      stack.push_back({car(p)});
      stack.push_back({cdr(p)});
    } else if (auto v = match<vector>(current.datum)) {
      enter(current);

      for (std::size_t i = 0; i < v->size(); ++i)
        stack.push_back({v->ref(i)});
    } else
      stack.pop_back();
  }

  return {std::move(labels), cycle};
}

template <auto OutputAtomic>
static void
output(context& ctx, ptr<> datum, ptr<textual_output_port> out,
       output_datum_labels labels) {
  struct record {
    ptr<>       datum;
    std::size_t written = 0;
    bool        omit_parens = false;
  };
  std::vector<record> stack{{datum}};

  while (!stack.empty()) {
    record& top = stack.back();

    if (top.written == 0 && labels.is_shared(top.datum)) {
      if (labels.has_label(top.datum)) {
        out->write(fmt::format("#{}#", labels.get_or_assign_label(top.datum)));
        stack.pop_back();
        continue;
      } else
        out->write(fmt::format("#{}=", labels.get_or_assign_label(top.datum)));
    }

    if (auto pair = match<insider::pair>(top.datum)) {
      switch (top.written) {
      case 0:
        if (!top.omit_parens)
          out->write('(');
        ++top.written;
        stack.push_back({car(pair)});
        break;

      case 1:
        ++top.written;

        if (is<insider::pair>(cdr(pair)) && !labels.is_shared(cdr(pair))) {
          out->write(' ');
          stack.push_back({cdr(pair), 0, true});
        } else if (cdr(pair) == ctx.constants->null) {
          if (!top.omit_parens)
            out->write(')');
          stack.pop_back();
        } else {
          out->write(" . ");
          stack.push_back({cdr(pair)});
        }
        break;

      case 2:
        if (!top.omit_parens)
          out->write(')');
        stack.pop_back();
      }
    }
    else if (auto vec = match<vector>(top.datum)) {
      if (top.written == 0) {
        out->write("#(");
      }

      if (top.written == vec->size()) {
        out->write(')');
        stack.pop_back();
        continue;
      }

      if (top.written != 0 && top.written != vec->size())
        out->write(' ');

      std::size_t index = top.written++;
      stack.push_back({vec->ref(index)});
    }
    else {
      OutputAtomic(ctx, top.datum, out);
      stack.pop_back();
    }
  }
}

void
write(context& ctx, ptr<> datum, ptr<textual_output_port> out) {
  find_shared_result fsr = find_shared(datum);
  if (fsr.has_cycle)
    output<write_atomic>(ctx, datum, out, std::move(fsr.labels));
  else
    output<write_atomic>(ctx, datum, out, {});
}

void
write_simple(context& ctx, ptr<> datum, ptr<textual_output_port> out) {
  output<write_atomic>(ctx, datum, out, {});
}

void
write_shared(context& ctx, ptr<> datum, ptr<textual_output_port> out) {
  find_shared_result fsr = find_shared(datum);
  output<write_atomic>(ctx, datum, out, std::move(fsr.labels));
}

void
display(context& ctx, ptr<> datum, ptr<textual_output_port> out) {
  output<display_atomic>(ctx, datum, out, {});
}

std::string
datum_to_string(context& ctx, ptr<> datum) {
  auto p = make<textual_output_port>(ctx, std::make_unique<string_port_sink>());
  write(ctx, datum, p);
  return p->get_string();
}

std::string
datum_to_display_string(context& ctx, ptr<> datum) {
  auto p = make<textual_output_port>(ctx, std::make_unique<string_port_sink>());
  display(ctx, datum, p);
  return p->get_string();
}

static void
throw_if_base_not_allowed(unsigned base) {
  if (base != 2 && base != 8 && base != 10 && base != 16)
    throw std::runtime_error{"Invalid base"};
}

static void
inexact_number_to_nondecimal_string(context& ctx, ptr<> z, unsigned base,
                                    ptr<textual_output_port> out) {
  if (is_finite(z)) {
    out->write("#i");
    write_number(ctx, exact(ctx, z), out, base);
  } else
    write_number(ctx, z, out); // Write +nan.0 or +inf.0, base doesn't matter
}

std::string
number_to_string(context& ctx, ptr<> z, unsigned base) {
  throw_if_base_not_allowed(base);
  if (!is_number(z))
    throw std::runtime_error{"Expected a number"};

  auto p = make<textual_output_port>(ctx, std::make_unique<string_port_sink>());

  if (is_exact(z) || base == 10)
    write_number(ctx, z, p, base);
  else
    inexact_number_to_nondecimal_string(ctx, z, base, p);

  return p->get_string();
}

static void
format_floating_point(ptr<floating_point> f,
                      char sign,
                      bool alternative,
                      std::optional<unsigned> precision,
                      char type,
                      ptr<textual_output_port> out) {
  if (sign != '+' && sign != '-' && sign != ' ')
    throw std::runtime_error{fmt::format("Invalid sign specifier: {}", sign)};

  if (type != 'f' && type != 'e' && type != 'g')
    throw std::runtime_error{fmt::format("Invalid type specifier: {}", type)};

  bool written = maybe_write_infinite_float(f, out);
  if (written)
    return;

  using namespace std::literals;
  std::string fmt = "{:";

  fmt += sign;

  if (alternative)
    fmt += '#';

  if (precision)
    fmt += "."s + std::to_string(*precision);

  fmt += type;
  fmt += '}';
  out->write(fmt::format(fmt::runtime(fmt), f->value));
}

static void
newline(ptr<textual_output_port> out) {
  out->write(U'\n');
}

void
export_write(context& ctx, ptr<module_> result) {
  auto default_output_port = make<textual_output_port>(
    ctx, std::make_unique<file_port_sink>(stdout, false)
  );
  ctx.constants->current_output_port_tag
    = create_parameter_tag(ctx, default_output_port);

  auto default_error_port = make<textual_output_port>(
    ctx, std::make_unique<file_port_sink>(stderr, false)
  );
  ctx.constants->current_error_port_tag
    = create_parameter_tag(ctx, default_error_port);

  define_top_level(ctx, "current-output-port-tag", result, true,
                   ctx.constants->current_output_port_tag);
  define_top_level(ctx, "current-error-port-tag", result, true,
                   ctx.constants->current_error_port_tag);
  define_procedure<write>(ctx,
                          "write",
                          result,
                          get_current_textual_output_port);
  define_procedure<write_simple>(ctx, "write-simple", result,
                                 get_current_textual_output_port);
  define_procedure<write_shared>(ctx, "write-shared", result,
                                 get_current_textual_output_port);
  define_procedure<display>(ctx,
                            "display",
                            result,
                            get_current_textual_output_port);
  define_procedure<newline>(ctx,
                            "newline",
                            result,
                            get_current_textual_output_port);
  define_procedure<number_to_string>(ctx, "number->string", result,
                                     [] (vm&) { return 10u; });
  define_procedure<datum_to_string>(ctx, "datum->string", result);
  define_procedure<format_floating_point>(ctx, "format-floating-point", result);
}

} // namespace insider
