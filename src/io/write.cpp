#include "write.hpp"

#include "compiler/source_location.hpp"
#include "context.hpp"
#include "io/port.hpp"
#include "runtime/basic_types.hpp"
#include "runtime/compare.hpp"
#include "runtime/numeric.hpp"
#include "runtime/string.hpp"
#include "util/define_procedure.hpp"

#include <fmt/format.h>

#include <algorithm>
#include <limits>
#include <locale>
#include <optional>
#include <sstream>
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
write_char(char32_t c, ptr<textual_output_port> out) {
  out->write(R"(#\)");
  out->write(c);
}

static void
write_number(context& ctx, ptr<> value, ptr<textual_output_port> out, unsigned base = 10);

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
write_big(context& ctx, ptr<big_integer> value, ptr<textual_output_port> out, unsigned base) {
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
    buffer.push_back(digit_to_letter(static_cast<unsigned>(assume<integer>(rem).value())));
    v = quot;
  }

  write_small_magnitude(buffer, assume<integer>(v).value(), base);

  for (auto c = buffer.rbegin(), e = buffer.rend(); c != e; ++c)
    out->write(static_cast<char32_t>(*c));
}

static void
write_fraction(context& ctx, ptr<fraction> value, ptr<textual_output_port> out, unsigned base) {
  write_number(ctx, value->numerator(), out, base);
  out->write('/');
  write_number(ctx, value->denominator(), out, base);
}

static void
write_float(ptr<floating_point> value, ptr<textual_output_port> out) {
  // Same as with string_to_double: std::to_chars would be the ideal way to implement this, but we'll go with an
  // std::ostringstream in its absence.

  if (value->value == floating_point::positive_infinity) {
    out->write("+inf.0");
    return;
  } else if (value->value == floating_point::negative_infinity) {
    out->write("-inf.0");
    return;
  } else if (std::isnan(value->value)) {
    if (std::signbit(value->value))
      out->write('-');
    else
      out->write('+');

    out->write("nan.0");
    return;
  }

  std::ostringstream os;
  os.imbue(std::locale("C"));

  os << std::showpoint << std::fixed
     << std::setprecision(std::numeric_limits<floating_point::value_type>::max_digits10 - 1)
     << value->value;

  std::string result = os.str();
  std::string::size_type dot = result.find('.');
  std::string::size_type last_nonzero = result.find_last_not_of('0');
  std::string::size_type end = std::max(dot + 1, last_nonzero);
  out->write(result.substr(0, end + 1));
}

template <int I>
static bool
is_exact_equal(ptr<> x) {
  return is_exact_integer(x) && is<integer>(x) && assume<integer>(x).value() == I;
}

static void
write_complex(context& ctx, ptr<complex> z, ptr<textual_output_port> out, unsigned base) {
  if (!is_exact_equal<0>(z->real()))
    write_number(ctx, z->real(), out, base);

  if (is_exact_equal<1>(z->imaginary()))
    out->write("+i");
  else if (is_exact_equal<-1>(z->imaginary()))
    out->write("-i");
  else {
    if (!is_negative(z->imaginary()) && !is_exact_equal<0>(z->real()) && is_finite(z->imaginary()))
      out->write('+');

    write_number(ctx, z->imaginary(), out, base);
    out->write('i');
  }
}

static void
write_number(context& ctx, ptr<> value, ptr<textual_output_port> out, unsigned base) {
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
  else if (is_number(datum))
    write_number(ctx, datum, out);
  else if (auto sym = match<symbol>(datum))
    out->write(sym->value());
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
  } else if (auto proc = match<procedure>(datum)) {
    if (proc->name)
      out->write(fmt::format("<procedure {}>", *proc->name));
    else
      out->write("<lambda>");
  } else if (auto np = match<native_procedure>(datum)) {
    out->write(fmt::format("<native procedure {}>", np->name));
  } else if (auto cls = match<closure>(datum)) {
    auto proc = cls->procedure();

    std::string name;
    if (proc->name)
      name = *proc->name;
    else
      name = "<lambda>";

    out->write(fmt::format("<closure {}>", name));
  } else if (auto core = match<core_form_type>(datum)) {
    out->write(fmt::format("<core form {}>", core->name));
  } else if (auto e = match<error>(datum)) {
    out->write(fmt::format("<error: {} {}>", e->message(ctx)->value(), datum_to_string(ctx, e->irritants(ctx))));
  } else if (auto fe = match<file_error>(datum)) {
    out->write(fmt::format("<file error: {}>", fe->message()));
  } else if (auto v = match<values_tuple>(datum)) {
    out->write("<values");
    for (std::size_t i = 0; i < v->size(); ++i) {
      out->write(' ');
      write_simple(ctx, v->ref(i), out);
    }
    out->write(">");
  } else
    out->write(fmt::format("<{}>", object_type_name(datum)));
}

static void
write_atomic(context& ctx, ptr<> datum, ptr<textual_output_port> out) {
  if (auto str = match<string>(datum))
    write_string(str, out);
  else if (auto c = match<char32_t>(datum))
    write_char(*c, out);
  else
    write_primitive(ctx, datum, out);
}

static void
display_atomic(context& ctx, ptr<> datum, ptr<textual_output_port> out) {
  if (auto str = match<string>(datum))
    out->write(str->value());
  else if (auto c = match<char32_t>(datum))
    out->write(*c);
  else
    write_primitive(ctx, datum, out);
}

namespace {
  class output_datum_labels {
  public:
    bool
    is_shared(ptr<> x) const { return labels_.count(x); }

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
    bool                has_cycle;
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
output(context& ctx, ptr<> datum, ptr<textual_output_port> out, output_datum_labels labels) {
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

static void
throw_if_base_not_allowed(unsigned base) {
  if (base != 2 && base != 8 && base != 10 && base != 16)
    throw std::runtime_error{"Invalid base"};
}

static void
inexact_number_to_nondecimal_string(context& ctx, ptr<> z, unsigned base, ptr<textual_output_port> out) {
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
newline(ptr<textual_output_port> out) {
  out->write(U'\n');
}

static ptr<textual_output_port>
get_default_port(context& ctx) {
  return expect<textual_output_port>(find_parameter_value(ctx, ctx.constants->current_output_port_tag));
}

static void
write_char_proc(char32_t c, ptr<textual_output_port> out) {
  out->write(c);
}

void
export_write(context& ctx, module_& result) {
  define_top_level(ctx, "current-output-port-tag", result, true, ctx.constants->current_output_port_tag);
  define_top_level(ctx, "current-error-port-tag", result, true, ctx.constants->current_error_port_tag);
  define_procedure<write>(ctx, "write", result, true, get_default_port);
  define_procedure<write_simple>(ctx, "write-simple", result, true, get_default_port);
  define_procedure<write_shared>(ctx, "write-shared", result, true, get_default_port);
  define_procedure<display>(ctx, "display", result, true, get_default_port);
  define_procedure<newline>(ctx, "newline", result, true, get_default_port);
  define_procedure<write_char_proc>(ctx, "write-char", result, true, get_default_port);
  define_procedure<number_to_string>(ctx, "number->string", result, true, [] (context&) { return 10u; });
  define_procedure<datum_to_string>(ctx, "datum->string", result, true);
}

void
init_write(context& ctx) {
  auto default_output_port = make<textual_output_port>(ctx, std::make_unique<file_port_sink>(stdout, false));
  ctx.constants->current_output_port_tag = create_parameter_tag(ctx, default_output_port);

  auto default_error_port = make<textual_output_port>(ctx, std::make_unique<file_port_sink>(stderr, false));
  ctx.constants->current_error_port_tag = create_parameter_tag(ctx, default_error_port);
}

} // namespace insider
