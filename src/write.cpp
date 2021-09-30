#include "write.hpp"

#include "basic_types.hpp"
#include "compare.hpp"
#include "context.hpp"
#include "converters.hpp"
#include "numeric.hpp"
#include "port.hpp"
#include "source_location.hpp"
#include "string.hpp"

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
    else
      out->write(c);
  out->write('"');
}

static void
write_char(char32_t c, ptr<textual_output_port> out) {
  out->write(R"(#\)");
  out->write(c);
}

static void
write_primitive(context& ctx, ptr<> datum, ptr<textual_output_port> out) {
  if (datum == ctx.constants->null.get())
    out->write("()");
  else if (datum == ctx.constants->void_.get())
    out->write("#void");
  else if (datum == ctx.constants->t.get())
    out->write("#t");
  else if (datum == ctx.constants->f.get())
    out->write("#f");
  else if (is_number(datum))
    write_number(ctx, datum, out);
  else if (auto sym = match<symbol>(datum))
    out->write(sym->value());
  else if (auto stx = match<syntax>(datum)) {
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
  } else if (auto core = match<core_form_type>(datum)) {
    out->write(fmt::format("<core form {}>", core->name));
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
        } else if (cdr(pair) == ctx.constants->null.get()) {
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
  write_simple(ctx, datum, p);
  return p->get_string();
}

void
export_write(context& ctx, module& result) {
  define_procedure(
    ctx, "write", result, true,
    [] (context& ctx, ptr<> datum) {
      write(ctx, datum, ctx.output_port.get());
    }
  );

  define_procedure(
    ctx, "write-simple", result, true,
    [] (context& ctx, ptr<> datum) {
      write_simple(ctx, datum, ctx.output_port.get());
    }
  );

  define_procedure(
    ctx, "write-shared", result, true,
    [] (context& ctx, ptr<> datum) {
      write_shared(ctx, datum, ctx.output_port.get());
    }
  );

  define_procedure(
    ctx, "display", result, true,
    [] (context& ctx, ptr<> datum) {
      display(ctx, datum, ctx.output_port.get());
    }
  );

  define_procedure(
    ctx, "newline", result, true,
    [] (context& ctx) { ctx.output_port->write('\n'); }
  );
}

} // namespace insider
