#include "write.hpp"

#include "basic_types.hpp"
#include "compare.hpp"
#include "context.hpp"
#include "numeric.hpp"
#include "port.hpp"
#include "source_location.hpp"

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
write_string(ptr<string> s, ptr<port> out) {
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
write_char(ptr<character> c, ptr<port> out) {
  out->write_string(R"(#\)");
  out->write_char(c->value());
}

static void
write_primitive(context& ctx, ptr<> datum, ptr<port> out) {
  if (datum == ctx.constants->null.get())
    out->write_string("()");
  else if (datum == ctx.constants->void_.get())
    out->write_string("#void");
  else if (datum == ctx.constants->t.get())
    out->write_string("#t");
  else if (datum == ctx.constants->f.get())
    out->write_string("#f");
  else if (is_number(datum))
    write_number(ctx, datum, out);
  else if (auto sym = match<symbol>(datum))
    out->write_string(sym->value());
  else if (auto stx = match<syntax>(datum)) {
    out->write_string("#<syntax ");
    out->write_string(format_location(stx->location()));
    out->write_char(' ');
    write_simple(ctx, syntax_to_datum(ctx, stx), out);
    out->write_string(">");
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
write_atomic(context& ctx, ptr<> datum, ptr<port> out) {
  if (auto str = match<string>(datum))
    write_string(str, out);
  else if (auto c = match<character>(datum))
    write_char(c, out);
  else
    write_primitive(ctx, datum, out);
}

static void
display_atomic(context& ctx, ptr<> datum, ptr<port> out) {
  if (auto str = match<string>(datum))
    out->write_string(str->value());
  else if (auto c = match<character>(datum))
    out->write_char(c->value());
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
  std::vector<ptr<>> stack{datum};

  while (!stack.empty()) {
    ptr<> current = stack.back();
    stack.pop_back();

    if (is_shareable(current)) {
      if (seen.count(current)) {
        labels.mark_shared(current);
        continue;
      }

      seen.emplace(current);
    }

    if (auto p = match<pair>(current)) {
      stack.push_back(car(p));
      stack.push_back(cdr(p));
    } else if (auto v = match<vector>(current)) {
      for (std::size_t i = 0; i < v->size(); ++i)
        stack.push_back(v->ref(i));
    }
  }

  return {std::move(labels)};
}

template <auto OutputAtomic>
static void
output(context& ctx, ptr<> datum, ptr<port> out, output_datum_labels labels) {
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
        out->write_string(fmt::format("#{}#", labels.get_or_assign_label(top.datum)));
        stack.pop_back();
        continue;
      } else
        out->write_string(fmt::format("#{}=", labels.get_or_assign_label(top.datum)));
    }

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

        if (is<insider::pair>(cdr(pair)) && !labels.is_shared(cdr(pair))) {
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
      OutputAtomic(ctx, top.datum, out);
      stack.pop_back();
    }
  }
}

void
write_simple(context& ctx, ptr<> datum, ptr<port> out) {
  output<write_atomic>(ctx, datum, out, {});
}

void
write_shared(context& ctx, ptr<> datum, ptr<port> out) {
  find_shared_result fsr = find_shared(datum);
  output<write_atomic>(ctx, datum, out, std::move(fsr.labels));
}

void
display(context& ctx, ptr<> datum, ptr<port> out) {
  output<display_atomic>(ctx, datum, out, {});
}

std::string
datum_to_string(context& ctx, ptr<> datum) {
  auto p = make<port>(ctx, "", false, true);
  write_simple(ctx, datum, p);
  return p->get_string();
}

} // namespace insider
