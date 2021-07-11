#include "write.hpp"

#include "basic_types.hpp"
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
output_primitive(context& ctx, ptr<> datum, ptr<port> out, bool display) {
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
output_simple(context& ctx, ptr<> datum, ptr<port> out, bool display) {
  struct record {
    ptr<>       datum;
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
write_simple(context& ctx, ptr<> datum, ptr<port> out) {
  output_simple(ctx, datum, out, false);
}

void
display(context& ctx, ptr<> datum, ptr<port> out) {
  output_simple(ctx, datum, out, true);
}

std::string
datum_to_string(context& ctx, ptr<> datum) {
  auto p = make<port>(ctx, "", false, true);
  write_simple(ctx, datum, p);
  return p->get_string();
}

} // namespace insider
