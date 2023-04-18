#include "read.hpp"

#include "compiler/source_location.hpp"
#include "io/port.hpp"
#include "io/reader_stream.hpp"
#include "io/tokenizer.hpp"
#include "runtime/basic_types.hpp"
#include "runtime/character.hpp"
#include "runtime/numeric.hpp"
#include "runtime/syntax.hpp"
#include "util/define_procedure.hpp"
#include "vm/vm.hpp"

#include <fmt/format.h>

#include <variant>

namespace insider {

read_error::read_error(std::string const& message, source_location const& loc)
  : translatable_runtime_error{fmt::format("{}: Read error: {}",
                                           format_location(loc), message)}
{ }

ptr<>
read_error::translate(context& ctx) const {
  return make<scheme_error>(ctx, what());
}

using datum_labels = std::unordered_map<std::string, ptr<>>;

static ptr<>
read(context& ctx, tokenizer& tokenizer, bool read_syntax, datum_labels& labels,
     std::optional<std::string> const& defining_label = {});

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
unwrap(ptr<> value) {
  if (auto stx = match<syntax>(value))
    return stx->get_expression_without_update();
  else
    return value;
}

static ptr<>
find_datum_label_reference(datum_labels const& labels, std::string const& label,
                           source_location const& ref_location) {
  if (auto it = labels.find(label); it != labels.end()) {
    assert(it->second);
    return it->second;
  } else
    throw read_error{fmt::format("Unknown datum label: {}", label),
                     ref_location};
}

static ptr<>
read_and_wrap(context& ctx, tokenizer& tokenizer, bool read_syntax,
              datum_labels& labels) {
  auto loc = tokenizer.get().location;
  return wrap(ctx,
              read(ctx, tokenizer, read_syntax, labels),
              loc, read_syntax);
}

static void
define_label(datum_labels& labels, std::string const& label, ptr<> value) {
  if (auto it = labels.find(label); it != labels.end())
    it->second = value;
  else
    labels.emplace(label, value);
}

static void
define_label(datum_labels& labels,
             std::optional<std::string> const& defining_label,
             ptr<> value) {
  if (defining_label)
    define_label(labels, *defining_label, value);
}

static ptr<>
read_list(context& ctx, tokenizer& tokenizer, bool read_syntax,
          datum_labels& labels,
          std::optional<std::string> const& defining_label) {
  token t = next_token(tokenizer);
  if (std::holds_alternative<token::end>(t.value))
    throw read_error{"Unterminated list", t.location};
  else if (std::holds_alternative<token::dot>(t.value))
    throw read_error{"Unexpected . token", t.location};
  else if (std::holds_alternative<token::right_paren>(t.value))
    return ctx.constants->null;

  ptr<pair> result = make<pair>(ctx, ctx.constants->null, ctx.constants->null);
  define_label(labels, defining_label, result);

  result->set_car(ctx.store,
                  read_and_wrap(ctx, tokenizer, read_syntax, labels));
  ptr<pair> tail = result;

  t = next_token(tokenizer);
  while (!std::holds_alternative<token::end>(t.value)
         && !std::holds_alternative<token::right_paren>(t.value)
         && !std::holds_alternative<token::dot>(t.value)) {
    ptr<pair> new_tail = make<pair>(ctx,
                                    read_and_wrap(ctx, tokenizer, read_syntax,
                                                  labels),
                                    ctx.constants->null);
    tail->set_cdr(ctx.store, new_tail);
    tail = new_tail;

    t = next_token(tokenizer);
  }

  if (std::holds_alternative<token::end>(t.value))
    throw read_error{"Unterminated list", t.location};
  else if (std::holds_alternative<token::dot>(t.value)) {
    t = next_token(tokenizer);
    ptr<> cdr = read_and_wrap(ctx, tokenizer, read_syntax, labels);
    tail->set_cdr(ctx.store, cdr);

    t = next_token(tokenizer);
    if (!std::holds_alternative<token::right_paren>(t.value))
      throw read_error{"Too many elements after .", t.location};
  }

  assert(std::holds_alternative<token::right_paren>(t.value));
  return result;
}

static bool
valid_bytevector_element(ptr<> e) {
  if (auto i = match<integer>(e))
    return i->value() >= std::numeric_limits<bytevector::element_type>::min()
           && i->value() <= std::numeric_limits<bytevector::element_type>::max();
  else
    return false;
}

static std::vector<ptr<>>
read_vector_elements(context& ctx, tokenizer& tokenizer, bool read_syntax,
                     datum_labels& labels, bool bytevector = false) {
  std::vector<ptr<>> elements;

  token t = next_token(tokenizer);
  while (!std::holds_alternative<token::end>(t.value)
         && !std::holds_alternative<token::right_paren>(t.value)) {
    source_location loc = tokenizer.location();
    elements.push_back(read_and_wrap(ctx, tokenizer, read_syntax, labels));

    if (bytevector) {
      elements.back() = unwrap(elements.back());
      if (!valid_bytevector_element(elements.back()))
        throw read_error{"Invalid bytevector element", loc};
    }

    t = next_token(tokenizer);
  }

  if (std::holds_alternative<token::end>(t.value))
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
read_vector(context& ctx, tokenizer& tokenizer, bool read_syntax,
            datum_labels& labels,
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

    dummy_vector = make<vector>(ctx, 0, ctx.constants->void_);
    define_label(labels, *defining_label, dummy_vector);
  }

  std::vector<ptr<>> elements = read_vector_elements(ctx, tokenizer, read_syntax,
                                                     labels);
  ptr<vector> result = make<vector>(ctx, elements.size(), ctx.constants->void_);
  for (std::size_t i = 0; i < elements.size(); ++i)
    result->set(ctx.store, i, elements[i]);

  if (defining_label) {
    replace_value(ctx, result, dummy_vector, result);
    labels[*defining_label] = result;
  }

  return result;
}

static ptr<>
read_bytevector(context& ctx, tokenizer& tokenizer, bool read_syntax,
                datum_labels& labels,
                std::optional<std::string> const& defining_label) {
  source_location loc = tokenizer.location();
  std::vector<ptr<>> elements = read_vector_elements(ctx, tokenizer, read_syntax,
                                                     labels, true);

  auto bv = make<bytevector>(ctx, elements.size());
  for (std::size_t i = 0; i < elements.size(); ++i)
    bv->set(i,
            static_cast<bytevector::element_type>(
              assume<integer>(elements[i]).value()
            ));

  if (defining_label)
    labels.emplace(*defining_label, bv);

  return bv;
}

static ptr<>
read_shortcut(context& ctx, tokenizer& tokenizer, 
              std::string const& shortcut, std::string const& expansion,
              bool read_syntax, datum_labels& labels,
              std::optional<std::string> const& defining_label) {
  token shortcut_token = tokenizer.get();
  token t = next_token(tokenizer);
  if (std::holds_alternative<token::end>(t.value))
    throw read_error{fmt::format("Expected token after {}", shortcut),
                     t.location};

  ptr<pair> result = cons(ctx,
                          wrap(ctx, ctx.intern(expansion),
                               shortcut_token.location, read_syntax),
                          ctx.constants->null);
  define_label(labels, defining_label, result);

  ptr<> body = read_and_wrap(ctx, tokenizer, read_syntax, labels);
  result->set_cdr(ctx.store, cons(ctx, body, ctx.constants->null));

  return result;
}

static ptr<>
define_label_for_atomic_value(ptr<> value, datum_labels& labels,
                              std::optional<std::string> const& defining_label) {
  if (defining_label)
    define_label(labels, *defining_label, value);

  return value;
}

static ptr<>
read(context& ctx, tokenizer& tokenizer, bool read_syntax, datum_labels& labels,
     std::optional<std::string> const& defining_label) {
  token const& first_token = tokenizer.get();

  if (std::holds_alternative<token::end>(first_token.value))
    return {};
  else if (std::holds_alternative<token::left_paren>(first_token.value))
    return read_list(ctx, tokenizer, read_syntax, labels, defining_label);
  else if (std::holds_alternative<token::begin_vector>(first_token.value))
    return read_vector(ctx, tokenizer, read_syntax, labels, defining_label);
  else if (std::holds_alternative<token::begin_bytevector>(first_token.value))
    return read_bytevector(ctx, tokenizer, read_syntax, labels, defining_label);
  else if (std::holds_alternative<token::quote>(first_token.value))
    return read_shortcut(ctx, tokenizer, "'", "quote", read_syntax,
                         labels, defining_label);
  else if (std::holds_alternative<token::syntax>(first_token.value))
    return read_shortcut(ctx, tokenizer, "#'", "syntax", read_syntax, labels,
                         defining_label);
  else if (std::holds_alternative<token::quasiquote>(first_token.value))
    return read_shortcut(ctx, tokenizer, "`", "quasiquote", read_syntax, labels,
                         defining_label);
  else if (std::holds_alternative<token::quasisyntax>(first_token.value))
    return read_shortcut(ctx, tokenizer, "#`", "quasisyntax", read_syntax,
                         labels, defining_label);
  else if (std::holds_alternative<token::unquote>(first_token.value))
    return read_shortcut(ctx, tokenizer, ",", "unquote", read_syntax, labels,
                         defining_label);
  else if (std::holds_alternative<token::unsyntax>(first_token.value))
    return read_shortcut(ctx, tokenizer, "#,", "unsyntax", read_syntax, labels,
                         defining_label);
  else if (std::holds_alternative<token::unquote_splicing>(first_token.value))
    return read_shortcut(ctx, tokenizer, ",@", "unquote-splicing", read_syntax,
                         labels, defining_label);
  else if (std::holds_alternative<token::unsyntax_splicing>(first_token.value))
    return read_shortcut(ctx, tokenizer, "#,@", "unsyntax-splicing", read_syntax,
                         labels, defining_label);
  else if (auto const* lit
             = std::get_if<token::generic_literal>(&first_token.value))
    return define_label_for_atomic_value(lit->value, labels, defining_label);
  else if (auto const* i = std::get_if<token::identifier>(&first_token.value))
    return define_label_for_atomic_value(ctx.intern(i->value), labels,
                                         defining_label);
  else if (auto const* k = std::get_if<token::keyword_literal>(&first_token.value))
    return define_label_for_atomic_value(ctx.intern_keyword(k->value), labels,
                                         defining_label);
  else if (auto const* b = std::get_if<token::boolean_literal>(&first_token.value))
    return define_label_for_atomic_value(
      b->value ? ctx.constants->t : ctx.constants->f,
      labels,
      defining_label
    );
  else if (std::holds_alternative<token::void_literal>(first_token.value))
    return define_label_for_atomic_value(ctx.constants->void_, labels,
                                         defining_label);
  else if (std::holds_alternative<token::default_value_literal>(first_token.value))
    return define_label_for_atomic_value(ctx.constants->default_value, labels,
                                         defining_label);
  else if (std::holds_alternative<token::dot>(first_token.value))
    throw read_error{"Unexpected . token", first_token.location};
  else if (std::holds_alternative<token::right_paren>(first_token.value))
    throw read_error{"Unexpected ) token", first_token.location};
  else if (auto const* dldef
             = std::get_if<token::datum_label_definition>(&first_token.value)) {
    std::string label = dldef->label;
    tokenizer.advance();
    return read(ctx, tokenizer, read_syntax, labels, label);
  } else if (auto const* dlref
             = std::get_if<token::datum_label_reference>(&first_token.value))
    return find_datum_label_reference(labels, dlref->label, first_token.location);
  else if (std::holds_alternative<token::datum_comment>(first_token.value)) {
    datum_labels dummy_labels;
    tokenizer.advance();
    read(ctx, tokenizer, false, dummy_labels); // Discard

    tokenizer.advance();
    return read(ctx, tokenizer, read_syntax, labels, defining_label);
  }

  assert(false); // Unimplemented token
  throw read_error{"Unimplemented token", first_token.location};
}

static ptr<>
read_optional(context& ctx, ptr<textual_input_port> stream) {
  reader_stream s{register_root(ctx, stream)};
  datum_labels labels;
  tokenizer t{ctx, s};
  return read(ctx, t, false, labels);
}

ptr<>
read(context& ctx, ptr<textual_input_port> stream) {
  if (ptr<> result = read_optional(ctx, stream))
    return result;
  else
    return ctx.constants->eof;
}

ptr<>
read(context& ctx, std::string s) {
  unique_port_handle<textual_input_port> h{open_input_string(ctx, std::move(s))};
  return read(ctx, h.get());
}

static ptr<syntax>
read_syntax_optional(context& ctx, reader_stream& s) {
  datum_labels labels;
  tokenizer t{ctx, s};
  return assume<syntax>(read_and_wrap(ctx, t, true, labels));
}

static ptr<>
read_syntax(context& ctx, reader_stream& s) {
  if (auto stx = read_syntax_optional(ctx, s))
    return stx;
  else
    return ctx.constants->eof;
}

ptr<>
read_syntax(context& ctx, ptr<textual_input_port> stream) {
  reader_stream s{register_root(ctx, stream)};
  return read_syntax(ctx, s);
}

ptr<>
read_syntax(context& ctx, std::string s) {
  unique_port_handle<textual_input_port> h{
    open_input_string(ctx, std::move(s))
  };
  return read_syntax(ctx, h.get());
}

std::vector<ptr<>>
read_multiple(context& ctx, ptr<textual_input_port> in) {
  std::vector<ptr<>> result;
  while (ptr<> elem = read_optional(ctx, in))
    result.push_back(elem);

  return result;
}

std::vector<ptr<>>
read_multiple(context& ctx, std::string s) {
  unique_port_handle<textual_input_port> h{
    open_input_string(ctx, std::move(s))
  };
  return read_multiple(ctx, h.get());
}

static std::vector<ptr<syntax>>
read_syntax_multiple(context& ctx, reader_stream& stream) {
  std::vector<ptr<syntax>> result;
  while (ptr<syntax> elem = read_syntax_optional(ctx, stream))
    result.push_back(elem);

  return result;
}

std::vector<ptr<syntax>>
read_syntax_multiple(context& ctx, ptr<textual_input_port> p) {
  reader_stream in{register_root(ctx, p)};
  return read_syntax_multiple(ctx, in);
}

std::vector<ptr<syntax>>
read_syntax_multiple_ci(context& ctx, ptr<textual_input_port> p) {
  reader_stream in{register_root(ctx, p)};
  in.fold_case = true;
  return read_syntax_multiple(ctx, in);
}

std::vector<ptr<syntax>>
read_syntax_multiple(context& ctx, std::string s) {
  unique_port_handle<textual_input_port> h{
    open_input_string(ctx, std::move(s))
  };
  return read_syntax_multiple(ctx, h.get());
}

ptr<>
string_to_number(context& ctx, std::string const& s, unsigned base) {
  if (base != 2 && base != 8 && base != 10 && base != 16)
    throw std::runtime_error{"Invalid base"};

  try {
    auto port = open_input_string(ctx, s);
    reader_stream stream{register_root(ctx, port)};

    if (ptr<> result = read_number(ctx, stream, stream.location(), base)) {
      if (!stream.read())
        return result;
      else
        return ctx.constants->f;
    } else
      return ctx.constants->f;
  } catch (read_error const&) {
    return ctx.constants->f;
  }
}

static ptr<>
read_syntax_multiple_proc(context& ctx, ptr<textual_input_port> p) {
  return make_list_from_range(ctx, read_syntax_multiple(ctx, p));
}

static ptr<>
read_syntax_multiple_ci_proc(context& ctx, ptr<textual_input_port> p) {
  return make_list_from_range(ctx, read_syntax_multiple_ci(ctx, p));
}

void
export_read(context& ctx, ptr<module_> result) {
  auto default_input_port = make<textual_input_port>(
    ctx, std::make_unique<stdin_source>(), "<stdin>"
  );
  ctx.constants->current_input_port_tag
    = create_parameter_tag(ctx, default_input_port);

  define_top_level(ctx, "current-input-port-tag", result, true,
                   ctx.constants->current_input_port_tag);
  define_procedure<
    static_cast<ptr<> (*)(context&, ptr<textual_input_port>)>(read)
  >(
    ctx, "read", result, get_current_textual_input_port
  );
  define_procedure<
    static_cast<ptr<> (*)(context&, ptr<textual_input_port>)>(read_syntax)
  >(
    ctx, "read-syntax", result, get_current_textual_input_port
  );
  define_procedure<read_syntax_multiple_proc>(
    ctx, "read-syntax-multiple", result, get_current_textual_input_port
  );
  define_procedure<read_syntax_multiple_ci_proc>(
    ctx,
    "read-syntax-multiple-ci",
    result,
    get_current_textual_input_port
  );
  define_procedure<string_to_number>(ctx, "string->number", result,
                                     [] (vm&) { return 10; });
  define_procedure<&read_error::scheme_error::message>(
    ctx, "read-error-message", result
  );
}

} // namespace insider
