#include "context.hpp"

#include "compiler/analyser.hpp"
#include "compiler/source_code_provider.hpp"
#include "compiler/variable.hpp"
#include "io/read.hpp"
#include "runtime/action.hpp"
#include "runtime/basic_types.hpp"
#include "runtime/internal_module.hpp"
#include "runtime/parameter_map.hpp"
#include "runtime/syntax.hpp"
#include "util/integer_cast.hpp"
#include "vm/execution_state.hpp"

#include <memory>

namespace insider {

context::context() {
  constants = std::make_unique<struct constants>();
  constants->null = make<null_type>(*this);
  constants->void_ = make<void_type>(*this);
  constants->t = make<boolean>(*this, true);
  constants->f = make<boolean>(*this, false);
  constants->eof = make<eof_type>(*this);
  constants->tail_call_tag = make<tail_call_tag_type>(*this);
  constants->integer_type_symbol = intern(integer_type_name);
  constants->character_type_symbol = intern(character_type_name);

  parameters = make<parameter_map>(*this);

  module_resolver().set_internal_module(make_internal_module(*this));

  struct {
    ptr<core_form_type>& object;
    std::string          name;
  } core_forms[]{
    {constants->let,               "let"},
    {constants->set,               "set!"},
    {constants->lambda,            "lambda"},
    {constants->if_,               "if"},
    {constants->define,            "define"},
    {constants->define_syntax,     "define-syntax"},
    {constants->begin,             "begin"},
    {constants->quote,             "quote"},
    {constants->quasiquote,        "quasiquote"},
    {constants->unquote,           "unquote"},
    {constants->unquote_splicing,  "unquote-splicing"},
    {constants->syntax,            "syntax"},
    {constants->quasisyntax,       "quasisyntax"},
    {constants->unsyntax,          "unsyntax"},
    {constants->unsyntax_splicing, "unsyntax-splicing"},
    {constants->syntax_trap,       "syntax-trap"},
    {constants->syntax_error,      "syntax-error"},
    {constants->let_syntax,        "let-syntax"},
    {constants->letrec_syntax,     "letrec-syntax"},
    {constants->meta,              "meta"}
  };
  for (auto const& form : core_forms) {
    form.object = make<core_form_type>(*this, form.name);
    auto index = add_top_level(form.object, form.name);
    auto name = intern(form.name);
    auto id = make<syntax>(*this, name, scope_set{internal_module()->scope()});
    auto var = make<top_level_variable>(*this, form.name, index);
    internal_module()->scope()->add(store, id, var);
    internal_module()->export_(name);
  }

  constants->init = make<core_form_type>(*this, "init");
  features_ = make_list(*this, intern("r7rs"), intern("full-unicode"));
}

context::~context() {
  constants.reset();
}

ptr<symbol>
context::intern(std::string const& s) {
  auto interned = interned_symbols_.find(s);
  if (interned != interned_symbols_.end()) {
    if (ptr<symbol> sym = interned->second)
      return sym;
    else {
      ptr<symbol> result = make<symbol>(*this, s);
      interned->second = result;
      return result;
    }
  }

  ptr<symbol> result = make<symbol>(*this, s);
  interned_symbols_.emplace(s, result);

  return result;
}

ptr<>
context::get_top_level_checked(operand i) const {
  if (static_cast<std::size_t>(i) >= top_level_objects_.size())
    throw std::runtime_error{fmt::format("Nonexistent top-level object {}", i)};

  return top_level_objects_[i];
}

void
context::set_top_level(operand i, ptr<> value) {
  assert(i >= 0);
  assert(static_cast<std::size_t>(i) < top_level_objects_.size());
  top_level_objects_[i] = value;
}

operand
context::add_top_level(ptr<> x, std::string name) {
  top_level_objects_.push_back(x);
  top_level_binding_names_.emplace_back(std::move(name));
  return static_cast<operand>(top_level_objects_.size() - 1);
}

std::string
context::get_top_level_name(operand i) const {
  if (static_cast<std::size_t>(i) < top_level_binding_names_.size())
    return top_level_binding_names_[i];
  else
    throw make_error("Invalid global operand {}", i);
}

void
context::add_feature(std::string const& f) {
  auto f_sym = intern(f);
  if (!memq(f_sym, features_))
    features_ = cons(*this, f_sym, features_);
}

tracked_ptr<module_>
context::internal_module_tracked() {
  return track(*this, internal_module());
}

scope::id_type
context::generate_scope_id() {
  return next_scope_id_++;
}

void
context::root_provider::visit_roots(member_visitor const& f) {
  f(ctx_.constants->null);
  f(ctx_.constants->void_);
  f(ctx_.constants->t);
  f(ctx_.constants->f);
  f(ctx_.constants->eof);
  f(ctx_.constants->tail_call_tag);
  f(ctx_.constants->integer_type_symbol);
  f(ctx_.constants->character_type_symbol);
  f(ctx_.constants->let);
  f(ctx_.constants->set);
  f(ctx_.constants->init);
  f(ctx_.constants->lambda);
  f(ctx_.constants->if_);
  f(ctx_.constants->define);
  f(ctx_.constants->define_syntax);
  f(ctx_.constants->begin);
  f(ctx_.constants->quote);
  f(ctx_.constants->quasiquote);
  f(ctx_.constants->unquote);
  f(ctx_.constants->unquote_splicing);
  f(ctx_.constants->syntax);
  f(ctx_.constants->quasisyntax);
  f(ctx_.constants->unsyntax);
  f(ctx_.constants->unsyntax_splicing);
  f(ctx_.constants->syntax_trap);
  f(ctx_.constants->syntax_error);
  f(ctx_.constants->let_syntax);
  f(ctx_.constants->letrec_syntax);
  f(ctx_.constants->meta);
  f(ctx_.constants->current_input_port_tag);
  f(ctx_.constants->current_output_port_tag);
  f(ctx_.constants->current_error_port_tag);
  f(ctx_.constants->current_source_file_origin_tag);
  f(ctx_.constants->is_main_module_tag);
  f(ctx_.constants->current_expand_module_tag);
  f(ctx_.constants->interaction_environment_specifier_tag);

  f(ctx_.parameters);

  for (auto& [name, symbol] : ctx_.interned_symbols_)
    f(weak(symbol));

  for (ptr<symbol>& name : ctx_.type_name_symbols_)
    f(name);

  for (ptr<>& x : ctx_.top_level_objects_)
    f(x);

  f(ctx_.features_);
}

} // namespace insider
