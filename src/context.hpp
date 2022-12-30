#ifndef INSIDER_CONTEXT_HPP
#define INSIDER_CONTEXT_HPP

#include "compiler/scope.hpp"
#include "memory/free_store.hpp"
#include "module_resolver.hpp"
#include "object.hpp"
#include "ptr.hpp"
#include "vm/bytecode.hpp"
#include "vm/operand.hpp"
#include "vm/stacktrace.hpp"

#include <map>
#include <tuple>
#include <vector>

namespace insider {

class boolean;
class context;
class core_form_type;
class default_value_type;
class eof_type;
class integer;
class keyword;
class module_;
class null_type;
class parameter_map;
class parameter_tag;
class procedure_prototype;
class scope;
class symbol;
class tail_call_tag_type;
class textual_output_port;
class transformer;
class void_type;

class vm;

class source_code_provider;

// Evaluation context.
class context {
public:
  struct constants {
    ptr<insider::null_type> null;
    ptr<insider::void_type> void_;
    ptr<boolean>            t, f;     // #t and #f.
    ptr<eof_type>           eof;
    ptr<default_value_type> default_value;
    ptr<tail_call_tag_type> tail_call_tag;
    ptr<core_form_type>
      let, set, init, lambda, if_, define, define_syntax, begin, quote,
      quasiquote, unquote, unquote_splicing, syntax, quasisyntax, unsyntax,
      unsyntax_splicing, syntax_trap, syntax_error, let_syntax, letrec_syntax,
      meta;
    ptr<parameter_tag> current_input_port_tag;
    ptr<parameter_tag> current_output_port_tag;
    ptr<parameter_tag> current_error_port_tag;
    ptr<parameter_tag> current_source_file_origin_tag;
    ptr<parameter_tag> is_main_module_tag;
    ptr<parameter_tag> current_expand_module_tag;
    ptr<parameter_tag> interaction_environment_specifier_tag;
    ptr<symbol>        integer_type_symbol;
    ptr<symbol>        character_type_symbol;
    ptr<symbol>        string_cursor_type_symbol;
  };

  free_store                       store;
  std::unique_ptr<constants>       constants;
  // Built from actions during stack unwinding.
  std::string                      action_backtrace;
  stacktrace                       last_exception_stacktrace;
  std::unique_ptr<vm> current_execution;
  ptr<parameter_map>               parameters;

  context();
  ~context();
  context(context const&) = delete;
  void
  operator = (context const&) = delete;

  // If the given string has been interned previously in the context, return the
  // pre-existing symbol. Otherwise, create a new symbol object and return
  // that. This means that two interned symbols can be compared for equality
  // using pointer comparison.
  ptr<symbol>
  intern(std::string const&);

  ptr<keyword>
  intern_keyword(std::string const&);

  ptr<symbol>
  intern_type_name(word_type type_index);

  ptr<>
  get_top_level(operand i) const { return top_level_objects_[i].value; }

  ptr<>
  get_top_level_checked(operand) const;

  void
  set_top_level(operand i, ptr<>);

  operand
  add_top_level(ptr<>, std::string name);

  operand
  add_top_level_mutable(ptr<>, std::string name);

  std::string
  get_top_level_name(operand) const;

  void
  add_feature(std::string const&);

  insider::module_resolver&
  module_resolver() { return module_resolver_; }

  ptr<module_>
  internal_module() { return module_resolver_.internal_module(); }

  tracked_ptr<module_>
  internal_module_tracked();

  ptr<>
  features() const { return features_; }

  scope::id_type
  generate_scope_id();

private:
  class root_provider : public insider::root_provider {
  public:
    explicit
    root_provider(context& ctx)
      : insider::root_provider{ctx.store}
      , ctx_{ctx}
    { }

  private:
    context& ctx_;

    void
    visit_roots(member_visitor const&) override;
  };

  struct top_level_binding {
    ptr<>       value;
    std::string name;
    bool        mutable_;
  };

  root_provider                                 root_provider_{*this};
  std::unordered_map<std::string, ptr<symbol>>  interned_symbols_;
  std::unordered_map<std::string, ptr<keyword>> interned_keywords_;
  std::vector<top_level_binding>                top_level_objects_;
  insider::module_resolver                      module_resolver_{*this};
  std::vector<ptr<symbol>>                      type_name_symbols_;

  ptr<> features_;
  scope::id_type next_scope_id_ = 0;
};

// Create an instance of an object using the context's free store.
template <typename T, typename... Args>
ptr<T>
make(context& ctx, Args&&... args) {
  return ctx.store.make<T>(std::forward<Args>(args)...);
}

template <typename T, typename... Args>
tracked_ptr<T>
make_tracked(context& ctx, Args&&... args) {
  return tracked_ptr<T>{ctx.store, make<T>(ctx, std::forward<Args>(args)...)};
}

template <typename T, typename... Args>
weak_ptr<T>
make_weak(context& ctx, Args&&... args) {
  return weak_ptr<T>{ctx.store, make<T>(ctx, std::forward<Args>(args)...)};
}

inline tracked_ptr<>
track(context& ctx, ptr<> o) { return {ctx.store, o}; }

template <typename T>
tracked_ptr<T>
track(context& ctx, ptr<T> o) { return {ctx.store, o}; }

inline ptr<symbol>
context::intern_type_name(word_type type_index) {
  if (type_name_symbols_.size() <= type_index)
    type_name_symbols_.resize(type_index + 1);

  ptr<symbol> sym = type_name_symbols_[type_index];
  if (!sym)
    sym = type_name_symbols_[type_index]
      = intern(types().types[type_index].name);
  return sym;
}

inline ptr<symbol>
type(context& ctx, ptr<> o) {
  if (is_object_ptr(o))
    return ctx.intern_type_name(object_type_index(o));
  else if (is_fixnum(o))
    return ctx.constants->integer_type_symbol;
  else if (is_character(o))
    return ctx.constants->character_type_symbol;
  else if (is_string_cursor(o))
    return ctx.constants->string_cursor_type_symbol;

  assert(false);
  return {};
}

} // namespace insider

#endif
