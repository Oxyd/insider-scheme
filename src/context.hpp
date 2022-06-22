#ifndef INSIDER_CONTEXT_HPP
#define INSIDER_CONTEXT_HPP

#include "compiler/scope.hpp"
#include "memory/free_store.hpp"
#include "module_resolver.hpp"
#include "object.hpp"
#include "ptr.hpp"
#include "runtime/compare.hpp"
#include "vm/bytecode.hpp"
#include "vm/operand.hpp"

#include <map>
#include <tuple>
#include <vector>

namespace insider {

class boolean;
class context;
class core_form_type;
class eof_type;
class integer;
class module_;
class null_type;
class parameter_map;
class procedure;
class scope;
class symbol;
class tail_call_tag_type;
class textual_output_port;
class transformer;
class void_type;

class execution_state;

class source_code_provider;

// Some top-level values are tagged to let the compiler understand them and
// optimise them.
enum class special_top_level_tag {
  plus,
  minus,
  times,
  divide,
  arith_equal,
  less_than,
  less_or_equal,
  greater_than,
  greater_or_equal,
  vector_set,
  vector_ref,
  type,
  cons,
  car,
  cdr,
  eq,
  box,
  unbox,
  box_set
};

// Evaluation context.
class context {
public:
  struct constants {
    ptr<insider::null_type> null;
    ptr<insider::void_type> void_;
    ptr<boolean>            t, f;     // #t and #f.
    ptr<eof_type>           eof;
    ptr<tail_call_tag_type> tail_call_tag;
    ptr<core_form_type>
      let, letrec_star, set, lambda, if_, define, define_syntax, begin, quote,
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
  };

  struct statics_list {
    operand
      null,
      void_,
      t, f,
      zero,
      one
      ;
  };

  free_store                       store;
  std::unique_ptr<constants>       constants;
  statics_list                     statics{};
  // Built from actions during stack unwinding.
  std::string                      error_backtrace;
  bytecode                         program;
  std::unique_ptr<execution_state> current_execution;
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

  operand
  intern_static(ptr<> const&);

  ptr<>
  get_static(operand i) const {
    assert(i >= 0);
    assert(static_cast<std::size_t>(i) < statics_.size());
    return statics_[i];
  }

  ptr<>
  get_static_checked(operand) const;

  ptr<>
  get_top_level(operand i) const { return top_level_objects_[i]; }

  ptr<>
  get_top_level_checked(operand) const;

  void
  set_top_level(operand i, ptr<>);

  operand
  add_top_level(ptr<>, std::string name);

  std::string
  get_top_level_name(operand) const;

  void
  tag_top_level(operand, special_top_level_tag);

  std::optional<special_top_level_tag>
  find_tag(operand) const;

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

  root_provider root_provider_{*this};
  std::unordered_map<std::string, ptr<symbol>> interned_symbols_;
  std::vector<ptr<>> statics_;
  eqv_unordered_map<ptr<>, operand> statics_cache_;
  std::vector<ptr<>> top_level_objects_;
  std::vector<std::string> top_level_binding_names_;
  std::unordered_map<operand, special_top_level_tag> top_level_tags_;
  insider::module_resolver module_resolver_{*this};

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
type(context& ctx, ptr<> o) {
  if (is_object_ptr(o))
    return ctx.intern(object_type(o).name);
  else if (is_fixnum(o))
    return ctx.intern(integer_type_name);
  else if (is_character(o))
    return ctx.intern(character_type_name);

  assert(false);
  return {};
}

} // namespace insider

#endif
