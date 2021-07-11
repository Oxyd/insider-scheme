#ifndef INSIDER_CONTEXT_HPP
#define INSIDER_CONTEXT_HPP

#include "compare.hpp"
#include "free_store.hpp"
#include "module.hpp"
#include "object.hpp"
#include "operand.hpp"
#include "ptr.hpp"

#include <map>
#include <tuple>
#include <vector>

namespace insider {

class parameter_tag;

// Flat map of parameter_tag's to values.
class parameter_map : public composite_object<parameter_map> {
public:
  static constexpr char const* scheme_name = "insider::parameter_map";

  ptr<>&
  find_value(ptr<parameter_tag>);

  void
  add_value(ptr<parameter_tag>, ptr<>);

  void
  visit_members(member_visitor const&);

private:
  std::vector<std::tuple<ptr<parameter_tag>, ptr<>>> values_;
};

class boolean;
class context;
class core_form_type;
class integer;
class module;
class null_type;
class port;
class procedure;
class scope;
class symbol;
class tail_call_tag_type;
class transformer;
class void_type;

class execution_state;

// SOME top-level values are tagged to let the compiler understand them and
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
  vector_ref
};

// Evaluation context.
class context {
public:
  struct constants {
    tracked_ptr<insider::null_type> null;
    tracked_ptr<insider::void_type> void_;
    tracked_ptr<boolean>        t, f;     // #t and #f.
    tracked_ptr<tail_call_tag_type> tail_call_tag;
    tracked_ptr<core_form_type>
      let, letrec_star, set, lambda, if_, box, unbox, box_set, define, define_syntax,
      begin, begin_for_syntax, quote, quasiquote, unquote, unquote_splicing,
      syntax, quasisyntax, unsyntax, unsyntax_splicing, syntax_trap, syntax_error,
      let_syntax, letrec_syntax;
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
  statics_list                     statics;
  tracked_ptr<port>                output_port;
  module                           internal_module; // (insider internal)
  std::string                      error_backtrace; // Built from actions during stack unwinding.
  bytecode                         program;
  std::unique_ptr<execution_state> current_execution;
  tracked_ptr<parameter_map>       parameters;

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
  intern_static(tracked_ptr<> const&);

  ptr<>
  get_static(operand i) const {
    assert(i < statics_.size());
    return statics_[i].get();
  }

  ptr<>
  get_static_checked(operand) const;

  ptr<>
  get_top_level(operand i) const { return top_level_objects_[i].get(); }

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
  load_library_module(std::vector<tracked_ptr<syntax>> const&);

  module*
  find_module(module_name const&);

  void
  prepend_module_provider(std::unique_ptr<module_provider>);

  void
  append_module_provider(std::unique_ptr<module_provider>);

private:
  std::unordered_map<std::string, weak_ptr<symbol>> interned_symbols_;
  std::vector<tracked_ptr<>> statics_;
  eqv_unordered_map<std::size_t> statics_cache_;
  std::vector<tracked_ptr<>> top_level_objects_;
  std::vector<std::string> top_level_binding_names_;
  std::unordered_map<operand, special_top_level_tag> top_level_tags_;
  std::map<module_name, protomodule> protomodules_;
  std::map<module_name, std::unique_ptr<module>> modules_;
  std::vector<std::unique_ptr<module_provider>> module_providers_;
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

inline tracked_ptr<>
track(context& ctx, ptr<> o) { return {ctx.store, o}; }

template <typename T>
tracked_ptr<T>
track(context& ctx, ptr<T> o) { return {ctx.store, o}; }

} // namespace insider

#endif
