#ifndef INSIDER_RUNTIME_SYNTAX_HPP
#define INSIDER_RUNTIME_SYNTAX_HPP

#include "compiler/scope.hpp"
#include "compiler/source_location.hpp"
#include "memory/member.hpp"
#include "object.hpp"
#include "runtime/compare.hpp"
#include "runtime/error.hpp"
#include "runtime/symbol.hpp"
#include "type_indexes.hpp"
#include "util/named_runtime_error.hpp"
#include "util/object_conversions.hpp"

#include <string>
#include <variant>

namespace insider {

class scope;
class symbol;
class syntax;

bool
is_identifier(ptr<>);

std::string
identifier_name(ptr<syntax> x);

std::optional<scope::binding>
lookup(ptr<syntax> id);

// Part of the program source code. It is an expression together with
// information about the source code location and the associated set of scopes.
//
// Scopes are propagated lazily: Modifying the scope set creates a new syntax
// object with adjusted scopes, but this object points to the same expression as
// the original object, thus the scope sets of the children are not updated. In
// this state, the syntax object is called "dirty".
//
// When the expression of a dirty syntax object is accessed, it first modifies
// its children's scope sets accordingly, making the children dirty and removing
// the dirty flag from itself. This necessitates duplicating the expression
// object it contains to make it point to the new syntax subobjects.
class syntax : public composite_object<syntax> {
public:
  static constexpr char const* scheme_name = "insider::syntax";
  static constexpr word_type static_type_index = type_indexes::syntax;

  struct update_record {
    scope_set_operation operation;
    ptr<insider::scope> scope;
  };

  explicit
  syntax(ptr<> expr);
  syntax(ptr<> expr, source_location loc);
  syntax(ptr<> expr, scope_set envs);
  syntax(ptr<> expr, source_location loc, scope_set scopes,
         std::vector<update_record> update_records = {});

  ptr<>
  update_and_get_expression(context&);

  ptr<symbol>
  get_symbol() const {
    assert(contains<symbol>());
    return assume<symbol>(expression_.get());
  }

  ptr<>
  get_expression_without_update() const { return expression_.get(); }

  source_location const&
  location() const { return location_; }

  scope_set const&
  scopes() const { return scopes_; }

  template <typename T>
  bool
  contains() const {
    return is<T>(expression_.get());
  }

  [[nodiscard]]
  ptr<syntax>
  add_scope(free_store&, ptr<scope>) const;

  [[nodiscard]]
  ptr<syntax>
  remove_scope(free_store&, ptr<scope>) const;

  [[nodiscard]]
  ptr<syntax>
  flip_scope(free_store&, ptr<scope>) const;

  void
  visit_members(member_visitor const&) const;

  bool
  dirty() const { return !update_records_.empty(); }

  std::vector<update_record>
  update_records() const { return update_records_; }

private:
  member_ptr<>               expression_;
  source_location            location_;
  scope_set                  scopes_;
  std::vector<update_record> update_records_;

  ptr<syntax>
  update_scope(free_store&, ptr<scope>, scope_set_operation) const;

  void
  update_children(context&);
};

template <typename T>
bool
syntax_is(ptr<syntax> stx) {
  return stx->template contains<T>();
}

template <typename T>
auto
syntax_match(context& ctx, ptr<syntax> stx) {
  return match<T>(stx->update_and_get_expression(ctx));
}

template <typename T>
auto
syntax_match_without_update(ptr<syntax> stx) {
  return match<T>(stx->get_expression_without_update());
}

template <typename T>
auto
syntax_expect(context& ctx, ptr<syntax> stx) {
  return expect<T>(stx->update_and_get_expression(ctx));
}

template <typename T>
auto
syntax_expect_without_update(ptr<syntax> stx) {
  return expect<T>(stx->get_expression_without_update());
}

template <typename T>
auto
syntax_assume(context& ctx, ptr<syntax> stx) {
  return assume<T>(stx->update_and_get_expression(ctx));
}

template <typename T>
auto
syntax_assume_without_update(ptr<syntax> stx) {
  return assume<T>(stx->get_expression_without_update());
}

template <typename T>
auto
semisyntax_is(ptr<> x) {
  if (auto stx = match<syntax>(x))
    return stx->template contains<T>();
  else
    return is<T>(x);
}

template <typename T>
auto
semisyntax_match(context& ctx, ptr<> x) {
  if (auto stx = match<syntax>(x))
    return match<T>(stx->update_and_get_expression(ctx));
  else
    return match<T>(x);
}

template <typename T>
auto
semisyntax_expect(context& ctx, ptr<> x) {
  if (auto stx = match<syntax>(x))
    return expect<T>(stx->update_and_get_expression(ctx));
  else
    return expect<T>(x);
}

template <typename T>
auto
semisyntax_assume(context& ctx, ptr<> x) {
  if (auto stx = match<syntax>(x))
    return assume<T>(stx->update_and_get_expression(ctx));
  else
    return assume<T>(x);
}

ptr<>
syntax_to_datum(context&, ptr<syntax>);

ptr<syntax>
datum_to_syntax(context&, ptr<syntax>, ptr<>);

ptr<>
syntax_to_list(context&, ptr<>);

// A procedure for transforming syntax to syntax.
class transformer : public composite_object<transformer> {
public:
  static constexpr char const* scheme_name = "insider::transformer";

  explicit
  transformer(ptr<> callable)
    : callable_{callable}
  { }

  ptr<>
  callable() const { return callable_; }

  void
  visit_members(member_visitor const&) const;

private:
  ptr<> const callable_;
};

bool
free_identifier_eq(ptr<syntax> x, ptr<syntax> y);

using syntax_error = named_runtime_error<class syntax_error_tag>;
using unbound_variable_error
  = named_runtime_error<class unbound_variable_error_tag>;
using out_of_scope_variable_error
  = named_runtime_error<class out_of_scope_variable_error_tag>;
using immutable_binding_error
  = named_runtime_error<class immutable_binding_error_tag>;

template <typename Error, typename... Args>
Error
make_compile_error(source_location const& loc, std::string_view fmt,
                   Args&&... args) {
  return make_error<Error>("{}: {}", format_location(loc),
                           fmt::format(fmt::runtime(fmt),
                                       std::forward<Args>(args)...));
}

template <typename Error, typename... Args>
Error
make_compile_error(ptr<syntax> stx, std::string_view fmt, Args&&... args) {
  return make_compile_error<Error>(stx->location(), fmt,
                                   std::forward<Args>(args)...);
}

} // namespace insider

#endif
