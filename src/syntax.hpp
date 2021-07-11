#ifndef INSIDER_SYNTAX_HPP
#define INSIDER_SYNTAX_HPP

#include "compare.hpp"
#include "expression.hpp"
#include "object.hpp"
#include "object_conversions.hpp"
#include "ptr.hpp"
#include "scope.hpp"
#include "source_location.hpp"

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

std::optional<scope::value_type>
lookup(ptr<syntax> id);

// Part of the programs' source code. It is an S-expression together with
// information about the source code location.
class syntax : public composite_object<syntax> {
public:
  static constexpr char const* scheme_name = "insider::syntax";

  syntax(ptr<> expr, source_location loc)
    : expression_{expr}
    , location_{std::move(loc)}
  {
    assert(expr);
  }

  syntax(ptr<> expr, scope_set envs)
    : expression_{expr}
    , scopes_{std::move(envs)}
  { }

  syntax(ptr<> expr, source_location loc, scope_set scopes)
    : expression_{expr}
    , location_{std::move(loc)}
    , scopes_{std::move(scopes)}
  { }

  ptr<>
  expression() const { return expression_; }

  source_location const&
  location() const { return location_; }

  scope_set const&
  scopes() const { return scopes_; }

  void
  add_scope(free_store&, ptr<scope>);

  void
  remove_scope(ptr<scope>);

  void
  flip_scope(free_store&, ptr<scope>);

  void
  visit_members(member_visitor const&);

private:
  ptr<>           expression_;
  source_location location_;
  scope_set       scopes_;
};

template <typename T>
bool
syntax_is(ptr<syntax> stx) {
  return is<T>(stx->expression());
}

template <typename T>
auto
syntax_match(ptr<syntax> stx) {
  return match<T>(stx->expression());
}

template <typename T>
auto
syntax_expect(ptr<syntax> stx) {
  return expect<T>(stx->expression());
}

template <typename T>
auto
syntax_assume(ptr<syntax> stx) {
  return assume<T>(stx->expression());
}

template <typename T>
auto
semisyntax_is(ptr<> x) {
  if (auto stx = match<syntax>(x))
    return is<T>(stx->expression());
  else
    return is<T>(x);
}

template <typename T>
auto
semisyntax_match(ptr<> x) {
  if (auto stx = match<syntax>(x))
    return match<T>(stx->expression());
  else
    return match<T>(x);
}

template <typename T>
auto
semisyntax_expect(ptr<> x) {
  if (auto stx = match<syntax>(x))
    return expect<T>(stx->expression());
  else
    return expect<T>(x);
}

template <typename T>
auto
semisyntax_assume(ptr<> x) {
  if (auto stx = match<syntax>(x))
    return assume<T>(stx->expression());
  else
    return assume<T>(x);
}

ptr<>
syntax_to_datum(context&, ptr<syntax>);

ptr<syntax>
datum_to_syntax(context&, ptr<syntax>, ptr<>);

ptr<>
syntax_to_list(context&, ptr<>);

ptr<syntax>
copy_syntax(context&, ptr<syntax>);

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
  visit_members(member_visitor const&);

private:
  insider::ptr<> callable_;
};

} // namespace insider

#endif
