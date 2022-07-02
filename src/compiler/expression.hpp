#ifndef INSIDER_COMPILER_EXPRESSION_HPP
#define INSIDER_COMPILER_EXPRESSION_HPP

#include "compiler/module_name.hpp"
#include "compiler/source_file_origin.hpp"
#include "compiler/variable.hpp"
#include "memory/free_store.hpp"
#include "memory/root_provider.hpp"
#include "object.hpp"
#include "util/depth_first_search.hpp"
#include "util/sum_type.hpp"
#include "vm/bytecode.hpp"

#include <memory>
#include <ranges>
#include <string>
#include <utility>
#include <variant>
#include <vector>

namespace insider {

class syntax;
class transformer;

using expression = sum_type<
  class literal_expression,
  class local_reference_expression,
  class top_level_reference_expression,
  class unknown_reference_expression,
  class application_expression,
  class sequence_expression,
  class let_expression,
  class local_set_expression,
  class top_level_set_expression,
  class lambda_expression,
  class if_expression
>;

using tracked_expression = tracked_sum_type<expression>;

class literal_expression : public composite_object<literal_expression> {
public:
  static constexpr char const* scheme_name = "insider::literal_expression";

  explicit
  literal_expression(ptr<> value) : value_{value} { }

  ptr<>
  value() const { return value_; }

  template <typename F>
  void
  visit_subexpressions(F&&) const { }

  void
  visit_members(member_visitor const& f) { f(value_); }

private:
  ptr<> value_;
};

class local_reference_expression
  : public composite_object<local_reference_expression>
{
public:
  static constexpr char const* scheme_name
    = "insider::local_reference_expression";

  explicit
  local_reference_expression(ptr<insider::variable> var)
    : variable_{var}
  { }

  ptr<insider::variable>
  variable() const { return variable_; }

  template <typename F>
  void
  visit_subexpressions(F&&) const { }

  void
  visit_members(member_visitor const& f) { f(variable_); }

private:
  ptr<insider::variable> variable_;
};

class top_level_reference_expression
  : public leaf_object<top_level_reference_expression>
{
public:
  static constexpr char const* scheme_name
    = "insider::top_level_reference_expression";

  operand location;
  std::string name;

  top_level_reference_expression(operand location, std::string name)
    : location{location}
    , name{std::move(name)}
  { }

  template <typename F>
  void
  visit_subexpressions(F&&) const { }
};

class unknown_reference_expression
  : public composite_object<unknown_reference_expression>
{
public:
  static constexpr char const* scheme_name
    = "insider::unknown_reference_expression";

  explicit
  unknown_reference_expression(ptr<syntax> name)
    : name_{name}
  { }

  ptr<syntax>
  name() const { return name_; }

  template <typename F>
  void
  visit_subexpressions(F&&) const { }

  void
  visit_members(member_visitor const& f) { f(name_); }

private:
  ptr<syntax> name_;
};

class application_expression : public composite_object<application_expression> {
public:
  static constexpr char const* scheme_name = "insider::application_expression";

  application_expression(expression t, std::vector<expression> args)
    : target_{t}
    , arguments_{std::move(args)}
  { }

  application_expression(expression t, std::ranges::range auto args)
    : target_{t}
    , arguments_(args.begin(), args.end())
  { }

  template <typename... Ts>
  application_expression(expression t, Ts&&... ts)
    : target_{t}
  {
    arguments_.reserve(sizeof...(Ts));
    (arguments_.push_back(ts), ...);
  }

  expression
  target() const { return target_; }

  std::vector<expression> const&
  arguments() const { return arguments_; }

  template <typename F>
  void
  visit_subexpressions(F&& f) const {
    f(target_);
    for (auto const& arg : arguments_)
      f(arg);
  }

  void
  visit_members(member_visitor const& f) {
    target_.visit_members(f);
    for (auto& arg : arguments_)
      arg.visit_members(f);
  }

private:
  expression              target_;
  std::vector<expression> arguments_;
};

class sequence_expression : public composite_object<sequence_expression>  {
public:
  static constexpr char const* scheme_name = "insider::sequence_expression";

  sequence_expression() = default;

  explicit
  sequence_expression(std::vector<expression> exprs)
    : expressions_{std::move(exprs)}
  { }

  explicit
  sequence_expression(std::ranges::range auto exprs)
    : expressions_(exprs.begin(), exprs.end())
  { }

  std::vector<expression> const&
  expressions() const { return expressions_; }

  void
  prepend_expression(free_store& fs, expression expr) {
    expressions_.insert(expressions_.begin(), expr);
    fs.notify_arc(this, expr.get());
  }

  template <typename F>
  void
  visit_subexpressions(F&& f) const {
    for (expression e : expressions_)
      f(e);
  }

  void
  visit_members(member_visitor const& f) {
    for (auto& e : expressions_)
      e.visit_members(f);
  }

private:
  std::vector<expression> expressions_;
};

class definition_pair_expression {
public:
  definition_pair_expression(ptr<syntax> id,
                             ptr<insider::variable> var,
                             insider::expression expr)
    : id_{id}
    , variable_{var}
    , expression_{expr}
  { }

  ptr<syntax>
  id() const { return id_; }

  ptr<insider::variable>
  variable() const { return variable_; }

  insider::expression
  expression() const { return expression_; }

  void
  visit_members(member_visitor const& f) {
    f(id_);
    f(variable_);
    expression_.visit_members(f);
  }

private:
  ptr<syntax>            id_;
  ptr<insider::variable> variable_;
  insider::expression    expression_;
};

class let_expression : public composite_object<let_expression> {
public:
  static constexpr char const* scheme_name = "insider::let_expression";

  let_expression(std::vector<definition_pair_expression> defs,
                 ptr<sequence_expression> body)
    : definitions_{std::move(defs)}
    , body_{body}
  { }

  std::vector<definition_pair_expression> const&
  definitions() const { return definitions_; }

  ptr<sequence_expression>
  body() const { return body_; }

  template <typename F>
  void
  visit_subexpressions(F&& f) const {
    for (auto const& def : definitions_)
      f(def.expression());
    f(body_);
  }

  void
  visit_members(member_visitor const& f) {
    for (definition_pair_expression& dp : definitions_)
      dp.visit_members(f);
    f(body_);
  }

private:
  std::vector<definition_pair_expression> definitions_;
  ptr<sequence_expression>                body_;
};

class local_set_expression : public composite_object<local_set_expression> {
public:
  static constexpr char const* scheme_name = "insider::local_set_expression";

  local_set_expression(ptr<variable> target, insider::expression expr)
    : target_{target}
    , expression_{expr}
  { }

  ptr<variable>
  target() const { return target_; }

  insider::expression
  expression() const { return expression_; }

  template <typename F>
  void
  visit_subexpressions(F&& f) const {
    f(expression_);
  }

  void
  visit_members(member_visitor const& f) {
    f(target_);
    expression_.visit_members(f);
  }

private:
  ptr<variable>       target_;
  insider::expression expression_;
};

class top_level_set_expression
  : public composite_object<top_level_set_expression>
{
public:
  static constexpr char const* scheme_name
    = "insider::top_level_set_expression";

  top_level_set_expression(operand location, expression expr)
    : location_{location}
    , expression_{expr}
  { }

  operand
  location() const { return location_; }

  insider::expression
  expression() const { return expression_; }

  template <typename F>
  void
  visit_subexpressions(F&& f) const {
    f(expression_);
  }

  void
  visit_members(member_visitor const& f) {
    expression_.visit_members(f);
  }

private:
  operand             location_;
  insider::expression expression_;
};

class lambda_expression : public composite_object<lambda_expression> {
public:
  static constexpr char const* scheme_name = "insider::lambda_expression";

  lambda_expression(std::vector<ptr<variable>> parameters,
                    bool has_rest,
                    ptr<sequence_expression> body,
                    std::optional<std::string> name,
                    std::vector<ptr<variable>> free_variables)
    : parameters_{std::move(parameters)}
    , has_rest_{has_rest}
    , body_{body}
    , name_{std::move(name)}
    , free_variables_{std::move(free_variables)}
  { }

  std::vector<ptr<variable>> const&
  parameters() const { return parameters_; }

  bool
  has_rest() const { return has_rest_; }

  ptr<sequence_expression>
  body() { return body_; }

  std::optional<std::string> const&
  name() const { return name_; }

  void
  set_name(std::string n) { name_ = std::move(n); }

  std::vector<ptr<variable>> const&
  free_variables() const { return free_variables_; }

  void
  add_free_variable(free_store& fs, ptr<variable> v) {
    free_variables_.push_back(v);
    fs.notify_arc(this, v);
  }

  template <typename F>
  void
  visit_subexpressions(F&& f) const {
    f(body_);
  }

  void
  visit_members(member_visitor const& f) {
    for (auto& p : parameters_)
      f(p);
    f(body_);
    for (auto& fv : free_variables_)
      f(fv);
  }

private:
  std::vector<ptr<variable>> parameters_;
  bool                       has_rest_;
  ptr<sequence_expression>   body_;
  std::optional<std::string> name_;
  std::vector<ptr<variable>> free_variables_;

};

class if_expression : public composite_object<if_expression> {
public:
  static constexpr char const* scheme_name = "insider::if_expression";

  if_expression(expression test, expression consequent, expression alternative)
    : test_{test}
    , consequent_{consequent}
    , alternative_{alternative}
  { }

  expression
  test() const { return test_; }

  expression
  consequent() const { return consequent_; }

  expression
  alternative() const { return alternative_; }

  template <typename F>
  void
  visit_subexpressions(F&& f) const {
    f(test_);
    f(consequent_);
    if (alternative_)
      f(alternative_);
  }

  void
  visit_members(member_visitor const& f) {
    test_.visit_members(f);
    consequent_.visit_members(f);
    alternative_.visit_members(f);
  }

private:
  expression test_;
  expression consequent_;
  expression alternative_;
};

expression
make_internal_reference(context& ctx, std::string name);

template <typename... Args>
static expression
make_application(context& ctx, std::string const& name, Args&&... args) {
  return make<application_expression>(
    ctx,
    make_internal_reference(ctx, name),
    std::forward<Args>(args)...
  );
}

inline void
push_children(auto expr, dfs_stack<expression>& stack) {
  expr->visit_subexpressions([&] (expression child) {
    stack.push_back(child);
  });
}

class expression_visitor : public dfs_visitor {
public:
  void
  enter(expression e, dfs_stack<expression>& stack) {
    visit([&] (auto expr) {
            enter_expression(expr);
            push_children(expr, stack);
          },
          e);
  }

  void
  leave(expression e) {
    visit([&] (auto expr) { leave_expression(expr); }, e);
  }

private:
  virtual void
  enter_expression(ptr<literal_expression>) { }

  virtual void
  leave_expression(ptr<literal_expression>) { }

  virtual void
  enter_expression(ptr<local_reference_expression>) { }

  virtual void
  leave_expression(ptr<local_reference_expression>) { }

  virtual void
  enter_expression(ptr<top_level_reference_expression>) { }

  virtual void
  leave_expression(ptr<top_level_reference_expression>) { }

  virtual void
  enter_expression(ptr<unknown_reference_expression>) { }

  virtual void
  leave_expression(ptr<unknown_reference_expression>) { }

  virtual void
  enter_expression(ptr<application_expression>) { }

  virtual void
  leave_expression(ptr<application_expression>) { }

  virtual void
  enter_expression(ptr<let_expression>) { }

  virtual void
  leave_expression(ptr<let_expression>) { }

  virtual void
  enter_expression(ptr<local_set_expression>) { }

  virtual void
  leave_expression(ptr<local_set_expression>) { }

  virtual void
  enter_expression(ptr<top_level_set_expression>) { }

  virtual void
  leave_expression(ptr<top_level_set_expression>) { }

  virtual void
  enter_expression(ptr<lambda_expression>) { }

  virtual void
  leave_expression(ptr<lambda_expression>) { }

  virtual void
  enter_expression(ptr<if_expression>) { }

  virtual void
  leave_expression(ptr<if_expression>) { }

  virtual void
  enter_expression(ptr<sequence_expression>) { }

  virtual void
  leave_expression(ptr<sequence_expression>) { }
};

template <typename F>
void
traverse_postorder(expression e, F&& f) {
  struct visitor : dfs_visitor {
    F& f;

    explicit
    visitor(F& f) : f{f} { }

    void
    enter(expression e, dfs_stack<expression>& stack) {
      visit([&] (auto expr) { push_children(expr, stack); },
            e);
    }

    void
    leave(expression expr) {
      f(expr);
    }
  } v{f};

  depth_first_search(e, v);
}

} // namespace insider

#endif
