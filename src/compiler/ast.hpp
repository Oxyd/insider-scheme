#ifndef INSIDER_COMPILER_AST_HPP
#define INSIDER_COMPILER_AST_HPP

#include "compiler/expression.hpp"
#include "compiler/module_name.hpp"
#include "compiler/source_file_origin.hpp"
#include "compiler/variable.hpp"
#include "memory/free_store.hpp"
#include "memory/root_provider.hpp"
#include "object.hpp"
#include "util/depth_first_search.hpp"
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

using result_stack = std::vector<expression>;

class literal_expression : public composite_object<literal_expression> {
public:
  static constexpr char const* scheme_name = "insider::literal_expression";

  explicit
  literal_expression(ptr<> value);

  ptr<>
  value() const { return value_; }

  template <typename F>
  void
  visit_subexpressions(F&&) const { }

  void
  visit_members(member_visitor const& f);

  void
  update(context&, result_stack&);

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
  local_reference_expression(ptr<local_variable> var);

  ptr<local_variable>
  variable() const { return variable_; }

  template <typename F>
  void
  visit_subexpressions(F&&) const { }

  void
  visit_members(member_visitor const& f);

  void
  update(context&, result_stack&);

private:
  ptr<local_variable> variable_;
};

class top_level_reference_expression
  : public composite_object<top_level_reference_expression>
{
public:
  static constexpr char const* scheme_name
    = "insider::top_level_reference_expression";

  explicit
  top_level_reference_expression(ptr<top_level_variable>);

  ptr<top_level_variable>
  variable() const { return variable_; }

  template <typename F>
  void
  visit_subexpressions(F&&) const { }

  void
  visit_members(member_visitor const& f);

  void
  update(context&, result_stack&);

private:
  ptr<top_level_variable> variable_;
};

class unknown_reference_expression
  : public composite_object<unknown_reference_expression>
{
public:
  static constexpr char const* scheme_name
    = "insider::unknown_reference_expression";

  explicit
  unknown_reference_expression(ptr<syntax> name);

  ptr<syntax>
  name() const { return name_; }

  template <typename F>
  void
  visit_subexpressions(F&&) const { }

  void
  visit_members(member_visitor const& f);

  void
  update(context&, result_stack&);

private:
  ptr<syntax> name_;
};

class application_expression : public composite_object<application_expression> {
public:
  static constexpr char const* scheme_name = "insider::application_expression";

  application_expression(expression t, std::vector<expression> args);

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
  visit_members(member_visitor const& f);

  void
  update(context&, result_stack&);

private:
  expression              target_;
  std::vector<expression> arguments_;
};

class sequence_expression : public composite_object<sequence_expression>  {
public:
  static constexpr char const* scheme_name = "insider::sequence_expression";

  sequence_expression() = default;

  explicit
  sequence_expression(std::vector<expression> exprs);

  explicit
  sequence_expression(std::ranges::range auto exprs)
    : expressions_(exprs.begin(), exprs.end())
  { }

  std::vector<expression> const&
  expressions() const { return expressions_; }

  void
  prepend_expression(free_store& fs, expression expr);

  template <typename F>
  void
  visit_subexpressions(F&& f) const {
    for (expression e : expressions_)
      f(e);
  }

  void
  visit_members(member_visitor const& f);

  void
  update(context&, result_stack&);

private:
  std::vector<expression> expressions_;
};

class definition_pair_expression {
public:
  definition_pair_expression(ptr<local_variable> var,
                             insider::expression expr);

  ptr<local_variable>
  variable() const { return variable_; }

  insider::expression
  expression() const { return expression_; }

  void
  visit_members(member_visitor const& f);

  bool
  operator == (definition_pair_expression const&) const = default;

private:
  ptr<local_variable> variable_;
  insider::expression expression_;
};

class let_expression : public composite_object<let_expression> {
public:
  static constexpr char const* scheme_name = "insider::let_expression";

  let_expression(std::vector<definition_pair_expression> defs,
                 ptr<sequence_expression> body);

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
  visit_members(member_visitor const& f);

  void
  update(context&, result_stack&);

private:
  std::vector<definition_pair_expression> definitions_;
  ptr<sequence_expression>                body_;
};

class local_set_expression : public composite_object<local_set_expression> {
public:
  static constexpr char const* scheme_name = "insider::local_set_expression";

  local_set_expression(ptr<local_variable> target, insider::expression expr);

  ptr<local_variable>
  target() const { return target_; }

  insider::expression
  expression() const { return expression_; }

  template <typename F>
  void
  visit_subexpressions(F&& f) const {
    f(expression_);
  }

  void
  visit_members(member_visitor const& f);

  void
  update(context&, result_stack&);

private:
  ptr<local_variable> target_;
  insider::expression expression_;
};

class top_level_set_expression
  : public composite_object<top_level_set_expression>
{
public:
  static constexpr char const* scheme_name
    = "insider::top_level_set_expression";

  top_level_set_expression(ptr<top_level_variable> var, expression expr,
                           bool is_init);

  ptr<top_level_variable>
  target() const { return variable_; }

  insider::expression
  expression() const { return expression_; }

  bool
  is_initialisation() const { return is_init_; }

  template <typename F>
  void
  visit_subexpressions(F&& f) const {
    f(expression_);
  }

  void
  visit_members(member_visitor const& f);

  void
  update(context&, result_stack&);

private:
  ptr<top_level_variable> variable_;
  insider::expression     expression_;
  bool                    is_init_;
};

class lambda_expression : public composite_object<lambda_expression> {
public:
  static constexpr char const* scheme_name = "insider::lambda_expression";

  lambda_expression(std::vector<ptr<local_variable>> parameters,
                    bool has_rest,
                    ptr<sequence_expression> body,
                    std::optional<std::string> name,
                    std::vector<ptr<local_variable>> free_variables);

  std::vector<ptr<local_variable>> const&
  parameters() const { return parameters_; }

  bool
  has_rest() const { return has_rest_; }

  ptr<sequence_expression>
  body() { return body_; }

  std::optional<std::string> const&
  name() const { return name_; }

  void
  set_name(std::string n) { name_ = std::move(n); }

  std::vector<ptr<local_variable>> const&
  free_variables() const { return free_variables_; }

  void
  add_free_variable(free_store& fs, ptr<local_variable> v);

  template <typename F>
  void
  visit_subexpressions(F&& f) const {
    f(body_);
  }

  void
  visit_members(member_visitor const& f);

  void
  update(context&, result_stack&);

private:
  std::vector<ptr<local_variable>> parameters_;
  bool                       has_rest_;
  ptr<sequence_expression>   body_;
  std::optional<std::string> name_;
  std::vector<ptr<local_variable>> free_variables_;
};

class if_expression : public composite_object<if_expression> {
public:
  static constexpr char const* scheme_name = "insider::if_expression";

  if_expression(expression test, expression consequent, expression alternative);

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
    f(alternative_);
  }

  void
  visit_members(member_visitor const& f);

  void
  update(context&, result_stack&);

private:
  expression test_;
  expression consequent_;
  expression alternative_;
};

expression
make_internal_reference(context& ctx, std::string const& name);

std::vector<expression>
untrack_expressions(std::vector<tracked_expression> const&);

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
  virtual
  ~expression_visitor() = default;

  void
  enter(expression e, dfs_stack<expression>& stack);

  void
  leave(expression e);

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

namespace detail {
  template <typename T>
  static ptr<T>
  duplicate(context& ctx, ptr<T> expr) {
    return make<T>(ctx, *expr);
  }

template <typename F, bool Copy>
  struct mapping_visitor {
    context& ctx;
    F& f;
    result_stack results;

    mapping_visitor(context& ctx, F& f)
      : ctx{ctx}
      , f{f}
    { }

    void
    enter(expression e, dfs_stack<expression>& stack) {
      visit([&] (auto expr) { push_children(expr, stack); },
            e);
    }

    void
    leave(expression e) {
      auto result = visit(
        [&] (auto expr) -> expression {
          if constexpr (Copy)
            expr = duplicate(ctx, expr);
          expr->update(ctx, results);
          return f(expr);
        },
        e
      );
      results.push_back(result);
    }
  };
}

template <typename F>
expression
map_ast(context& ctx, expression e, F&& f) {
  detail::mapping_visitor<F, false> v{ctx, f};
  depth_first_search(e, v);
  assert(v.results.size() == 1);
  return v.results.back();
}

template <typename F>
expression
map_ast_copy(context& ctx, expression e, F&& f) {
  detail::mapping_visitor<F, true> v{ctx, f};
  depth_first_search(e, v);
  assert(v.results.size() == 1);
  return v.results.back();
}

} // namespace insider

#endif
