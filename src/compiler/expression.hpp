#ifndef INSIDER_COMPILER_EXPRESSION_HPP
#define INSIDER_COMPILER_EXPRESSION_HPP

#include "compiler/module_name.hpp"
#include "compiler/source_file_origin.hpp"
#include "compiler/variable.hpp"
#include "memory/free_store.hpp"
#include "memory/root_provider.hpp"
#include "util/depth_first_search.hpp"
#include "vm/bytecode.hpp"

#include <memory>
#include <string>
#include <utility>
#include <variant>
#include <vector>

namespace insider {

class syntax;
class transformer;

struct expression;

struct literal_expression {
  tracked_ptr<> value;

  explicit
  literal_expression(tracked_ptr<> const& value) : value{value} { }

  template <typename F>
  void
  visit_subexpressions(F&&) { }
};

struct local_reference_expression {
  std::shared_ptr<insider::variable> variable;

  explicit
  local_reference_expression(std::shared_ptr<insider::variable> var)
    : variable{std::move(var)}
  { }

  template <typename F>
  void
  visit_subexpressions(F&&) { }
};

struct top_level_reference_expression {
  operand location;
  std::string name;

  top_level_reference_expression(operand location, std::string name)
    : location{location}
    , name{std::move(name)}
  { }

  template <typename F>
  void
  visit_subexpressions(F&&) { }
};

struct unknown_reference_expression {
  tracked_ptr<syntax> name;

  explicit
  unknown_reference_expression(tracked_ptr<syntax> name)
    : name{std::move(name)}
  { }

  template <typename F>
  void
  visit_subexpressions(F&&) { }
};

struct application_expression {
  std::unique_ptr<expression> target;
  std::vector<std::unique_ptr<expression>> arguments;

  application_expression(std::unique_ptr<expression> t,
                         std::vector<std::unique_ptr<expression>> args)
    : target{std::move(t)}
    , arguments{std::move(args)}
  { }

  template <typename... Ts>
  application_expression(std::unique_ptr<expression> t, Ts&&... ts)
    : target{std::move(t)}
  {
    arguments.reserve(sizeof...(Ts));
    (arguments.push_back(std::move(ts)), ...);
  }

  template <typename F>
  void
  visit_subexpressions(F&& f) {
    f(target.get());
    for (auto const& arg : arguments)
      f(arg.get());
  }
};

struct sequence_expression {
  std::vector<std::unique_ptr<expression>> expressions;

  sequence_expression() = default;

  explicit
  sequence_expression(std::vector<std::unique_ptr<expression>> exprs)
    : expressions{std::move(exprs)}
  { }

  template <typename F>
  void
  visit_subexpressions(F&& f) {
    for (std::unique_ptr<expression> const& e : expressions)
      f(e.get());
  }
};

struct definition_pair_expression {
  tracked_ptr<syntax>                  id;
  std::shared_ptr<insider::variable>   variable;
  std::unique_ptr<insider::expression> expression;

  definition_pair_expression(tracked_ptr<syntax> id,
                             std::shared_ptr<insider::variable> var,
                             std::unique_ptr<insider::expression> expr)
    : id{std::move(id)}
    , variable{std::move(var)}
    , expression{std::move(expr)}
  { }
};

struct let_expression {
  std::vector<definition_pair_expression> definitions;
  sequence_expression body;

  let_expression(std::vector<definition_pair_expression> defs,
                 sequence_expression body)
    : definitions{std::move(defs)}
    , body{std::move(body)}
  { }

  template <typename F>
  void
  visit_subexpressions(F&& f) {
    for (auto const& def : definitions)
      f(def.expression.get());
    for (auto const& expr : body.expressions)
      f(expr.get());
  }
};

struct local_set_expression {
  std::shared_ptr<variable>            target;
  std::unique_ptr<insider::expression> expression;

  local_set_expression(std::shared_ptr<variable> target,
                       std::unique_ptr<insider::expression> expr)
    : target{std::move(target)}
    , expression{std::move(expr)}
  { }

  template <typename F>
  void
  visit_subexpressions(F&& f) {
    f(expression.get());
  }
};

struct top_level_set_expression {
  operand location;
  std::unique_ptr<insider::expression> expression;

  top_level_set_expression(operand location,
                           std::unique_ptr<insider::expression> expr)
    : location{location}
    , expression{std::move(expr)}
  { }

  template <typename F>
  void
  visit_subexpressions(F&& f) {
    f(expression.get());
  }
};

struct lambda_expression {
  std::vector<std::shared_ptr<variable>> parameters;
  bool has_rest;
  sequence_expression body;
  std::optional<std::string> name;
  std::vector<std::shared_ptr<variable>> free_variables;

  lambda_expression(std::vector<std::shared_ptr<variable>> parameters,
                    bool has_rest,
                    sequence_expression body,
                    std::optional<std::string> name,
                    std::vector<std::shared_ptr<variable>> free_variables)
    : parameters{std::move(parameters)}
    , has_rest{has_rest}
    , body{std::move(body)}
    , name{std::move(name)}
    , free_variables{std::move(free_variables)}
  { }

  template <typename F>
  void
  visit_subexpressions(F&& f) {
    for (auto const& expr : body.expressions)
      f(expr.get());
  }
};

struct if_expression {
  std::unique_ptr<expression> test;
  std::unique_ptr<expression> consequent;
  std::unique_ptr<expression> alternative;

  if_expression(std::unique_ptr<expression> test,
                std::unique_ptr<expression> consequent,
                std::unique_ptr<expression> alternative)
    : test{std::move(test)}
    , consequent{std::move(consequent)}
    , alternative{std::move(alternative)}
  { }

  template <typename F>
  void
  visit_subexpressions(F&& f) {
    f(test.get());
    f(consequent.get());
    if (alternative)
      f(alternative.get());
  }
};

struct expression {
  using value_type = std::variant<
    literal_expression,
    local_reference_expression,
    top_level_reference_expression,
    unknown_reference_expression,
    application_expression,
    let_expression,
    local_set_expression,
    top_level_set_expression,
    lambda_expression,
    if_expression,
    sequence_expression
  >;

  value_type value;

  explicit
  expression(value_type value)
    : value{std::move(value)}
  { }
};

std::unique_ptr<expression>
make_internal_reference(context& ctx, std::string name);

template <typename T, typename... Args>
std::unique_ptr<expression>
make_expression(Args&&... args) {
  return std::make_unique<expression>(
    expression{T(std::forward<Args>(args)...)}
  );
}

template <typename... Args>
static application_expression
make_application_expression(context& ctx, std::string const& name,
                            Args&&... args) {
  return application_expression{
    make_internal_reference(ctx, name),
    std::forward<Args>(args)...
  };
}

template <typename... Args>
static std::unique_ptr<expression>
make_application(context& ctx, std::string const& name, Args&&... args) {
  return std::make_unique<expression>(make_application_expression(
    ctx, name, std::forward<Args>(args)...
  ));
}

inline void
push_children(auto& expr, dfs_stack<expression*>& stack) {
  expr.visit_subexpressions([&] (expression* child) {
    stack.push_back(child);
  });
}

class expression_visitor : public dfs_visitor {
public:
  void
  enter(expression* e, dfs_stack<expression*>& stack) {
    std::visit([&] (auto& expr) {
                 enter_expression(expr);
                 push_children(expr, stack);
               },
               e->value);
  }

  void
  leave(expression* e) {
    std::visit([&] (auto& expr) { leave_expression(expr); }, e->value);
  }

private:
  virtual void
  enter_expression(literal_expression&) { }

  virtual void
  leave_expression(literal_expression&) { }

  virtual void
  enter_expression(local_reference_expression&) { }

  virtual void
  leave_expression(local_reference_expression&) { }

  virtual void
  enter_expression(top_level_reference_expression&) { }

  virtual void
  leave_expression(top_level_reference_expression&) { }

  virtual void
  enter_expression(unknown_reference_expression&) { }

  virtual void
  leave_expression(unknown_reference_expression&) { }

  virtual void
  enter_expression(application_expression&) { }

  virtual void
  leave_expression(application_expression&) { }

  virtual void
  enter_expression(let_expression&) { }

  virtual void
  leave_expression(let_expression&) { }

  virtual void
  enter_expression(local_set_expression&) { }

  virtual void
  leave_expression(local_set_expression&) { }

  virtual void
  enter_expression(top_level_set_expression&) { }

  virtual void
  leave_expression(top_level_set_expression&) { }

  virtual void
  enter_expression(lambda_expression&) { }

  virtual void
  leave_expression(lambda_expression&) { }

  virtual void
  enter_expression(if_expression&) { }

  virtual void
  leave_expression(if_expression&) { }

  virtual void
  enter_expression(sequence_expression&) { }

  virtual void
  leave_expression(sequence_expression&) { }
};

template <typename F>
void
traverse_postorder(expression* e, F&& f) {
  struct visitor : dfs_visitor {
    F& f;

    explicit
    visitor(F& f) : f{f} { }

    void
    enter(expression* e, dfs_stack<expression*>& stack) {
      std::visit([&] (auto& expr) { push_children(expr, stack); },
                 e->value);
    }

    void
    leave(expression* expr) {
      f(expr);
    }
  } v{f};

  depth_first_search(e, v);
}

} // namespace insider

#endif
