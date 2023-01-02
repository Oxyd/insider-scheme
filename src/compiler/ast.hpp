#ifndef INSIDER_COMPILER_AST_HPP
#define INSIDER_COMPILER_AST_HPP

#include "compiler/debug_info.hpp"
#include "compiler/expression.hpp"
#include "compiler/variable.hpp"
#include "memory/free_store.hpp"
#include "object.hpp"
#include "runtime/basic_types.hpp"
#include "util/depth_first_search.hpp"
#include "vm/bytecode.hpp"

#include <memory>
#include <ranges>
#include <string>
#include <utility>
#include <variant>
#include <vector>

namespace insider {

class native_procedure;
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
  visit_members(member_visitor const& f) const;

  void
  update(context&, result_stack&);

  std::size_t
  size_estimate() const { return 1; }

  std::string
  show(context&, std::size_t indent) const;

private:
  ptr<> const value_;
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
  visit_members(member_visitor const& f) const;

  void
  update(context&, result_stack&);

  std::size_t
  size_estimate() const { return 1; }

  std::string
  show(context&, std::size_t indent) const;

private:
  ptr<local_variable> const variable_;
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
  visit_members(member_visitor const& f) const;

  void
  update(context&, result_stack&);

  std::size_t
  size_estimate() const { return 1; }

  std::string
  show(context&, std::size_t indent) const;

private:
  ptr<top_level_variable> const variable_;
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
  visit_members(member_visitor const& f) const;

  void
  update(context&, result_stack&);

  std::size_t
  size_estimate() const { return 1; }

  std::string
  show(context&, std::size_t indent) const;

private:
  ptr<syntax> const name_;
};

class application_expression : public composite_object<application_expression> {
public:
  static constexpr char const* scheme_name = "insider::application_expression";

  enum class target_kind {
    generic, scheme, native
  };

  application_expression(expression t, std::vector<expression> args);

  application_expression(expression t, std::vector<expression> args,
                         std::vector<ptr<keyword>> arg_names);

  template <typename... Ts>
  application_expression(expression t, Ts&&... ts)
    : target_{t}
  {
    arguments_.reserve(sizeof...(Ts));
    (arguments_.push_back(ts), ...);

    argument_names_.resize(sizeof...(Ts));

    update_size_estimate();

    assert(arguments_.size() == argument_names_.size());
  }

  expression
  target() const { return target_; }

  std::vector<expression> const&
  arguments() const { return arguments_; }

  std::vector<ptr<keyword>> const&
  argument_names() const { return argument_names_; }

  std::optional<insider::debug_info>&
  debug_info() { return debug_info_; }

  target_kind
  kind() const { return kind_; }

  void
  set_kind(target_kind k) { kind_ = k; }

  template <typename F>
  void
  visit_subexpressions(F&& f) const {
    f(target_);
    for (auto const& arg : arguments_)
      f(arg);
  }

  void
  visit_members(member_visitor const& f) const;

  void
  update(context&, result_stack&);

  std::size_t
  size_estimate() const { return size_estimate_; }

  std::string
  show(context&, std::size_t indent) const;

private:
  expression                         target_;
  std::vector<expression>            arguments_;
  std::vector<ptr<keyword>>          argument_names_;
  std::size_t                        size_estimate_ = 0;
  std::optional<insider::debug_info> debug_info_;
  target_kind                        kind_ = target_kind::generic;

  void
  update_size_estimate();
};

class built_in_operation_expression
  : public composite_object<built_in_operation_expression>
{
public:
  static constexpr char const* scheme_name
    = "insider::built_in_operation_expression";

  built_in_operation_expression(opcode, std::vector<expression>,
                                bool has_result,
                                ptr<native_procedure> proc);

  opcode
  operation() const { return operation_; }

  std::vector<expression> const&
  operands() const { return operands_; }

  bool
  has_result() const { return has_result_; }

  ptr<native_procedure>
  procedure() const { return proc_; }

  template <typename F>
  void
  visit_subexpressions(F&& f) const {
    for (auto const& op : operands_)
      f(op);
  }

  void
  visit_members(member_visitor const&) const;

  void
  update(context&, result_stack&);

  std::size_t
  size_estimate() const { return size_estimate_; }

  std::string
  show(context&, std::size_t indent) const;

private:
  opcode                      operation_;
  std::vector<expression>     operands_;
  bool                        has_result_;
  ptr<native_procedure> const proc_;
  std::size_t                 size_estimate_ = 0;

  void
  update_size_estimate();
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
  {
    update_size_estimate();
  }

  std::vector<expression> const&
  expressions() const { return expressions_; }

  template <typename F>
  void
  visit_subexpressions(F&& f) const {
    for (expression e : expressions_ | std::views::reverse)
      f(e);
  }

  void
  visit_members(member_visitor const& f) const;

  void
  update(context&, result_stack&);

  std::size_t
  size_estimate() const { return size_estimate_; }

  std::string
  show(context&, std::size_t indent) const;

private:
  std::vector<expression> expressions_;
  std::size_t             size_estimate_ = 0;

  void
  update_size_estimate();
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
  visit_members(member_visitor const& f) const;

  bool
  operator == (definition_pair_expression const&) const = default;

private:
  ptr<local_variable> const variable_;
  insider::expression       expression_;
};

class let_expression : public composite_object<let_expression> {
public:
  static constexpr char const* scheme_name = "insider::let_expression";

  let_expression(std::vector<definition_pair_expression> defs, expression body);

  std::vector<definition_pair_expression> const&
  definitions() const { return definitions_; }

  expression
  body() const { return body_; }

  template <typename F>
  void
  visit_subexpressions(F&& f) const {
    f(body_);
    for (auto const& def : definitions_ | std::views::reverse)
      f(def.expression());
  }

  void
  visit_members(member_visitor const& f) const;

  void
  update(context&, result_stack&);

  std::size_t
  size_estimate() const { return size_estimate_; }

  std::string
  show(context&, std::size_t indent) const;

private:
  std::vector<definition_pair_expression> definitions_;
  expression                              body_;
  std::size_t                             size_estimate_ = 0;

  void
  update_size_estimate();
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
  visit_members(member_visitor const& f) const;

  void
  update(context&, result_stack&);

  std::size_t
  size_estimate() const { return size_estimate_; }

  std::string
  show(context&, std::size_t indent) const;

private:
  ptr<local_variable> const target_;
  insider::expression       expression_;
  std::size_t               size_estimate_ = 0;

  void
  update_size_estimate();
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
  visit_members(member_visitor const& f) const;

  void
  update(context&, result_stack&);

  std::size_t
  size_estimate() const { return size_estimate_; }

  std::string
  show(context&, std::size_t indent) const;

private:
  ptr<top_level_variable> const variable_;
  insider::expression           expression_;
  bool                          is_init_;
  std::size_t                   size_estimate_ = 0;

  void
  update_size_estimate();
};

class lambda_expression : public composite_object<lambda_expression> {
public:
  static constexpr char const* scheme_name = "insider::lambda_expression";

  struct parameter {
    ptr<local_variable> const variable;
    bool                      optional;

    void
    visit_members(member_visitor const& f) const;
  };

  // Duplicate a lambda expression with new self variable and without free
  // variables.
  lambda_expression(ptr<lambda_expression> source,
                    ptr<local_variable> new_self_variable);

  // Duplicate a lambda expression with a new body.
  lambda_expression(ptr<lambda_expression> source,
                    expression new_body);

  lambda_expression(context&,
                    std::vector<parameter> parameters,
                    std::vector<ptr<keyword>> parameter_names,
                    bool has_rest,
                    expression body,
                    std::string name,
                    std::vector<ptr<local_variable>> free_variables);

  std::vector<parameter> const&
  parameters() const { return parameters_; }

  std::vector<ptr<keyword>> const&
  parameter_names() const { return parameter_names_; }

  bool
  has_rest() const { return has_rest_; }

  expression
  body() { return body_; }

  void
  update_body(free_store&, expression new_body);

  std::string const&
  name() const { return name_; }

  void
  set_name(std::string n) { name_ = std::move(n); }

  std::vector<ptr<local_variable>> const&
  free_variables() const { return free_variables_; }

  void
  add_free_variable(free_store& fs, ptr<local_variable> v);

  ptr<local_variable>
  self_variable() const { return self_variable_; }

  std::size_t
  num_self_references() const { return num_self_references_; }

  void
  set_num_self_references(std::size_t n) { num_self_references_ = n; }

  void
  remove_self_reference() {
    assert(num_self_references_ > 0);
    --num_self_references_;
  }

  template <typename F>
  void
  visit_subexpressions(F&& f) const {
    f(body_);
  }

  void
  visit_members(member_visitor const& f) const;

  void
  update(context&, result_stack&);

  std::size_t
  size_estimate() const {
    return 1 + free_variables_.size();
  }

  std::string
  show(context&, std::size_t indent) const;

private:
  std::vector<parameter> const     parameters_;
  std::vector<ptr<keyword>> const  parameter_names_;
  bool                             has_rest_;
  expression                       body_;
  std::string                      name_;
  std::vector<ptr<local_variable>> free_variables_;
  ptr<local_variable> const        self_variable_;
  std::size_t                      size_estimate_       = 0;
  std::size_t                      num_self_references_ = 0;
};

std::size_t
required_parameter_count(ptr<lambda_expression> lambda);

std::size_t
optional_leading_parameter_count(ptr<lambda_expression> lambda);

std::size_t
leading_parameter_count(ptr<lambda_expression> lambda);

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
  visit_members(member_visitor const& f) const;

  void
  update(context&, result_stack&);

  std::size_t
  size_estimate() const { return size_estimate_; }

  std::string
  show(context&, std::size_t indent) const;

private:
  expression  test_;
  expression  consequent_;
  expression  alternative_;
  std::size_t size_estimate_ = 0;

  void
  update_size_estimate();
};

class loop_id : public leaf_object<loop_id> {
public:
  static constexpr char const* scheme_name = "insider::loop_id";
};

class loop_body : public composite_object<loop_body> {
public:
  static constexpr char const* scheme_name = "insider::loop_body";

  loop_body(expression body, ptr<loop_id> id,
            std::vector<ptr<local_variable>> loop_vars);

  expression
  body() const { return body_; }

  ptr<loop_id>
  id() const { return id_; }

  std::vector<ptr<local_variable>> const&
  variables() const { return vars_; }

  template <typename F>
  void
  visit_subexpressions(F&& f) const {
    f(body_);
  }

  void
  visit_members(member_visitor const&) const;

  void
  update(context&, result_stack&);

  std::size_t
  size_estimate() const;

  std::string
  show(context&, std::size_t indent) const;

private:
  expression                             body_;
  ptr<loop_id> const                     id_;
  std::vector<ptr<local_variable>> const vars_;
};

// Performs two things: First it assigns new values to the loop variables, then
// goes back to the corresponding loop_body. The assignments do not need to be
// boxed, so they are a part of this expression rather than explicit
// local_set_expression's.
class loop_continue : public composite_object<loop_continue> {
public:
  static constexpr char const* scheme_name = "insider::loop_continue";

  loop_continue(ptr<loop_id> id,
                std::vector<definition_pair_expression> loop_vars);

  ptr<loop_id>
  id() const { return id_; }

  std::vector<definition_pair_expression>
  variables() const { return vars_; }

  template <typename F>
  void
  visit_subexpressions(F&& f) const {
    for (auto const& dp : vars_ | std::views::reverse)
      f(dp.expression());
  }

  void
  visit_members(member_visitor const&) const;

  void
  update(context&, result_stack&);

  std::size_t
  size_estimate() const { return size_estimate_; }

  std::string
  show(context&, std::size_t indent) const;

private:
  ptr<loop_id> const                      id_;
  std::vector<definition_pair_expression> vars_;
  std::size_t                             size_estimate_ = 0;

  void
  update_size_estimate();
};

expression
make_internal_reference(context& ctx, std::string const& name);

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

template <typename F>
void
traverse_postorder(expression e, F&& f) {
  struct visitor {
    F& f;

    explicit
    visitor(F& f) : f{f} { }

    void
    enter(expression e, dfs_stack<expression>& stack) {
      visit([&] (auto expr) { push_children(expr, stack); },
            e);
    }

    bool
    leave(expression expr, dfs_stack<expression>&) {
      visit(f, expr);
      return true;
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

  template <typename InnerVisitor, bool Copy>
  struct mapping_visitor {
    context&      ctx;
    InnerVisitor& inner_visitor;
    result_stack  results;

    mapping_visitor(context& ctx, InnerVisitor& inner_visitor)
      : ctx{ctx}
      , inner_visitor{inner_visitor}
    { }

    void
    enter(expression e, dfs_stack<expression>& stack) {
      visit(
        [&] (auto expr) {
          inner_visitor.enter(expr);
          push_children(expr, stack);
        },
        e
      );
    }

    bool
    leave(expression e, dfs_stack<expression>&) {
      auto result = visit(
        [&] (auto expr) -> expression {
          if constexpr (Copy)
            expr = duplicate(ctx, expr);
          expr->update(ctx, results);
          return inner_visitor.leave(expr);
        },
        e
      );
      results.push_back(result);
      return true;
    }
  };

  template <typename F>
  struct function_visitor_wrapper {
    F& f;

    explicit
    function_visitor_wrapper(F& f)
      : f{f}
    { }

    void
    enter(auto) { }

    expression
    leave(auto e) { return f(e); }
  };
}

template <typename Visitor>
expression
transform_ast(context& ctx, expression e, Visitor&& visitor) {
  detail::mapping_visitor<Visitor, false> v{ctx, visitor};
  depth_first_search(e, v);
  assert(v.results.size() == 1);
  return v.results.back();
}

template <typename Visitor>
expression
transform_ast_copy(context& ctx, expression e, Visitor&& visitor) {
  detail::mapping_visitor<Visitor, true> v{ctx, visitor};
  depth_first_search(e, v);
  assert(v.results.size() == 1);
  return v.results.back();
}

template <typename F>
expression
map_ast(context& ctx, expression e, F&& f) {
  return transform_ast(ctx, e, detail::function_visitor_wrapper<F>{f});
}

template <typename F>
expression
map_ast_copy(context& ctx, expression e, F&& f) {
  return transform_ast_copy(ctx, e, detail::function_visitor_wrapper<F>{f});
}

inline std::size_t
size_estimate(expression expr) {
  return visit([] (auto e) { return e->size_estimate(); }, expr);
}

inline std::string
show(context& ctx, expression expr, std::size_t indent) {
  return visit([&] (auto e) { return e->show(ctx, indent); }, expr);
}

} // namespace insider

#endif
