#include "compiler/expression.hpp"

#include "context.hpp"
#include "module.hpp"

namespace insider {

literal_expression::literal_expression(ptr<> value)
  : value_{value}
{ }

void
literal_expression::visit_members(member_visitor const& f) {
  f(value_);
}

local_reference_expression::local_reference_expression(ptr<insider::variable> var)
  : variable_{var}
{ }

void
local_reference_expression::visit_members(member_visitor const& f) {
  f(variable_);
}

top_level_reference_expression::top_level_reference_expression(operand location,
                                                               std::string name)
  : location{location}
  , name{std::move(name)}
{ }

unknown_reference_expression::unknown_reference_expression(ptr<syntax> name)
  : name_{name}
{ }

void
unknown_reference_expression::visit_members(member_visitor const& f) {
  f(name_);
}

application_expression::application_expression(expression t,
                                               std::vector<expression> args)
  : target_{t}
  , arguments_{std::move(args)}
{ }

void
application_expression::visit_members(member_visitor const& f) {
  target_.visit_members(f);
  for (auto& arg : arguments_)
    arg.visit_members(f);
}

sequence_expression::sequence_expression(std::vector<expression> exprs)
  : expressions_{std::move(exprs)}
{ }

void
sequence_expression::prepend_expression(free_store& fs, expression expr) {
  expressions_.insert(expressions_.begin(), expr);
  fs.notify_arc(this, expr.get());
}

void
sequence_expression::visit_members(member_visitor const& f) {
  for (auto& e : expressions_)
    e.visit_members(f);
}

definition_pair_expression::definition_pair_expression(
  ptr<syntax> id,
  ptr<insider::variable> var,
  insider::expression expr
)
  : id_{id}
  , variable_{var}
  , expression_{expr}
{ }

void
definition_pair_expression::visit_members(member_visitor const& f) {
  f(id_);
  f(variable_);
  expression_.visit_members(f);
}

let_expression::let_expression(std::vector<definition_pair_expression> defs,
                               ptr<sequence_expression> body)
  : definitions_{std::move(defs)}
  , body_{body}
{ }

void
let_expression::visit_members(member_visitor const& f) {
  for (definition_pair_expression& dp : definitions_)
    dp.visit_members(f);
  f(body_);
}

local_set_expression::local_set_expression(ptr<variable> target,
                                           insider::expression expr)
  : target_{target}
  , expression_{expr}
{ }

void
local_set_expression::visit_members(member_visitor const& f) {
  f(target_);
  expression_.visit_members(f);
}

top_level_set_expression::top_level_set_expression(operand location,
                                                   insider::expression expr)
  : location_{location}
  , expression_{expr}
{ }

void
top_level_set_expression::visit_members(member_visitor const& f) {
  expression_.visit_members(f);
}

lambda_expression::lambda_expression(std::vector<ptr<variable>> parameters,
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

void
lambda_expression::add_free_variable(free_store& fs, ptr<variable> v) {
  free_variables_.push_back(v);
  fs.notify_arc(this, v);
}

void
lambda_expression::visit_members(member_visitor const& f) {
  for (auto& p : parameters_)
    f(p);
  f(body_);
  for (auto& fv : free_variables_)
    f(fv);
}

if_expression::if_expression(expression test, expression consequent,
                             expression alternative)
  : test_{test}
  , consequent_{consequent}
  , alternative_{alternative}
{ }

void
if_expression::visit_members(member_visitor const& f) {
  test_.visit_members(f);
  consequent_.visit_members(f);
  alternative_.visit_members(f);
}

expression
make_internal_reference(context& ctx, std::string name) {
  std::optional<module_::binding_type> binding
    = ctx.internal_module()->find(ctx.intern(name));
  assert(binding);
  assert(binding->variable);
  assert(binding->variable->global);

  return make<top_level_reference_expression>(
    ctx, *binding->variable->global, std::move(name)
  );
}

void
expression_visitor::enter(expression e, dfs_stack<expression>& stack) {
  visit([&] (auto expr) {
          enter_expression(expr);
          push_children(expr, stack);
        },
        e);
}

void
expression_visitor::leave(expression e) {
  visit([&] (auto expr) { leave_expression(expr); }, e);
}
} // namespace insider
