#include "compiler/expression.hpp"

#include "context.hpp"
#include "module.hpp"

namespace insider {

static expression
pop(result_stack& stack) {
  expression result = stack.back();
  stack.pop_back();
  return result;
}

template <typename T>
static ptr<T>
pop(result_stack& stack) {
  return assume<T>(pop(stack));
}

static std::vector<expression>
pop_vector(result_stack& stack, std::size_t n) {
  std::vector<expression> result;
  result.reserve(n);
  for (std::size_t i = 0; i < n; ++i)
    result.push_back(pop(stack));
  return result;
}

literal_expression::literal_expression(ptr<> value)
  : value_{value}
{ }

void
literal_expression::visit_members(member_visitor const& f) {
  f(value_);
}

expression
literal_expression::duplicate(context& ctx, result_stack&) {
  return make<literal_expression>(ctx, value_);
}

local_reference_expression::local_reference_expression(
  ptr<insider::variable> var
)
  : variable_{var}
{ }

void
local_reference_expression::visit_members(member_visitor const& f) {
  f(variable_);
}

expression
local_reference_expression::duplicate(context& ctx, result_stack&) {
  return make<local_reference_expression>(ctx, variable_);
}

top_level_reference_expression::top_level_reference_expression(
  ptr<insider::variable> v
)
  : variable_{v}
{ }

void
top_level_reference_expression::visit_members(member_visitor const& f) {
  f(variable_);
}

expression
top_level_reference_expression::duplicate(context& ctx, result_stack&) {
  return make<top_level_reference_expression>(ctx, variable_);
}

unknown_reference_expression::unknown_reference_expression(ptr<syntax> name)
  : name_{name}
{ }

void
unknown_reference_expression::visit_members(member_visitor const& f) {
  f(name_);
}

expression
unknown_reference_expression::duplicate(context& ctx, result_stack&) {
  return make<unknown_reference_expression>(ctx, name_);
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

expression
application_expression::duplicate(context& ctx, result_stack& stack) {
  expression target = pop(stack);
  auto args = pop_vector(stack, arguments_.size());
  return make<application_expression>(ctx, target, std::move(args));
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

expression
sequence_expression::duplicate(context& ctx, result_stack& stack) {
  return make<sequence_expression>(ctx, pop_vector(stack, expressions_.size()));
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
  assert(!is<stack_frame_extra_data>(expression_.get()));
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

expression
let_expression::duplicate(context& ctx, result_stack& stack) {
  auto definition_exprs = pop_vector(stack, definitions_.size());
  auto body = pop<sequence_expression>(stack);

  std::vector<definition_pair_expression> def_pairs;
  def_pairs.reserve(definition_exprs.size());
  for (std::size_t i = 0; i < definition_exprs.size(); ++i)
    def_pairs.emplace_back(definitions_[i].id(), definitions_[i].variable(),
                           definition_exprs[i]);

  return make<let_expression>(ctx, std::move(def_pairs), body);
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

expression
local_set_expression::duplicate(context& ctx, result_stack& stack) {
  return make<local_set_expression>(ctx, target_, pop(stack));
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

expression
top_level_set_expression::duplicate(context& ctx, result_stack& stack) {
  return make<top_level_set_expression>(ctx, location_, pop(stack));
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

expression
lambda_expression::duplicate(context& ctx, result_stack& stack) {
  return make<lambda_expression>(
    ctx,
    parameters_,
    has_rest_,
    pop<sequence_expression>(stack),
    name_,
    free_variables_
  );
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
if_expression::duplicate(context& ctx, result_stack& stack) {
  auto test = pop(stack);
  auto consequent = pop(stack);
  auto alternative = pop(stack);
  return make<if_expression>(ctx, test, consequent, alternative);
}

expression
make_internal_reference(context& ctx, std::string const& name) {
  std::optional<module_::binding_type> binding
    = ctx.internal_module()->find(ctx.intern(name));
  assert(binding);
  assert(binding->variable);
  assert(binding->variable->global);

  return make<top_level_reference_expression>(ctx, binding->variable);
}

std::vector<expression>
untrack_expressions(std::vector<tracked_expression> const& v) {
  std::vector<expression> result;
  result.reserve(v.size());
  for (tracked_expression const& e : v)
    result.push_back(e.get());
  return result;
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
