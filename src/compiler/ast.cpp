#include "compiler/ast.hpp"

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

void
literal_expression::update(context&, result_stack&) { }

local_reference_expression::local_reference_expression(
  ptr<local_variable> var
)
  : variable_{var}
{ }

void
local_reference_expression::visit_members(member_visitor const& f) {
  f(variable_);
}

void
local_reference_expression::update(context&, result_stack&) { }

top_level_reference_expression::top_level_reference_expression(
  ptr<top_level_variable> v
)
  : variable_{v}
{ }

void
top_level_reference_expression::visit_members(member_visitor const& f) {
  f(variable_);
}

void
top_level_reference_expression::update(context&, result_stack&) { }

unknown_reference_expression::unknown_reference_expression(ptr<syntax> name)
  : name_{name}
{ }

void
unknown_reference_expression::visit_members(member_visitor const& f) {
  f(name_);
}

void
unknown_reference_expression::update(context&, result_stack&) { }

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

static void
update_member(free_store& fs, auto owner, auto& ref, auto new_value) {
  if (ref != new_value) {
    ref = new_value;
    fs.notify_arc(owner, new_value);
  }
}

static void
update_member(free_store& fs, auto owner, expression& ref, expression new_value) {
  if (ref != new_value) {
    ref = new_value;
    fs.notify_arc(owner, new_value.get());
  }
}

static void
update_member(free_store& fs, auto owner, std::vector<expression>& v,
              std::vector<expression> new_values) {
  if (v != new_values) {
    v = std::move(new_values);
    for (expression member : v)
      fs.notify_arc(owner, member.get());
  }
}

void
application_expression::update(context& ctx, result_stack& stack) {
  expression target = pop(stack);
  auto args = pop_vector(stack, arguments_.size());

  update_member(ctx.store, this, target_, target);
  update_member(ctx.store, this, arguments_, std::move(args));

  update_size_estimate();
}

static std::size_t
sum_size_estimates(std::vector<expression> const& expressions) {
  std::size_t result = 0;
  for (expression e : expressions)
    result += size_estimate(e);
  return result;
}

void
application_expression::update_size_estimate() {
  size_estimate_
    = insider::size_estimate(target_) + sum_size_estimates(arguments_)
    + 1;
}

sequence_expression::sequence_expression(std::vector<expression> exprs)
  : expressions_{std::move(exprs)}
{
  update_size_estimate();
}

void
sequence_expression::visit_members(member_visitor const& f) {
  for (auto& e : expressions_)
    e.visit_members(f);
}

void
sequence_expression::update(context& ctx, result_stack& stack) {
  update_member(ctx.store, this, expressions_,
                pop_vector(stack, expressions_.size()));
  update_size_estimate();
}

void
sequence_expression::update_size_estimate() {
  size_estimate_ = sum_size_estimates(expressions_);
}

definition_pair_expression::definition_pair_expression(
  ptr<local_variable> var,
  insider::expression expr
)
  : variable_{var}
  , expression_{expr}
{ }

void
definition_pair_expression::visit_members(member_visitor const& f) {
  f(variable_);
  expression_.visit_members(f);
  assert(!is<stack_frame_extra_data>(expression_.get()));
}

let_expression::let_expression(std::vector<definition_pair_expression> defs,
                               ptr<sequence_expression> body)
  : definitions_{std::move(defs)}
  , body_{body}
{
  update_size_estimate();
}

void
let_expression::visit_members(member_visitor const& f) {
  for (definition_pair_expression& dp : definitions_)
    dp.visit_members(f);
  f(body_);
}

void
let_expression::update(context& ctx, result_stack& stack) {
  auto definition_exprs = pop_vector(stack, definitions_.size());
  auto body = pop<sequence_expression>(stack);

  update_member(ctx.store, this, body_, body);

  std::vector<definition_pair_expression> def_pairs;
  def_pairs.reserve(definition_exprs.size());
  for (std::size_t i = 0; i < definition_exprs.size(); ++i)
    def_pairs.emplace_back(definitions_[i].variable(), definition_exprs[i]);

  if (definitions_ != def_pairs) {
    definitions_ = std::move(def_pairs);
    for (definition_pair_expression& dp : definitions_) {
      ctx.store.notify_arc(this, dp.variable());
      ctx.store.notify_arc(this, dp.expression().get());
    }
  }

  update_size_estimate();
}

void
let_expression::update_size_estimate() {
  size_estimate_ = body_->size_estimate();
  for (definition_pair_expression const& dp : definitions_)
    size_estimate_ += insider::size_estimate(dp.expression());
}

local_set_expression::local_set_expression(ptr<local_variable> target,
                                           insider::expression expr)
  : target_{target}
  , expression_{expr}
{
  update_size_estimate();
}

void
local_set_expression::visit_members(member_visitor const& f) {
  f(target_);
  expression_.visit_members(f);
}

void
local_set_expression::update(context& ctx, result_stack& stack) {
  update_member(ctx.store, this, expression_, pop(stack));
  update_size_estimate();
}

void
local_set_expression::update_size_estimate() {
  size_estimate_ = 1 + insider::size_estimate(expression_);
}

top_level_set_expression::top_level_set_expression(ptr<top_level_variable> var,
                                                   insider::expression expr,
                                                   bool is_init)
  : variable_{var}
  , expression_{expr}
  , is_init_{is_init}
{
  update_size_estimate();
}

void
top_level_set_expression::visit_members(member_visitor const& f) {
  f(variable_);
  expression_.visit_members(f);
}

void
top_level_set_expression::update(context& ctx, result_stack& stack) {
  update_member(ctx.store, this, expression_, pop(stack));
  update_size_estimate();
}

void
top_level_set_expression::update_size_estimate() {
  size_estimate_ = 1 + insider::size_estimate(expression_);
}

lambda_expression::lambda_expression(
  std::vector<ptr<local_variable>> parameters,
  bool has_rest,
  ptr<sequence_expression> body,
  std::string name,
  std::vector<ptr<local_variable>> free_variables
)
  : parameters_{std::move(parameters)}
  , has_rest_{has_rest}
  , body_{body}
  , name_{std::move(name)}
  , free_variables_{std::move(free_variables)}
{ }

void
lambda_expression::add_free_variable(free_store& fs, ptr<local_variable> v) {
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

void
lambda_expression::update(context& ctx, result_stack& stack) {
  update_member(ctx.store, this, body_, pop<sequence_expression>(stack));
}

if_expression::if_expression(expression test, expression consequent,
                             expression alternative)
  : test_{test}
  , consequent_{consequent}
  , alternative_{alternative}
{
  update_size_estimate();
}

void
if_expression::visit_members(member_visitor const& f) {
  test_.visit_members(f);
  consequent_.visit_members(f);
  alternative_.visit_members(f);
}

void
if_expression::update(context& ctx, result_stack& stack) {
  auto test = pop(stack);
  auto consequent = pop(stack);
  auto alternative = pop(stack);
  update_member(ctx.store, this, test_, test);
  update_member(ctx.store, this, consequent_, consequent);
  update_member(ctx.store, this, alternative_, alternative);
  update_size_estimate();
}

void
if_expression::update_size_estimate() {
  size_estimate_
    = 1 + insider::size_estimate(test_) + insider::size_estimate(consequent_);

  if (alternative_)
    size_estimate_ += 1 + insider::size_estimate(alternative_);
}

expression
make_internal_reference(context& ctx, std::string const& name) {
  std::optional<module_::binding_type> binding
    = ctx.internal_module()->find(ctx.intern(name));
  assert(binding);
  assert(binding->variable);
  assert(is<top_level_variable>(binding->variable));

  return make<top_level_reference_expression>(
    ctx,
    assume<top_level_variable>(binding->variable)
  );
}

std::vector<expression>
untrack_expressions(std::vector<tracked_expression> const& v) {
  std::vector<expression> result;
  result.reserve(v.size());
  for (tracked_expression const& e : v)
    result.push_back(e.get());
  return result;
}

} // namespace insider
