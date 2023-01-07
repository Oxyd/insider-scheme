#include "compiler/ast.hpp"

#include "context.hpp"
#include "io/write.hpp"
#include "module.hpp"
#include "util/define_struct.hpp"
#include "util/sum_type.hpp"

namespace insider {

static expression
pop(result_stack& stack) {
  assert(!stack.empty());
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

static std::vector<expression>
pop_vector_reverse(result_stack& stack, std::size_t n) {
  std::vector<expression> result;
  result.resize(n);
  for (std::size_t i = 0; i < n; ++i)
    result[result.size() - i - 1] = pop(stack);
  return result;
}

static std::string
make_indent(std::size_t indent) {
  return std::string(indent, ' ');
}

literal_expression::literal_expression(ptr<> value)
  : value_{value}
{ }

void
literal_expression::visit_members(member_visitor const& f) const {
  f(value_);
}

void
literal_expression::update(context&, result_stack&) { }

std::string
literal_expression::show(context& ctx, std::size_t indent) const {
  return fmt::format("{}- literal-expression: {}\n",
                     make_indent(indent), datum_to_string(ctx, value_));
}

local_reference_expression::local_reference_expression(
  ptr<local_variable> var
)
  : variable_{var}
{ }

void
local_reference_expression::visit_members(member_visitor const& f) const {
  f(variable_);
}

void
local_reference_expression::update(context&, result_stack&) { }

std::string
local_reference_expression::show(context&, std::size_t indent) const {
  return fmt::format("{}- local-reference: {}@{}\n",
                     make_indent(indent),
                     variable_->name(), static_cast<void*>(variable_.value()));
}

top_level_reference_expression::top_level_reference_expression(
  ptr<top_level_variable> v
)
  : variable_{v}
{ }

void
top_level_reference_expression::visit_members(member_visitor const& f) const {
  f(variable_);
}

void
top_level_reference_expression::update(context&, result_stack&) { }

std::string
top_level_reference_expression::show(context&, std::size_t indent) const {
  return fmt::format("{}- top-level-reference: {}@{}\n",
                     make_indent(indent),
                     variable_->name(), static_cast<void*>(variable_.value()));
}

unknown_reference_expression::unknown_reference_expression(ptr<syntax> name)
  : name_{name}
{ }

void
unknown_reference_expression::visit_members(member_visitor const& f) const {
  f(name_);
}

void
unknown_reference_expression::update(context&, result_stack&) { }

std::string
unknown_reference_expression::show(context& ctx, std::size_t indent) const {
  return fmt::format("{}- unknown-reference: {}\n",
                     make_indent(indent), datum_to_string(ctx, name_));
}

application_expression::application_expression(expression t,
                                               std::vector<expression> args)
  : target_{t}
  , arguments_{std::move(args)}
{
  argument_names_.resize(arguments_.size());
}

application_expression::application_expression(
  expression t,
  std::vector<expression> args,
  std::vector<ptr<keyword>> arg_names
)
  : target_{t}
  , arguments_{std::move(args)}
  , argument_names_{std::move(arg_names)}
{
  assert(arguments_.size() == argument_names_.size());
}

void
application_expression::visit_members(member_visitor const& f) const {
  target_.visit_members(f);
  for (auto const& arg : arguments_)
    arg.visit_members(f);
  for (auto const& kw : argument_names_)
    f(kw);
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

std::string
application_expression::show(context& ctx, std::size_t indent) const {
  std::string i = make_indent(indent);
  std::string t = insider::show(ctx, target_, indent + 4);
  std::string args;
  for (expression e : arguments_)
    args += insider::show(ctx, e, indent + 4);

  return fmt::format("{}- application:\n"
                     "{}  target:\n"
                     "{}"
                     "{}  arguments:\n"
                     "{}",
                     i, i, t, i, args);
}

bool
has_keyword_arguments(ptr<application_expression> app) {
  return std::ranges::any_of(app->argument_names(),
                             [] (ptr<keyword> kw) { return kw != nullptr; });
}

built_in_operation_expression::built_in_operation_expression(
  opcode op, std::vector<expression> operands, bool has_result,
  ptr<native_procedure> proc
)
  : operation_{op}
  , operands_{std::move(operands)}
  , has_result_{has_result}
  , proc_{proc}
{
  update_size_estimate();
}

void
built_in_operation_expression::visit_members(member_visitor const& f) const {
  for (expression const& operand : operands_)
    operand.visit_members(f);
  f(proc_);
}

void
built_in_operation_expression::update(context& ctx, result_stack& stack) {
  auto operands = pop_vector(stack, operands_.size());
  update_member(ctx.store, this, operands_, std::move(operands));
  update_size_estimate();
}

void
built_in_operation_expression::update_size_estimate() {
  size_estimate_ = 1 + sum_size_estimates(operands_);
}

std::string
built_in_operation_expression::show(context& ctx, std::size_t indent) const {
  std::string i = make_indent(indent);
  std::string t = make_indent(indent + 4) + opcode_to_info(operation_).mnemonic;
  std::string operands;
  for (expression e : operands_)
    operands += insider::show(ctx, e, indent + 4);

  return fmt::format("{}- built-in-operation {}:\n"
                     "{}  operands:\n"
                     "{}\n",
                     i, opcode_to_info(operation_).mnemonic, i, operands);
}

sequence_expression::sequence_expression(std::vector<expression> exprs)
  : expressions_{std::move(exprs)}
{
  update_size_estimate();
}

void
sequence_expression::visit_members(member_visitor const& f) const {
  for (auto& e : expressions_)
    e.visit_members(f);
}

void
sequence_expression::update(context& ctx, result_stack& stack) {
  update_member(ctx.store, this, expressions_,
                pop_vector_reverse(stack, expressions_.size()));
  update_size_estimate();
}

void
sequence_expression::update_size_estimate() {
  size_estimate_ = sum_size_estimates(expressions_);
}

std::string
sequence_expression::show(context& ctx, std::size_t indent) const {
  std::string subexprs;
  for (expression e : expressions_)
    subexprs += insider::show(ctx, e, indent + 2);
  return fmt::format("{}- sequence:\n{}", make_indent(indent), subexprs);
}

definition_pair_expression::definition_pair_expression(
  ptr<local_variable> var,
  insider::expression expr
)
  : variable_{var}
  , expression_{expr}
{ }

void
definition_pair_expression::visit_members(member_visitor const& f) const {
  f(variable_);
  expression_.visit_members(f);
}

let_expression::let_expression(std::vector<definition_pair_expression> defs,
                               expression body)
  : definitions_{std::move(defs)}
  , body_{body}
{
  update_size_estimate();
}

void
let_expression::visit_members(member_visitor const& f) const {
  for (definition_pair_expression const& dp : definitions_)
    dp.visit_members(f);
  body_.visit_members(f);
}

void
let_expression::update(context& ctx, result_stack& stack) {
  auto body = pop(stack);
  auto definition_exprs = pop_vector_reverse(stack, definitions_.size());

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
  size_estimate_ = insider::size_estimate(body_);
  for (definition_pair_expression const& dp : definitions_)
    size_estimate_ += insider::size_estimate(dp.expression());
}

std::string
let_expression::show(context& ctx, std::size_t indent) const {
  std::string defs;
  for (definition_pair_expression const& dp : definitions_)
    defs += fmt::format("{}- {}@{}\n{}",
                        make_indent(indent + 4),
                        dp.variable()->name(),
                        static_cast<void*>(dp.variable().value()),
                        insider::show(ctx, dp.expression(), indent + 6));
  std::string i = make_indent(indent);
  return fmt::format("{}- let:\n"
                     "{}  variables:\n"
                     "{}"
                     "{}  body:\n"
                     "{}",
                     i, i, defs, i, insider::show(ctx, body_, indent + 4));
}

local_set_expression::local_set_expression(ptr<local_variable> target,
                                           insider::expression expr)
  : target_{target}
  , expression_{expr}
{
  update_size_estimate();
}

void
local_set_expression::visit_members(member_visitor const& f) const {
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

std::string
local_set_expression::show(context& ctx, std::size_t indent) const {
  return fmt::format("{}- local-set: {}@{}\n{}",
                     make_indent(indent),
                     target_->name(),
                     static_cast<void*>(target_.value()),
                     insider::show(ctx, expression_, indent + 2));
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
top_level_set_expression::visit_members(member_visitor const& f) const {
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

std::string
top_level_set_expression::show(context& ctx, std::size_t indent) const {
  return fmt::format("{}- top-level-set: {}@{}\n{}",
                     make_indent(indent),
                     variable_->name(),
                     static_cast<void*>(variable_.value()),
                     insider::show(ctx, expression_, indent + 2));
}

void
lambda_expression::parameter::visit_members(member_visitor const& f) const {
  f(variable);
}

lambda_expression::lambda_expression(ptr<lambda_expression> source,
                                     ptr<local_variable> new_self_variable)
  : parameters_{source->parameters_}
  , has_rest_{source->has_rest_}
  , body_{source->body_}
  , name_{source->name_}
  , self_variable_{new_self_variable}
  , num_self_references_{source->num_self_references_}
{ }

lambda_expression::lambda_expression(ptr<lambda_expression> source,
                                     expression new_body)
  : parameters_{source->parameters_}
  , has_rest_{source->has_rest_}
  , body_{new_body}
  , name_{source->name_}
  , free_variables_{source->free_variables_}
  , self_variable_{source->self_variable_}
  , num_self_references_{source->num_self_references_}
{ }

lambda_expression::lambda_expression(
  context& ctx,
  std::vector<parameter> parameters,
  std::vector<ptr<keyword>> parameter_names,
  bool has_rest,
  expression body,
  std::string name,
  std::vector<ptr<local_variable>> free_variables
)
  : parameters_{std::move(parameters)}
  , parameter_names_{std::move(parameter_names)}
  , has_rest_{has_rest}
  , body_{body}
  , name_{std::move(name)}
  , free_variables_{std::move(free_variables)}
  , self_variable_{
      make<local_variable>(ctx, fmt::format("<self variable for {}>", name_))
    }
{
  self_variable_->flags().is_self_variable = true;
  self_variable_->set_constant_initialiser(ctx.store,
                                           ptr<lambda_expression>(this));
}

void
lambda_expression::update_body(free_store& fs, expression new_body) {
  body_ = new_body;
  fs.notify_arc(this, body_.get());
}

void
lambda_expression::add_free_variable(free_store& fs, ptr<local_variable> v) {
  free_variables_.push_back(v);
  fs.notify_arc(this, v);
}

void
lambda_expression::visit_members(member_visitor const& f) const {
  for (auto const& p : parameters_)
    p.visit_members(f);
  for (auto const& kw : parameter_names_)
    f(kw);
  body_.visit_members(f);
  for (auto const& fv : free_variables_)
    f(fv);
  f(self_variable_);
}

void
lambda_expression::update(context& ctx, result_stack& stack) {
  update_member(ctx.store, this, body_, pop(stack));
}

std::string
lambda_expression::show(context& ctx, std::size_t indent) const {
  std::string params;
  for (auto const& p : parameters_)
    params += fmt::format("{}@{} ",
                          p.variable->name(),
                          static_cast<void*>(p.variable.value()));

  return fmt::format("{}- lambda: {} {}\n{}",
                     make_indent(indent), name_, params,
                     insider::show(ctx, body_, indent + 2));
}

std::size_t
required_parameter_count(ptr<lambda_expression> lambda) {
  return leading_parameter_count(lambda) - optional_leading_parameter_count(lambda);
}

std::size_t
optional_leading_parameter_count(ptr<lambda_expression> lambda) {
  return std::ranges::count_if(
    lambda->parameters(),
    [] (lambda_expression::parameter const& p) { return p.optional; }
  );
}

std::size_t
leading_parameter_count(ptr<lambda_expression> lambda) {
  if (lambda->has_rest())
    return lambda->parameters().size() - 1;
  else
    return lambda->parameters().size();
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
if_expression::visit_members(member_visitor const& f) const {
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
    = 1 + insider::size_estimate(test_) + insider::size_estimate(consequent_)
    + 1 + insider::size_estimate(alternative_);
}

std::string
if_expression::show(context& ctx, std::size_t indent) const {
  std::string i = make_indent(indent);
  return fmt::format("{}- if\n"
                     "{}  test:\n"
                     "{}"
                     "{}  consequent:\n"
                     "{}"
                     "{}  alternative:\n"
                     "{}",
                     i,
                     i,
                     insider::show(ctx, test_, indent + 4),
                     i,
                     insider::show(ctx, consequent_, indent + 4),
                     i,
                     insider::show(ctx, alternative_, indent + 4));
}

loop_body::loop_body(expression body, ptr<loop_id> id,
                     std::vector<ptr<local_variable>> loop_vars)
  : body_{body}
  , id_{id}
  , vars_{std::move(loop_vars)}
{ }

void
loop_body::visit_members(member_visitor const& f) const {
  body_.visit_members(f);
  f(id_);
  for (ptr<local_variable> const& var : vars_)
    f(var);
}

void
loop_body::update(context& ctx, result_stack& stack) {
  update_member(ctx.store, this, body_, pop(stack));
}

std::size_t
loop_body::size_estimate() const {
  return insider::size_estimate(body_);
}

std::string
loop_body::show(context& ctx, std::size_t indent) const {
  return fmt::format("{}- loop-body: {}\n{}",
                     make_indent(indent),
                     static_cast<void*>(id_.value()),
                     insider::show(ctx, body_, indent + 2));
}

loop_continue::loop_continue(ptr<loop_id> id,
                             std::vector<definition_pair_expression> vars)
  : id_{id}
  , vars_{std::move(vars)}
{
  update_size_estimate();
}

void
loop_continue::visit_members(member_visitor const& f) const {
  f(id_);
  for (definition_pair_expression const& var : vars_)
    var.visit_members(f);
}

void
loop_continue::update(context& ctx, result_stack& stack) {
  auto var_exprs = pop_vector_reverse(stack, vars_.size());

  std::vector<definition_pair_expression> new_vars;
  new_vars.reserve(vars_.size());
  for (std::size_t i = 0; i < vars_.size(); ++i)
    new_vars.emplace_back(vars_[i].variable(), var_exprs[i]);

  if (vars_ != new_vars) {
    vars_ = std::move(new_vars);
    for (definition_pair_expression& var : vars_) {
      ctx.store.notify_arc(this, var.variable());
      ctx.store.notify_arc(this, var.expression().get());
    }
  }

  update_size_estimate();
}

void
loop_continue::update_size_estimate() {
  size_estimate_ = 1;
  for (definition_pair_expression const& var : vars_)
    size_estimate_ += insider::size_estimate(var.expression());
}

std::string
loop_continue::show(context& ctx, std::size_t indent) const {
  std::string vars;
  for (definition_pair_expression const& dp : vars_)
    vars += fmt::format("{}- {}@{}\n{}",
                        make_indent(indent + 4),
                        dp.variable()->name(),
                        static_cast<void*>(dp.variable().value()),
                        insider::show(ctx, dp.expression(), indent + 6));

  std::string i = make_indent(indent);
  return fmt::format("{}- loop-continue: {}\n"
                     "{}  variables:\n"
                     "{}",
                     i,
                     static_cast<void*>(id_.value()),
                     i,
                     vars);
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

static ptr<>
application_expression_arguments(context& ctx, ptr<application_expression> app) {
  return make_list_from_range(ctx, app->arguments());
}

static ptr<>
sequence_expression_expressions(context& ctx, ptr<sequence_expression> seq) {
  return make_list_from_range(ctx, seq->expressions());
}

static ptr<>
let_expression_definitions(context& ctx, ptr<let_expression> let) {
  return make_list_from_range(
    ctx, let->definitions(),
    [&] (definition_pair_expression const& dp) {
      return cons(ctx, dp.variable(), dp.expression().get());
    }
  );
}

static ptr<>
lambda_expression_parameters(context& ctx, ptr<lambda_expression> lambda) {
  return make_list_from_range(ctx, lambda->parameters(),
                              [] (lambda_expression::parameter const& param) {
                                return param.variable;
                              });
}

static ptr<>
lambda_expression_free_variables(context& ctx, ptr<lambda_expression> lambda) {
  return make_list_from_range(ctx, lambda->free_variables());
}

void
export_ast(context& ctx, ptr<module_> result) {
  define_struct<literal_expression>(ctx, "literal-expression", result)
    .field<&literal_expression::value>("value");

  define_struct<local_reference_expression>(ctx, "local-reference-expression",
                                            result)
    .field<&local_reference_expression::variable>("variable");

  define_struct<top_level_reference_expression>(ctx,
                                                "top-level-reference-expression",
                                                result)
    .field<&top_level_reference_expression::variable>("variable");

  define_struct<unknown_reference_expression>(ctx,
                                              "unknown-reference-expression",
                                              result)
    .field<&unknown_reference_expression::name>("name");

  define_struct<application_expression>(ctx, "application-expression", result)
    .field<&application_expression::target>("target")
    .field<&application_expression_arguments>("arguments")
    ;

  define_struct<sequence_expression>(ctx, "sequence-expression", result)
    .field<&sequence_expression_expressions>("expressions");

  define_struct<let_expression>(ctx, "let-expression", result)
    .field<&let_expression_definitions>("definitions")
    .field<&let_expression::body>("body")
    ;

  define_struct<local_set_expression>(ctx, "local-set!-expression", result)
    .field<&local_set_expression::target>("target")
    .field<&local_set_expression::expression>("expression")
    ;

  define_struct<top_level_set_expression>(ctx, "top-level-set!-expression",
                                          result)
    .field<&top_level_set_expression::target>("target")
    .field<&top_level_set_expression::expression>("expression")
    ;

  define_struct<lambda_expression>(ctx, "lambda-expression", result)
    .field<&lambda_expression_parameters>("parameters")
    .field<&lambda_expression::has_rest>("has-rest?")
    .field<&lambda_expression::body>("body")
    .field<&lambda_expression::name>("name")
    .field<&lambda_expression_free_variables>("free-variables")
    ;

  define_struct<if_expression>(ctx, "if-expression", result)
    .field<&if_expression::test>("test")
    .field<&if_expression::consequent>("consequent")
    .field<&if_expression::alternative>("alternative")
    ;
}

} // namespace insider
