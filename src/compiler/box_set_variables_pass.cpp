#include "compiler/box_set_variables_pass.hpp"

#include "compiler/ast.hpp"
#include "compiler/clone_ast.hpp"
#include "compiler/parsing_context.hpp"
#include "compiler/variable.hpp"
#include "context.hpp"

namespace insider {

static expression
box_variable_reference(context& ctx,
                       ptr<local_reference_expression> local_ref,
                       ptr<local_variable> var) {
  if (local_ref->variable() == var)
    return make_application(
      ctx, "unbox",
      make<local_reference_expression>(ctx, var)
    );
  else
    return local_ref;
}

static expression
box_variable_reference(context& ctx,
                       ptr<local_set_expression> local_set,
                       ptr<local_variable> var) {
  if (local_set->target() == var)
    return make_application(
      ctx, "box-set!",
      make<local_reference_expression>(ctx, local_set->target()),
      local_set->expression()
    );
  else
    return local_set;
}

static expression
box_variable_reference(context&, auto e, ptr<local_variable>) {
  return e;
}

static expression
box_variable_references(context& ctx, expression s, ptr<local_variable> var) {
  return map_ast(
    ctx, s,
    [&] (expression expr) {
      return visit(
        [&] (auto e) { return box_variable_reference(ctx, e, var); },
        expr
      );
    }
  );
}

static std::vector<ptr<local_variable>>
find_set_variables(ptr<let_expression> let) {
  auto rng = let->definitions()
    | std::views::transform([] (auto const& dp) { return dp.variable(); })
    | std::views::filter([] (ptr<local_variable> v) {
        return v->flags().is_set && !v->flags().is_set_eliminable;
      });
  return std::vector<ptr<local_variable>>(rng.begin(), rng.end());
}

static definition_pair_expression
box_definition_pair(context& ctx, definition_pair_expression const& dp) {
  dp.variable()->flags().is_set = false;
  return {dp.variable(), make_application(ctx, "box", dp.expression())};
}

static std::vector<definition_pair_expression>
box_definition_pairs(context& ctx,
                     std::vector<definition_pair_expression> const& dps) {
  auto rng = dps | std::views::transform(
    [&] (definition_pair_expression const& dp) {
      if (dp.variable()->flags().is_set)
        return box_definition_pair(ctx, dp);
      else
        return dp;
    }
  );
  return std::vector(rng.begin(), rng.end());
}

static expression
box_set_variables(context& ctx, ptr<let_expression> let) {
  std::vector<ptr<local_variable>> set_vars = find_set_variables(let);
  if (!set_vars.empty()) {
    std::vector<definition_pair_expression> dps
      = box_definition_pairs(ctx, let->definitions());
    expression body = let->body();
    for (ptr<local_variable> v : set_vars)
      body = box_variable_references(ctx, body, v);
    return make<let_expression>(ctx, std::move(dps), body);
  } else
    return let;
}

static bool
any_param_set(ptr<lambda_expression> lambda) {
  return std::ranges::any_of(
    lambda->parameters(),
    [] (lambda_expression::parameter const& p) {
      return p.variable->flags().is_set;
    }
  );
}

static expression
box_lambda(context& ctx, ptr<lambda_expression> lambda) {
  std::vector<expression> new_body;
  expression body = lambda->body();
  for (auto const& param : lambda->parameters())
    if (param.variable->flags().is_set) {
      body = box_variable_references(ctx, body, param.variable);
      new_body.emplace_back(
        make<local_set_expression>(
          ctx, param.variable,
          make_application(
            ctx, "box",
            make<local_reference_expression>(ctx, param.variable)
          )
        )
      );
    }

  new_body.emplace_back(lambda->body());
  return make<lambda_expression>(
    ctx, lambda, make<sequence_expression>(ctx, std::move(new_body))
  );
}

static expression
box_set_variables(context& ctx, ptr<lambda_expression> lambda) {
  if (any_param_set(lambda))
    return box_lambda(ctx, clone_ast(ctx, lambda));
  else
    return lambda;
}

static definition_pair_expression
box_loop_variable(context& ctx, definition_pair_expression const& var) {
  return definition_pair_expression{
    var.variable(),
    make<application_expression>(ctx,
                                 ptr<syntax>{},
                                 make_internal_reference(ctx, "box"),
                                 var.expression())
  };
}

static std::vector<definition_pair_expression>
box_loop_variables(context& ctx, ptr<loop_continue> cont) {
  std::vector<definition_pair_expression> new_vars;
  for (auto const& var : cont->variables())
    if (var.variable()->flags().is_set)
      new_vars.emplace_back(box_loop_variable(ctx, var));
    else
      new_vars.emplace_back(var);
  return new_vars;
}

static expression
box_set_variables(context& ctx, ptr<loop_continue> cont) {
  auto new_vars = box_loop_variables(ctx, cont);
  if (new_vars != cont->variables())
    return make<loop_continue>(ctx, cont->id(), std::move(new_vars));
  else
    return cont;
}

static expression
box_set_variables(context&, auto e) {
  return e;
}

expression
box_set_variables(parsing_context& pc, expression s) {
  return map_ast(
    pc.ctx, s,
    [&] (expression expr) {
      return visit([&] (auto e) { return box_set_variables(pc.ctx, e); },
                   expr);
    }
  );
}

} // namespace insider
