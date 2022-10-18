#include "compiler/evaluate_constants_pass.hpp"

#include "compiler/ast.hpp"
#include "context.hpp"
#include "runtime/basic_types.hpp"
#include "vm/vm.hpp"

namespace insider {

static bool
is_initialised_to_constant(auto var) {
  return var->constant_initialiser()
         && is<literal_expression>(var->constant_initialiser());
}

static expression
propagate_and_evaluate_constants(context&,
                                 ptr<local_reference_expression> ref,
                                 bool&) {
  if (is_initialised_to_constant(ref->variable()))
    return ref->variable()->constant_initialiser();
  else
    return ref;
}

static expression
propagate_and_evaluate_constants(context&,
                                 ptr<top_level_reference_expression> ref,
                                 bool&) {
  if (is_initialised_to_constant(ref->variable()))
    return ref->variable()->constant_initialiser();
  else
    return ref;
}

static bool
is_const(definition_pair_expression const& dp) {
  return is_initialised_to_constant(dp.variable());
}

static ptr<>
constant_value_for_expression(expression e);

static ptr<>
constant_value_for_expression(ptr<literal_expression> lit) {
  return lit->value();
}

static ptr<>
constant_value_for_expression(ptr<local_reference_expression> ref) {
  if (expression k = ref->variable()->constant_initialiser())
    if (auto lit = match<literal_expression>(k))
      return lit->value();
  return {};
}

static ptr<>
constant_value_for_expression(ptr<top_level_reference_expression> ref) {
  if (expression k = ref->variable()->constant_initialiser())
    if (auto lit = match<literal_expression>(k))
      return lit->value();
  return {};
}

static ptr<>
constant_value_for_expression(ptr<sequence_expression> seq) {
  // Technically the correct condition is "all subexpressions are constant",
  // but it's quite unlikely that a sequence with multiple subexpressions will
  // be all-constant.

  if (seq->expressions().size() == 1)
    return constant_value_for_expression(seq->expressions().front());
  else
    return {};
}

static ptr<>
constant_value_for_expression(auto) { return {}; }

static ptr<>
constant_value_for_expression(expression e) {
  return visit([] (auto expr) { return constant_value_for_expression(expr); },
               e);
}

static expression
constant_initialiser_expression(expression e) {
  if (auto lit = match<literal_expression>(e))
    return lit;
  else if (auto seq = match<sequence_expression>(e)) {
    assert(seq->expressions().size() == 1);
    return constant_initialiser_expression(seq->expressions().front());
  } else {
    assert(false);
    return {};
  }
}

static std::vector<definition_pair_expression>
update_let_definitions(context& ctx,
                       std::vector<definition_pair_expression> const& dps,
                       bool& go_again) {
  std::vector<definition_pair_expression> result;
  for (definition_pair_expression dp : dps) {
    // set!-eliminable variable definitions need to be retained because they're
    // still going to be set!.
    if (!is_const(dp)
        || dp.variable()->flags().is_set_eliminable
        || dp.variable()->flags().is_loop_variable)
      result.push_back(dp);

    if (!is_const(dp) && constant_value_for_expression(dp.expression())
        && !dp.variable()->flags().is_set
        && !dp.variable()->flags().is_loop_variable) {
      // The variable was made constant in this pass. We need to mark it as
      // constant and do another pass to propagate it into the body of this let.

      dp.variable()->set_constant_initialiser(
        ctx.store, constant_initialiser_expression(dp.expression())
      );
      go_again = true;
    }
  }

  return result;
}

static expression
propagate_and_evaluate_constants(context& ctx, ptr<let_expression> let,
                                 bool& go_again) {
  auto new_dps = update_let_definitions(ctx, let->definitions(), go_again);
  if (new_dps.empty())
    return let->body();
  else if (new_dps.size() < let->definitions().size())
    return make<let_expression>(ctx, std::move(new_dps), let->body());
  else
    return let;
}

static ptr<>
find_constant_evaluable_callable(context& ctx,
                                 ptr<application_expression> app) {
  if (auto ref = match<top_level_reference_expression>(app->target())) {
    ptr<> callable = ctx.get_top_level(ref->variable()->index);
    if (auto np = match<native_procedure>(callable))
      if (np->constant_evaluable)
        return np;
  }

  return {};
}

static bool
can_be_constant_evaluated(ptr<application_expression> app) {
  return std::ranges::all_of(
    app->arguments(),
    [] (expression e) {
      return visit(
        [] (auto expr) {
          return constant_value_for_expression(expr) != nullptr;
        },
        e
      );
    }
  );
}

static void
coalesce_eqv_values(context& ctx, std::vector<ptr<>>& values) {
  // Values which are eqv? are meant to be coalesced within the whole program.
  // This coalescing normally happens due to the compiler calling
  // context::intern_static, which hasn't happened yet.
  //
  // Technically, this could produce invalid results when a value is meant to
  // be eqv? to another value not in this call, but for that to matter the
  // called procedure would somehow have to have access to objects outside
  // its arguments, which shouldn't happen for constant-evaluable procedures.

  for (std::size_t i = 0; i < values.size(); ++i)
    for (std::size_t j = i + 1; j < values.size(); ++j)
      if (values[i] != values[j] && eqv(ctx, values[i], values[j]))
        values[j] = values[i];
}

static std::vector<ptr<>>
make_arguments_for_constant_evaluation(context& ctx,
                                       ptr<application_expression> app) {
  std::vector<ptr<>> result;
  result.reserve(app->arguments().size());
  for (expression arg : app->arguments())
    result.push_back(constant_value_for_expression(arg));

  coalesce_eqv_values(ctx, result);
  return result;
}

static expression
evaluate_constant_application(context& ctx, ptr<> callable,
                              ptr<application_expression> app) {
  try {
    ptr<> result = call_with_continuation_barrier(
      ctx, callable, make_arguments_for_constant_evaluation(ctx, app)
    );
    return make<literal_expression>(ctx, result);
  } catch (...) {
    return {};
  }
}

static expression
propagate_and_evaluate_constants(context& ctx,
                                 ptr<application_expression> app,
                                 bool&) {
  if (can_be_constant_evaluated(app))
    if (auto callable = find_constant_evaluable_callable(ctx, app))
      if (auto result = evaluate_constant_application(ctx, callable, app))
        return result;
  return app;
}

static expression
propagate_and_evaluate_constants(context& ctx, ptr<if_expression> ifexpr,
                                 bool&) {
  if (auto test_value = constant_value_for_expression(ifexpr->test())) {
    if (test_value == ctx.constants->f)
      return ifexpr->alternative();
    else
      return ifexpr->consequent();
  } else
    return ifexpr;
}

static expression
propagate_and_evaluate_constants(context&, auto x, bool&) { return x; }

expression
propagate_and_evaluate_constants(context& ctx, expression e, analysis_context) {
  bool go_again = false;
  do {
    go_again = false;
    e = map_ast(
      ctx, e,
      [&] (expression expr) {
        return visit(
          [&] (auto e) {
            return propagate_and_evaluate_constants(ctx, e, go_again);
          },
          expr
        );
      }
    );
  } while (go_again);
  return e;
}

} // namespace insider
