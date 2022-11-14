#include "compiler/optimise_applications_pass.hpp"

#include "compiler/ast.hpp"
#include "context.hpp"
#include "runtime/basic_types.hpp"

namespace insider {

static expression
get_constant_initialiser(ptr<local_reference_expression> ref) {
  return ref->variable()->constant_initialiser();
}

static expression
get_constant_initialiser(ptr<top_level_reference_expression> ref) {
  return ref->variable()->constant_initialiser();
}

static expression
get_constant_initialiser(auto) { return {}; }

static expression
find_application_target(ptr<application_expression> app) {
  return visit([] (auto expr) { return get_constant_initialiser(expr); },
               app->target());
}

static ptr<>
find_callable(context& ctx,
              ptr<application_expression> app) {
  if (auto ref = match<top_level_reference_expression>(app->target()))
    return ctx.get_top_level(ref->variable()->index);
  else
    return {};
}

static ptr<lambda_expression>
find_scheme_application_target(ptr<application_expression> app) {
  if (auto target = find_application_target(app))
    return match<lambda_expression>(target);
  else
    return {};
}

static bool
scheme_application_is_valid(ptr<application_expression> app,
                            ptr<lambda_expression> lambda) {
  return app->arguments().size() == lambda->parameters().size()
         && !lambda->has_rest();
}

static bool
is_native_application(context& ctx, ptr<application_expression> app) {
  if (auto callable = find_callable(ctx, app))
    return is<native_procedure>(callable);
  else
    return false;
}

static expression
visit_application(context& ctx, ptr<application_expression> app) {
  if (auto lambda = find_scheme_application_target(app)) {
    if (scheme_application_is_valid(app, lambda))
      app->set_kind(application_expression::target_kind::scheme);
  } else if (is_native_application(ctx, app))
    app->set_kind(application_expression::target_kind::native);

  return app;
}

static expression
visit_application(context&, auto e) { return e; }

expression
optimise_applications(context& ctx, expression e, analysis_context) {
  return map_ast(ctx, e,
                 [&] (auto expr) { return visit_application(ctx, expr); });
}

} // namespace insider
