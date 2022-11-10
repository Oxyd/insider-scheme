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

static bool
is_self_application(ptr<application_expression> app) {
  if (auto ref = match<local_reference_expression>(app->target()))
    return ref->variable()->flags().is_self_variable;
  else
    return false;
}

static bool
is_scheme_application(ptr<application_expression> app) {
  if (auto target = find_application_target(app))
    return is<lambda_expression>(target);
  else
    return is_self_application(app);
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
  if (is_scheme_application(app))
    app->set_kind(application_expression::target_kind::scheme);
  else if (is_native_application(ctx, app))
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
