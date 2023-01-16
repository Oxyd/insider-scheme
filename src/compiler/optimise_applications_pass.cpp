#include "compiler/optimise_applications_pass.hpp"

#include "compiler/ast.hpp"
#include "compiler/expression.hpp"
#include "compiler/parsing_context.hpp"
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
  return !lambda->has_rest()
         && app->arguments().size() >= required_parameter_count(lambda)
         && app->arguments().size() <= leading_parameter_count(lambda);
}

static bool
is_native_application(context& ctx, ptr<application_expression> app) {
  if (auto callable = find_callable(ctx, app))
    return is<native_procedure>(callable);
  else
    return false;
}

static bool
has_unsupplied_optionals(ptr<application_expression> app,
                         ptr<lambda_expression> lambda) {
  return app->arguments().size() < leading_parameter_count(lambda);
}

static expression
visit_scheme_application(context& ctx, ptr<application_expression> app,
                         ptr<lambda_expression> lambda) {
  if (!scheme_application_is_valid(app, lambda))
    return app;

  if (has_keyword_arguments(app) || has_unsupplied_optionals(app, lambda)) {
    if (auto supplemented
          = reorder_supplement_and_validate_application(ctx, app, lambda))
      app = supplemented;
    else
      return app;
  }

  app->set_kind(application_expression::target_kind::scheme);
  return app;
}

template <auto Test>
static bool
is_literal_visitor(ptr<literal_expression> lit) {
  return Test(lit->value());
}

template <auto>
static bool
is_literal_visitor(auto) { return false; }

template <auto Test>
static bool
is_literal(expression e) {
  return visit([] (auto expr) { return is_literal_visitor<Test>(expr); }, e);
}

constexpr auto is_literal_one = is_literal<[] (ptr<> value) {
  if (auto i = match<integer>(value))
    return i->value() == 1;
  else
    return false;
}>;

static ptr<application_expression>
addition_substitutor(context& ctx, ptr<application_expression> app) {
  if (app->arguments().size() == 2) {
    if (is_literal_one(app->arguments()[0]))
      return make<application_expression>(
        ctx,
        app->origin(),
        make_internal_reference(ctx, "increment"),
        app->arguments()[1]
      );
    else if (is_literal_one(app->arguments()[1]))
      return make<application_expression>(
        ctx,
        app->origin(),
        make_internal_reference(ctx, "increment"),
        app->arguments()[0]
      );
  }

  return app;
}

static ptr<application_expression>
subtraction_substitutor(context& ctx, ptr<application_expression> app) {
  if (app->arguments().size() == 2) {
    if (is_literal_one(app->arguments()[1]))
      return make<application_expression>(
        ctx,
        app->origin(),
        make_internal_reference(ctx, "decrement"),
        app->arguments()[0]
      );
  } else if (app->arguments().size() == 1)
    return make<application_expression>(
      ctx,
      app->origin(),
      make_internal_reference(ctx, "negate"),
      app->arguments()[0]
    );

  return app;
}

constexpr auto is_default_value_literal = is_literal<is_default_value>;

static ptr<application_expression>
eq_substitutor(context& ctx, ptr<application_expression> app) {
  assert(app->arguments().size() == 2);

  if (is_default_value_literal(app->arguments()[0]))
    return make<application_expression>(
      ctx,
      app->origin(),
      make_internal_reference(ctx, "default-value?"),
      app->arguments()[1]
    );
  else if (is_default_value_literal(app->arguments()[1]))
    return make<application_expression>(
      ctx,
      app->origin(),
      make_internal_reference(ctx, "default-value?"),
      app->arguments()[0]
    );

  return app;
}

using application_substitutor
  = ptr<application_expression> (*)(context&, ptr<application_expression>);

namespace {
  struct application_visitor {
    using native_substitutor_map
      = std::unordered_map<ptr<top_level_variable>, application_substitutor>;

    context& ctx;
    native_substitutor_map native_substitutors;

    explicit
    application_visitor(context&);

    void
    enter(auto) { }

    ptr<application_expression>
    substitute(ptr<application_expression> app) {
      if (auto ref = match<top_level_reference_expression>(app->target()))
        if (auto substitutor = native_substitutors.find(ref->variable());
            substitutor != native_substitutors.end())
          return substitutor->second(ctx, app);
      return app;
    }

    expression
    visit_native_application(ptr<application_expression> app) {
      app = substitute(app);
      app->set_kind(application_expression::target_kind::native);
      return app;
    }

    expression
    leave(ptr<application_expression> app) {
      if (auto lambda = find_scheme_application_target(app))
        return visit_scheme_application(ctx, app, lambda);
      else if (is_native_application(ctx, app))
        return visit_native_application(app);
      else
        return app;
    }

    expression
    leave(auto e) { return e; }
  };
}

namespace {
  struct substitutor_definition {
    std::string             name;
    application_substitutor substitutor;
  };
}

static substitutor_definition
substitutors[]{
  {"+", addition_substitutor},
  {"-", subtraction_substitutor},
  {"eq?", eq_substitutor}
};

application_visitor:: application_visitor(context& ctx)
  : ctx{ctx}
{
  for (auto const& def : substitutors) {
    auto binding = ctx.internal_module()->find(ctx.intern(def.name));
    native_substitutors.emplace(assume<top_level_variable>(binding->variable),
                                def.substitutor);
  }
}

expression
optimise_applications(parsing_context& pc, expression e) {
  return transform_ast(pc.ctx, e, application_visitor{pc.ctx});
}

} // namespace insider
