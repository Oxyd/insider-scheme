#include "compiler/ast_transforms.hpp"

#include "compiler/ast.hpp"
#include "context.hpp"

#include <ranges>

namespace insider {

pass_list const all_passes{
  analyse_variables,
  inline_procedures,
  propagate_constants,
  box_set_variables,
  analyse_free_variables
};

expression
apply_passes(context& ctx, expression e, analysis_context ac,
             pass_list const& ps) {
  for (pass p : ps)
    e = p(ctx, e, ac);
  return e;
}

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
        return v->is_set;
      });
  return std::vector<ptr<local_variable>>(rng.begin(), rng.end());
}

static definition_pair_expression
box_definition_pair(context& ctx, definition_pair_expression const& dp) {
  return {dp.variable(), make_application(ctx, "box", dp.expression())};
}

static std::vector<definition_pair_expression>
box_definition_pairs(context& ctx,
                     std::vector<definition_pair_expression> const& dps) {
  auto rng = dps | std::views::transform(
    [&] (definition_pair_expression const& dp) {
      if (dp.variable()->is_set)
        return box_definition_pair(ctx, dp);
      else
        return dp;
    }
  );
  return std::vector(rng.begin(), rng.end());
}

static void
mark_set_variables(ptr<local_set_expression> set) {
  set->target()->is_set = true;
}

static void
mark_set_variables(ptr<top_level_set_expression> set) {
  if (!set->is_initialisation()) {
    set->target()->is_set = true;
    set->target()->constant_initialiser = {};
  }
}

static void
mark_set_variables(auto) { }

static void
assign_constant_initialiser(auto var, expression e) {
  if (is<literal_expression>(e) || is<lambda_expression>(e))
    var->constant_initialiser = e;
}

static void
find_constant_values(ptr<let_expression> let) {
  for (definition_pair_expression const& dp : let->definitions())
    if (!dp.variable()->is_set)
      assign_constant_initialiser(dp.variable(), dp.expression());
}

static void
find_constant_values(ptr<top_level_set_expression> set) {
  if (set->is_initialisation() && !set->target()->is_set)
    assign_constant_initialiser(set->target(), set->expression());
}

static void
find_constant_values(auto) { }

static void
visit_variables(analysis_context ac, auto e) {
  mark_set_variables(e);
  if (ac == analysis_context::closed)
    find_constant_values(e);
}

expression
analyse_variables(context&, expression expr, analysis_context ac) {
  traverse_postorder(expr, [&] (expression subexpr) {
    visit([&] (auto e) { visit_variables(ac, e); }, subexpr);
  });
  return expr;
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
    return make<let_expression>(ctx, std::move(dps),
                                assume<sequence_expression>(body));
  } else
    return let;
}

static expression
box_set_variables(context& ctx, ptr<lambda_expression> lambda) {
  std::vector<expression> sets;
  expression body = lambda->body();
  for (ptr<local_variable> param : lambda->parameters())
    if (param->is_set) {
      body = box_variable_references(ctx, body, param);
      sets.emplace_back(make<local_set_expression>(
        ctx, param,
        make_application(
          ctx, "box",
          make<local_reference_expression>(ctx, param)
        )
      ));
    }

  if (!sets.empty()) {
    auto proper_body = assume<sequence_expression>(body);
    std::vector<expression> body_exprs = std::move(sets);
    body_exprs.insert(body_exprs.end(),
                      proper_body->expressions().begin(),
                      proper_body->expressions().end());
    return make<lambda_expression>(
      ctx, lambda->parameters(), lambda->has_rest(),
      make<sequence_expression>(ctx, std::move(body_exprs)), lambda->name(),
      lambda->free_variables()
    );
  } else
    return lambda;
}

static expression
box_set_variables(context&, auto e) {
  return e;
}

expression
box_set_variables(context& ctx, expression s, analysis_context) {
  return map_ast(
    ctx, s,
    [&] (expression expr) {
      return visit([&] (auto e) { return box_set_variables(ctx, e); },
                   expr);
    }
  );
}

static bool
is_initialised_to_constant(auto var) {
  return var->constant_initialiser
         && is<literal_expression>(var->constant_initialiser);
}

static expression
propagate_constants(context&, ptr<local_reference_expression> ref) {
  if (is_initialised_to_constant(ref->variable()))
    return ref->variable()->constant_initialiser;
  else
    return ref;
}

static expression
propagate_constants(context&, ptr<top_level_reference_expression> ref) {
  if (is_initialised_to_constant(ref->variable()))
    return ref->variable()->constant_initialiser;
  else
    return ref;
}

static bool
is_const(definition_pair_expression const& dp) {
  return is_initialised_to_constant(dp.variable());
}

static std::vector<definition_pair_expression>
remove_const_definitions(std::vector<definition_pair_expression> const& dps) {
  std::vector<definition_pair_expression> result;
  result.reserve(dps.size());
  std::ranges::remove_copy_if(dps, std::back_inserter(result), is_const);
  return result;
}

static expression
propagate_constants(context& ctx, ptr<let_expression> let) {
  auto new_dps = remove_const_definitions(let->definitions());
  if (new_dps.empty())
    return let->body();
  else if (new_dps.size() < let->definitions().size())
    return make<let_expression>(ctx, std::move(new_dps), let->body());
  else
    return let;
}

static expression
propagate_constants(context&, auto x) { return x; }

expression
propagate_constants(context& ctx, expression e, analysis_context) {
  return map_ast(
    ctx, e,
    [&] (expression expr) {
      return visit([&] (auto e) { return propagate_constants(ctx, e); },
                   expr);
    }
  );
}

static ptr<lambda_expression>
find_constant_lambda_initialiser(auto var) {
  if (var->constant_initialiser
      && is<lambda_expression>(var->constant_initialiser))
    return assume<lambda_expression>(var->constant_initialiser);
  else
    return {};
}

static ptr<lambda_expression>
find_constant_lambda_initialiser_for_application_target(
  ptr<top_level_reference_expression> target_expr
) {
  return find_constant_lambda_initialiser(target_expr->variable());
}

static ptr<lambda_expression>
find_constant_lambda_initialiser_for_application_target(
  ptr<local_reference_expression> target_expr
) {
  return find_constant_lambda_initialiser(target_expr->variable());
}

static ptr<lambda_expression>
find_constant_lambda_initialiser_for_application_target(auto) {
  return {};
}

using variable_map
  = std::unordered_map<ptr<local_variable>, ptr<local_variable>>;

static std::vector<definition_pair_expression>
map_definition_pairs(variable_map const& map,
                     std::vector<definition_pair_expression> const& dps) {
  std::vector<definition_pair_expression> result;
  result.reserve(dps.size());

  for (definition_pair_expression const& dp : dps)
    if (auto mapping = map.find(dp.variable()); mapping != map.end())
      result.emplace_back(mapping->second, dp.expression());
    else
      result.emplace_back(dp);

  return result;
}

namespace {
  // Visitor that takes an AST and processes it for inlining in other parts of
  // the AST. This includes creating fresh variables everywhere.
  struct inliner {
    context&     ctx;
    variable_map map;

    explicit
    inliner(context& ctx)
      : ctx{ctx}
    { }

    void
    enter(ptr<let_expression> let) {
      for (auto const& dp : let->definitions()) {
        assert(!map.contains(dp.variable()));
        auto copy = make<local_variable>(ctx, *dp.variable());
        map.emplace(dp.variable(), copy);
      }
    }

    void
    enter(auto) { }

    expression
    leave(ptr<local_reference_expression> ref) {
      if (auto mapping = map.find(ref->variable()); mapping != map.end())
        return make<local_reference_expression>(ctx, mapping->second);
      else
        return ref;
    }

    expression
    leave(ptr<local_set_expression> set) {
      if (auto mapping = map.find(set->target()); mapping != map.end())
        return make<local_set_expression>(ctx, mapping->second,
                                          set->expression());
      else
        return set;
    }

    expression
    leave(ptr<let_expression> let) {
      return make<let_expression>(ctx,
                                  map_definition_pairs(map, let->definitions()),
                                  let->body());
    }

    expression
    leave(ptr<lambda_expression> lambda) {
      // Need to remove free variable information; it will be recomputed when
      // analyse_free_variables visits this for the AST this lambda is being
      // inlined into.

      if (!lambda->free_variables().empty())
        return make<lambda_expression>(
          ctx,
          lambda->parameters(),
          lambda->has_rest(),
          lambda->body(),
          lambda->name(),
          std::vector<ptr<local_variable>>{} // free_variables
        );
      else
        return lambda;
    }

    expression
    leave(auto e) { return e; }
  };
}

static expression
inline_application(context& ctx, ptr<application_expression> app,
                   ptr<lambda_expression> target) {
  assert(app->arguments().size() == target->parameters().size());

  std::vector<definition_pair_expression> dps;
  for (std::size_t i = 0; i < app->arguments().size(); ++i)
    dps.emplace_back(target->parameters()[i], app->arguments()[i]);

  return transform_ast_copy(
    ctx,
    make<let_expression>(ctx, std::move(dps), target->body()),
    inliner{ctx}
  );
}

namespace {
  struct inline_visitor {
    context& ctx;
    std::vector<ptr<lambda_expression>> entered_lambdas;

    inline_visitor(context& ctx)
      : ctx{ctx}
    { }

    void
    enter(ptr<lambda_expression> lambda) {
      entered_lambdas.push_back(lambda);
    }

    void
    enter(auto) { }

    bool
    is_self_recursive_call(ptr<lambda_expression> called_lambda) {
      return std::ranges::any_of(entered_lambdas,
                                 [&] (auto l) { return l == called_lambda; });
    }

    bool
    can_inline(ptr<lambda_expression> lambda) {
      return !lambda->has_rest() && !is_self_recursive_call(lambda);
    }

    expression
    leave(ptr<application_expression> app) {
      auto lambda = visit(
        [&] (auto e) {
          return find_constant_lambda_initialiser_for_application_target(e);
        },
        app->target()
      );

      if (lambda && can_inline(lambda))
        return inline_application(ctx, app, lambda);
      else
        return app;
    }

    expression
    leave([[maybe_unused]] ptr<lambda_expression> lambda) {
      assert(entered_lambdas.back() == lambda);
      entered_lambdas.pop_back();
      return lambda;
    }

    expression
    leave(auto expr) {
      return expr;
    }
  };
}

expression
inline_procedures(context& ctx, expression e, analysis_context) {
  return transform_ast(ctx, e, inline_visitor{ctx});
}

using variable_set = std::unordered_set<ptr<local_variable>>;

namespace {
  class free_variable_visitor : public expression_visitor {
  public:
    context& ctx;
    std::vector<variable_set> bound_vars_stack{variable_set{}};
    std::vector<variable_set> free_vars_stack{variable_set{}};

    explicit
    free_variable_visitor(context& ctx)
      : ctx{ctx}
    { }

    void
    enter_expression(ptr<lambda_expression> lambda) override {
      free_vars_stack.emplace_back();
      bound_vars_stack.emplace_back();
      for (ptr<local_variable> param : lambda->parameters())
        bound_vars_stack.back().emplace(param);
    }

    void
    leave_expression(ptr<lambda_expression> lambda) override {
      assert(lambda->free_variables().empty());

      auto inner_free = std::move(free_vars_stack.back());
      free_vars_stack.pop_back();
      bound_vars_stack.pop_back();

      for (ptr<local_variable> v : inner_free) {
        lambda->add_free_variable(ctx.store, v);

        // Lambda expression's free variables count as variable references in
        // the enclosing procedure.

        if (!bound_vars_stack.back().contains(v))
          free_vars_stack.back().emplace(v);
      }
    }

    void
    enter_expression(ptr<let_expression> let) override {
      for (definition_pair_expression const& dp : let->definitions())
        bound_vars_stack.back().emplace(dp.variable());
    }

    void
    leave_expression(ptr<let_expression> let) override {
      for (definition_pair_expression const& dp : let->definitions())
        bound_vars_stack.back().erase(dp.variable());
    }

    void
    enter_expression(ptr<local_reference_expression> ref) override {
      if (!bound_vars_stack.back().contains(ref->variable()))
        free_vars_stack.back().emplace(ref->variable());
    }

    void
    enter_expression([[maybe_unused]] ptr<local_set_expression> set) override {
      // Local set!s are boxed, so this shouldn't happen.
      assert(bound_vars_stack.back().contains(set->target()));
    }
  };
}

expression
analyse_free_variables(context& ctx, expression e, analysis_context) {
  free_variable_visitor v{ctx};
  depth_first_search(e, v);

  // Top-level can't have any free variables
  assert(v.free_vars_stack.back().empty());

  return e;
}

} // namespace insider
