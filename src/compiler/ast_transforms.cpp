#include "compiler/ast_transforms.hpp"

#include "compiler/ast.hpp"
#include "context.hpp"

#include <ranges>

namespace insider {

pass_list const all_passes{
  analyse_variables,
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
  return {dp.id(), dp.variable(),
          make_application(ctx, "box", dp.expression())};
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
    set->target()->constant_value = {};
  }
}

static void
mark_set_variables(auto) { }

static void
find_constant_values(ptr<let_expression> let) {
  for (definition_pair_expression const& dp : let->definitions())
    if (!dp.variable()->is_set)
      if (auto lit = match<literal_expression>(dp.expression()))
        dp.variable()->constant_value = lit->value();
}

static void
find_constant_values(ptr<top_level_set_expression> set) {
  if (set->is_initialisation() && !set->target()->is_set)
    if (auto lit = match<literal_expression>(set->expression()))
      set->target()->constant_value = lit->value();
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

static expression
propagate_constants(context& ctx, ptr<local_reference_expression> ref) {
  if (ref->variable()->constant_value)
    return make<literal_expression>(ctx, ref->variable()->constant_value);
  else
    return ref;
}

static expression
propagate_constants(context& ctx, ptr<top_level_reference_expression> ref) {
  if (ref->variable()->constant_value)
    return make<literal_expression>(ctx, ref->variable()->constant_value);
  else
    return ref;
}

static bool
is_const(definition_pair_expression const& dp) {
  return dp.variable()->constant_value != ptr<>{};
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
