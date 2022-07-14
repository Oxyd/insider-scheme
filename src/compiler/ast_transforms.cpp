#include "compiler/ast_transforms.hpp"

#include "compiler/expression.hpp"
#include "context.hpp"

#include <ranges>

namespace insider {

static expression
box_variable_reference(context& ctx,
                       ptr<local_reference_expression> local_ref,
                       ptr<variable> var) {
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
                       ptr<variable> var) {
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
box_variable_reference(context&, auto e, ptr<variable>) {
  return e;
}

static expression
box_variable_references(context& ctx, expression s, ptr<variable> var) {
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

static std::vector<ptr<variable>>
find_set_variables(ptr<let_expression> let) {
  auto rng = let->definitions()
    | std::views::transform([] (auto const& dp) { return dp.variable(); })
    | std::views::filter([] (ptr<variable> v) {
        return v->is_set;
      });
  return std::vector<ptr<variable>>(rng.begin(), rng.end());
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

static expression
box_set_variables(context& ctx, ptr<let_expression> let) {
  std::vector<ptr<variable>> set_vars = find_set_variables(let);
  if (!set_vars.empty()) {
    std::vector<definition_pair_expression> dps
      = box_definition_pairs(ctx, let->definitions());
    expression body = let->body();
    for (ptr<variable> v : set_vars)
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
  for (ptr<variable> param : lambda->parameters())
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

static void
mark_set_variables(ptr<local_set_expression> set) {
  set->target()->is_set = true;
}

static void
mark_set_variables(auto) { }

static void
mark_set_variables(expression expr) {
  traverse_postorder(expr, [] (expression subexpr) {
    visit([] (auto e) { mark_set_variables(e); }, subexpr);
  });
}

expression
box_set_variables(context& ctx, expression s) {
  mark_set_variables(s);
  return map_ast(
    ctx, s,
    [&] (expression expr) {
      return visit([&] (auto e) { return box_set_variables(ctx, e); },
                   expr);
    }
  );
}

using variable_set = std::unordered_set<ptr<variable>>;

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
      for (ptr<variable> param : lambda->parameters())
        bound_vars_stack.back().emplace(param);
    }

    void
    leave_expression(ptr<lambda_expression> lambda) override {
      auto inner_free = std::move(free_vars_stack.back());
      free_vars_stack.pop_back();
      bound_vars_stack.pop_back();

      for (ptr<variable> v : inner_free) {
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

void
analyse_free_variables(context& ctx, expression& e) {
  free_variable_visitor v{ctx};
  depth_first_search(e, v);

  // Top-level can't have any free variables
  assert(v.free_vars_stack.back().empty());
}

} // namespace insider