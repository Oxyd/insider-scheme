#include "compiler/ast_transforms.hpp"

#include "compiler/expression.hpp"
#include "context.hpp"

namespace insider {

static void
box_variable_reference(context& ctx,
                       expression* expr,
                       local_reference_expression& local_ref,
                       std::shared_ptr<variable> const& var) {
  if (local_ref.variable == var) {
    std::shared_ptr<variable> var = std::move(local_ref.variable);
    expr->value = make_application_expression(
      ctx, "unbox",
      make_expression<local_reference_expression>(std::move(var))
    );
  }
}

static void
box_variable_reference(context& ctx,
                       expression* expr,
                       local_set_expression& local_set,
                       std::shared_ptr<variable> const& var) {
  if (local_set.target == var) {
    local_set_expression original_set = std::move(local_set);
    expr->value = make_application_expression(
      ctx, "box-set!",
      make_expression<local_reference_expression>(original_set.target),
      std::move(original_set.expression)
    );
  }
}

static void
box_variable_reference(context&, expression*, auto&,
                       std::shared_ptr<variable> const&) { }

static void
box_variable_references(context& ctx, expression* s,
                        std::shared_ptr<variable> const& var) {
  traverse_postorder(
    s,
    [&] (expression* expr) {
      std::visit([&] (auto& e) { box_variable_reference(ctx, expr, e, var); },
                 expr->value);
    }
  );
}

static void
box_set_variable(context& ctx, expression* expr, let_expression& let) {
  for (definition_pair_expression& def : let.definitions)
    if (def.variable->is_set) {
      box_variable_references(ctx, expr, def.variable);

      std::unique_ptr<expression> orig_expr = std::move(def.expression);
      def.expression = make_application(ctx, "box",
                                        std::move(orig_expr));
    }
}

static void
box_set_variable(context& ctx, expression* expr, lambda_expression& lambda) {
  for (std::shared_ptr<variable> const& param : lambda.parameters)
    if (param->is_set) {
      box_variable_references(ctx, expr, param);

      auto ref = std::make_unique<expression>(
        local_reference_expression{param}
      );
      auto box = make_application(ctx, "box", std::move(ref));
      auto set = make_expression<local_set_expression>(param, std::move(box));

      lambda.body.expressions.insert(lambda.body.expressions.begin(),
                                     std::move(set));
    }
}

static void
box_set_variable(context&, expression*, auto&) { }

void
box_set_variables(context& ctx, expression* s) {
  traverse_postorder(
    s,
    [&] (expression* expr) {
      std::visit([&] (auto& e) { box_set_variable(ctx, expr, e); },
                 expr->value);
    }
  );
}

using variable_set = std::unordered_set<std::shared_ptr<variable>>;

namespace {
  class free_variable_visitor : public expression_visitor {
  public:
    std::vector<variable_set> bound_vars_stack{variable_set{}};
    std::vector<variable_set> free_vars_stack{variable_set{}};

    void
    enter_expression(lambda_expression& lambda) override {
      free_vars_stack.emplace_back();
      bound_vars_stack.emplace_back();
      for (auto const& param : lambda.parameters)
        bound_vars_stack.back().emplace(param);
    }

    void
    leave_expression(lambda_expression& lambda) override {
      auto inner_free = std::move(free_vars_stack.back());
      free_vars_stack.pop_back();
      bound_vars_stack.pop_back();

      for (auto const& v : inner_free) {
        lambda.free_variables.push_back(v);

        // Lambda expression's free variables count as variable references in
        // the enclosing procedure.

        if (!bound_vars_stack.back().count(v))
          free_vars_stack.back().emplace(v);
      }
    }

    void
    enter_expression(let_expression& let) override {
      for (definition_pair_expression const& dp : let.definitions)
        bound_vars_stack.back().emplace(dp.variable);
    }

    void
    leave_expression(let_expression& let) override {
      for (definition_pair_expression const& dp : let.definitions)
        bound_vars_stack.back().erase(dp.variable);
    }

    void
    enter_expression(local_reference_expression& ref) override {
      if (!bound_vars_stack.back().count(ref.variable))
        free_vars_stack.back().emplace(ref.variable);
    }

    void
    enter_expression([[maybe_unused]] local_set_expression& set) override {
      // Local set!s are boxed, so this shouldn't happen.
      assert(bound_vars_stack.back().count(set.target));
    }
  };
}

void
analyse_free_variables(expression* e) {
  free_variable_visitor v;
  depth_first_search(e, v);

  // Top-level can't have any free variables
  assert(v.free_vars_stack.back().empty());
}

} // namespace insider
