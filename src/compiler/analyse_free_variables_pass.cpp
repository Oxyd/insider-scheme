#include "compiler/analyse_free_variables_pass.hpp"

#include "compiler/ast.hpp"
#include "compiler/variable.hpp"
#include "context.hpp"

#include <unordered_set>

namespace insider {

using variable_set = std::unordered_set<ptr<local_variable>>;

namespace {
  class free_variable_visitor {
  public:
    context& ctx;
    std::vector<variable_set> bound_vars_stack{variable_set{}};
    std::vector<variable_set> free_vars_stack{variable_set{}};

    explicit
    free_variable_visitor(context& ctx)
      : ctx{ctx}
    { }

    bool
    enter(ptr<lambda_expression> lambda) {
      free_vars_stack.emplace_back();
      bound_vars_stack.emplace_back();
      for (lambda_expression::parameter const& param : lambda->parameters())
        bound_vars_stack.back().emplace(param.variable);
      bound_vars_stack.back().emplace(lambda->self_variable());

      return true;
    }

    expression
    leave(ptr<lambda_expression> lambda) {
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

      return lambda;
    }

    bool
    enter(ptr<let_expression> let) {
      for (definition_pair_expression const& dp : let->definitions())
        bound_vars_stack.back().emplace(dp.variable());

      return true;
    }

    expression
    leave(ptr<let_expression> let) {
      for (definition_pair_expression const& dp : let->definitions())
        bound_vars_stack.back().erase(dp.variable());

      return let;
    }

    bool
    enter(ptr<local_reference_expression> ref) {
      if (!bound_vars_stack.back().contains(ref->variable()))
        free_vars_stack.back().emplace(ref->variable());

      return true;
    }

    bool
    enter([[maybe_unused]] ptr<local_set_expression> set) {
      // Local set!s are boxed, so this shouldn't happen.
      assert(bound_vars_stack.back().contains(set->target()));
      return true;
    }

    bool
    enter(auto) { return true; }

    expression
    leave(auto e) { return e; }
  };
}

expression
analyse_free_variables(context& ctx, expression e, analysis_context) {
  free_variable_visitor v{ctx};
  transform_ast(ctx, e, v);

  // Top-level can't have any free variables
  assert(v.free_vars_stack.back().empty());

  return e;
}

} // namespace insider
