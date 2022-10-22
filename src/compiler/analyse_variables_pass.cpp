#include "compiler/analyse_variables_pass.hpp"

#include "compiler/analysis_context.hpp"
#include "compiler/ast.hpp"
#include "compiler/expression.hpp"
#include "context.hpp"
#include "util/depth_first_search.hpp"

namespace insider {

namespace {
  struct variable_analysis_visitor {
    context&                           ctx;
    analysis_context                   ac;
    std::vector<variable>              entered_sets;
    std::vector<std::vector<variable>> current_variables{{}};

    variable_analysis_visitor(context& ctx, analysis_context ac)
      : ctx{ctx}
      , ac{ac}
    { }

    ~variable_analysis_visitor() {
      assert(entered_sets.empty());
      assert(current_variables.size() == 1);
    }

    void
    enter(expression e, dfs_stack<expression>& stack) {
      visit(
        [&] (auto expr) {
          enter_expression(expr);
          push_children(expr, stack);
        },
        e
      );
    }

    bool
    leave(expression e, dfs_stack<expression>&) {
      visit(
        [&] (auto e) {
          visit_variables(e);
          leave_expression(e);
        },
        e
      );
      return true;
    }

    void
    enter_expression(ptr<local_set_expression> set) {
      enter_set(set->target(), set->expression());
    }

    void
    enter_expression(ptr<top_level_set_expression> set) {
      enter_set(set->target(), set->expression());
    }

    void
    enter_expression(ptr<if_expression>) {
      current_variables.emplace_back();
    }

    void
    enter_expression(ptr<lambda_expression> lambda) {
      current_variables.emplace_back();
      for (auto const& var : lambda->parameters())
        current_variables.back().push_back(var);
    }

    void
    enter_expression(ptr<let_expression> let) {
      for (auto const& dp : let->definitions())
        current_variables.back().push_back(dp.variable());
    }

    void
    enter_expression(auto) { }

    void
    leave_expression(ptr<local_set_expression> set) {
      leave_set(set->target());
    }

    void
    leave_expression(ptr<top_level_set_expression> set) {
      leave_set(set->target());
    }

    void
    leave_expression(ptr<if_expression>) {
      current_variables.pop_back();
    }

    void
    leave_expression(ptr<lambda_expression>) {
      current_variables.pop_back();
    }

    void
    leave_expression(auto) { }

    void
    enter_set(variable v, expression rhs) {
      if (is<lambda_expression>(rhs))
        entered_sets.push_back(v);
    }

    void
    leave_set(variable v) {
      if (!entered_sets.empty() && entered_sets.back() == v)
        entered_sets.pop_back();
    }

    bool
    is_current(variable v) {
      return std::ranges::find(current_variables.back(), v)
             != current_variables.back().end();
    }

    void
    update_variable_flags(ptr<local_set_expression> set) {
      variable_flags& flags = set->target()->flags();
      flags.is_set_eliminable = !flags.is_read && !flags.is_set
                                && is_current(set->target());
      flags.is_set = true;

      if (!flags.is_set_eliminable)
        set->target()->set_constant_initialiser(ctx.store, {});
    }

    void
    update_variable_flags(ptr<top_level_set_expression> set) {
      if (!set->is_initialisation()) {
        variable_flags& flags = set->target()->flags();
        flags.is_set_eliminable = !flags.is_read && !flags.is_set
                                  && is_current(set->target());
        flags.is_set = true;
        set->target()->set_constant_initialiser(ctx.store, {});
      }
    }

    bool
    variable_use_counts_as_read(variable var) {
      return std::ranges::find(entered_sets, var) == entered_sets.end();
    }

    void
    update_variable_flags(ptr<local_reference_expression> ref) {
      if (variable_use_counts_as_read(ref->variable()))
        ref->variable()->flags().is_read = true;
    }

    void
    update_variable_flags(ptr<top_level_reference_expression> ref) {
      if (variable_use_counts_as_read(ref->variable()))
        ref->variable()->flags().is_read = true;
    }

    void
    update_variable_flags(auto) { }

    void
    assign_constant_initialiser(auto var, expression e) {
      if (is<literal_expression>(e) || is<lambda_expression>(e))
        var->set_constant_initialiser(ctx.store, e);
    }

    void
    find_constant_values(ptr<let_expression> let) {
      for (definition_pair_expression const& dp : let->definitions())
        if (!dp.variable()->flags().is_set)
          assign_constant_initialiser(dp.variable(), dp.expression());
    }

    void
    find_constant_values(ptr<local_set_expression> set) {
      if (set->target()->flags().is_set_eliminable)
        assign_constant_initialiser(set->target(), set->expression());
    }

    void
    find_constant_values(ptr<top_level_set_expression> set) {
      if (set->is_initialisation() && !set->target()->flags().is_set)
        assign_constant_initialiser(set->target(), set->expression());
    }

    void
    find_constant_values(auto) { }

    void
    visit_variables(auto e) {
      update_variable_flags(e);
      if (ac == analysis_context::closed)
        find_constant_values(e);
    }
  };
}

expression
analyse_variables(context& ctx, expression expr, analysis_context ac) {
  depth_first_search(expr, variable_analysis_visitor{ctx, ac});
  return expr;
}

} // namespace insider
