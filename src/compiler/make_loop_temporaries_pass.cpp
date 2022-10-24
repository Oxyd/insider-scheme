#include "compiler/make_loop_temporaries_pass.hpp"

#include "compiler/ast.hpp"
#include "context.hpp"

namespace insider {

namespace {
  struct uses_any_of_visitor {
    std::unordered_set<ptr<local_variable>> const& vars;
    bool result = false;

    void
    enter(expression e, dfs_stack<expression>& stack) {
      visit([&] (auto expr) { push_children(expr, stack); }, e);
    }

    bool
    leave(expression e, dfs_stack<expression>&) {
      visit([&] (auto expr) { leave_expression(expr); }, e);
      return true;
    }

    void
    leave_expression(ptr<local_reference_expression> ref) {
      if (vars.contains(ref->variable()))
        result = true;
    }

    void
    leave_expression(auto) { }
  };
}

static bool
uses_any_of(std::unordered_set<ptr<local_variable>> const& vars, expression e) {
  uses_any_of_visitor v{vars};
  depth_first_search(e, v);
  return v.result;
}

std::unordered_set<ptr<local_variable>>
find_variables_to_precalculate(
  std::vector<definition_pair_expression> const& vars
) {
  if (vars.empty())
    return {};

  std::unordered_set<ptr<local_variable>> result;
  std::unordered_set<ptr<local_variable>> dead_vars{vars.front().variable()};

  for (std::size_t i = 1; i < vars.size(); ++i) {
    if (uses_any_of(dead_vars, vars[i].expression()))
      result.emplace(vars[i].variable());
    dead_vars.emplace(vars[i].variable());
  }

  return result;
}

static expression
make_temporaries(context& ctx, ptr<loop_continue> cont,
                 std::unordered_set<ptr<local_variable>> const& to_precalc) {
  std::vector<definition_pair_expression> new_vars;
  new_vars.reserve(cont->variables().size());

  std::vector<definition_pair_expression> precalc_vars;
  precalc_vars.reserve(to_precalc.size());

  for (definition_pair_expression const& dp : cont->variables())
    if (!to_precalc.contains(dp.variable()))
      new_vars.emplace_back(dp.variable(), dp.expression());
    else {
      auto new_var = make<local_variable>(ctx, dp.variable()->name());
      new_var->flags().is_loop_variable = true;
      precalc_vars.emplace_back(new_var, dp.expression());
      new_vars.emplace_back(dp.variable(),
                            make<local_reference_expression>(ctx, new_var));
    }

  return make<let_expression>(
    ctx,
    precalc_vars,
    make<loop_continue>(ctx, cont->id(), std::move(new_vars))
  );
}

static expression
make_loop_temporaries(context& ctx, ptr<loop_continue> cont) {
  auto to_precalc = find_variables_to_precalculate(cont->variables());
  if (!to_precalc.empty())
    return make_temporaries(ctx, cont, to_precalc);
  else
    return cont;
}

static expression
make_loop_temporaries(context&, auto e) { return e; }

expression
make_loop_temporaries(context& ctx, expression e, analysis_context) {
  return map_ast(ctx, e,
                 [&] (auto expr) { return make_loop_temporaries(ctx, expr); });
}

} // namespace insider
