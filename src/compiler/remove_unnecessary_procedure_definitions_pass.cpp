#include "compiler/remove_unnecessary_procedure_definitions_pass.hpp"

#include "compiler/ast.hpp"
#include "compiler/variable.hpp"
#include "context.hpp"
#include "util/depth_first_search.hpp"

namespace insider {

namespace {
  struct is_used_visitor {
    ptr<local_variable> var;
    bool                result = false;

    void
    enter(expression e, dfs_stack<expression>& stack) {
      if (!result)
        visit(
          [&] (auto expr) {
            enter_expression(expr);
            push_children(expr, stack);
          },
          e
        );
    }

    void
    enter_expression(ptr<local_reference_expression> ref) {
      if (ref->variable() == var)
        result = true;
    }

    void
    enter_expression(auto) { }

    bool
    leave(expression, dfs_stack<expression>&) { return true; }
  };
}

static bool
is_used(expression e, ptr<local_variable> var) {
  is_used_visitor v{var};
  depth_first_search(e, v);
  return v.result;
}

static std::vector<definition_pair_expression>
filter_unused_procedures(ptr<let_expression> let) {
  std::vector<definition_pair_expression> new_dps;
  for (auto const& dp : let->definitions())
    if (!is<lambda_expression>(dp.expression())
        || is_used(let->body(), dp.variable()))
      new_dps.emplace_back(dp);
  return new_dps;
}

static expression
remove_unnecessary_procedure_definitions(context& ctx,
                                         ptr<let_expression> let) {
  auto new_dps = filter_unused_procedures(let);
  if (new_dps.size() < let->definitions().size())
    return make<let_expression>(ctx, std::move(new_dps), let->body());
  else
    return let;
}

static std::vector<expression>
filter_unused_procedure_assignments(ptr<sequence_expression> seq) {
  std::vector<expression> new_exprs;
  for (expression e : seq->expressions()) {
    if (auto set = match<local_set_expression>(e))
      if (set->target()->flags().is_set_eliminable
          && is<lambda_expression>(set->expression())
          && !is_used(seq, set->target()))
        continue;

    new_exprs.emplace_back(e);
  }
  return new_exprs;
}

static expression
remove_unnecessary_procedure_definitions(context& ctx,
                                         ptr<sequence_expression> seq) {
  auto new_exprs = filter_unused_procedure_assignments(seq);
  if (new_exprs.size() < seq->expressions().size())
    return make<sequence_expression>(ctx, std::move(new_exprs));
  else
    return seq;
}

static expression
remove_unnecessary_procedure_definitions(context&, auto e) {
  return e;
}

expression
remove_unnecessary_procedure_definitions(context& ctx, expression e,
                                         analysis_context) {
  return map_ast(ctx, e,
                 [&] (auto expr) {
                   return remove_unnecessary_procedure_definitions(ctx, expr);
                 });
}

} // namespace insider
