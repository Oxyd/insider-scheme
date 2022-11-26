#include "compiler/remove_unnecessary_definitions_pass.hpp"

#include "compiler/ast.hpp"
#include "compiler/variable.hpp"
#include "context.hpp"
#include "util/depth_first_search.hpp"

namespace insider {

static bool
has_unused_variable_definition(ptr<let_expression> let) {
  return std::ranges::any_of(
    let->definitions(),
    [] (auto const& dp) { return !dp.variable()->flags().is_read; }
  );
}

static bool
needs_to_be_retained(ptr<literal_expression>) { return false; }

static bool
needs_to_be_retained(ptr<lambda_expression>) { return false; }

static bool
needs_to_be_retained(auto) { return true; }

static bool
needs_to_be_retained(expression e) {
  return visit([] (auto expr) { return needs_to_be_retained(expr); }, e);
}

static auto
filter_unnecessary_definitions(ptr<let_expression> let) {
  std::vector<definition_pair_expression> new_dps;
  std::vector<expression> retained_init_exprs;

  for (auto const& dp : let->definitions()) {
    if (!dp.variable()->flags().is_read) {
      if (needs_to_be_retained(dp.expression()))
        retained_init_exprs.push_back(dp.expression());
    } else
      new_dps.push_back(dp);
  }

  return std::tuple{new_dps, retained_init_exprs};
}

static expression
make_filtered_let(context& ctx,
                  std::vector<definition_pair_expression> dps,
                  std::vector<expression> pre_exprs,
                  expression body) {
  if (dps.empty() && pre_exprs.empty())
    return body;
  else if (dps.empty()) {
    pre_exprs.emplace_back(body);
    return make<sequence_expression>(ctx, std::move(pre_exprs));
  } else {
    auto new_let = make<let_expression>(ctx, std::move(dps), body);
    if (pre_exprs.empty())
      return new_let;
    else {
      pre_exprs.emplace_back(new_let);
      return make<sequence_expression>(ctx, std::move(pre_exprs));
    }
  }
}

static expression
remove_unnecessary_definitions(context& ctx, ptr<let_expression> let) {
  if (has_unused_variable_definition(let) || let->definitions().empty()) {
    auto [dps, exprs] = filter_unnecessary_definitions(let);
    return make_filtered_let(ctx, std::move(dps), std::move(exprs), let->body());
  } else
    return let;
}

static expression
remove_unnecessary_definitions(context& ctx, ptr<local_set_expression> set) {
  if (!set->target()->flags().is_read) {
    if (needs_to_be_retained(set->expression()))
      return set->expression();
    else
      return make<literal_expression>(ctx, ctx.constants->void_);
  } else
    return set;
}

static expression
remove_unnecessary_definitions(context&, auto e) { return e; }

expression
remove_unnecessary_definitions(context& ctx, expression e, analysis_context) {
  return map_ast(ctx, e,
                 [&] (auto expr) {
                   return remove_unnecessary_definitions(ctx, expr);
                 });
}

} // namespace insider
