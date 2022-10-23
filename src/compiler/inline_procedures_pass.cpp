#include "compiler/inline_procedures_pass.hpp"

#include "compiler/ast.hpp"
#include "compiler/clone_ast.hpp"
#include "context.hpp"

#include <fmt/format.h>

namespace insider {

static constexpr std::size_t inline_size_limit = 50;

static std::vector<definition_pair_expression>
make_definition_pairs_for_mandatory_args(ptr<application_expression> app,
                                         ptr<lambda_expression> lambda,
                                         std::size_t num_args) {
  std::vector<definition_pair_expression> dps;
  for (std::size_t i = 0; i < num_args; ++i)
    dps.emplace_back(lambda->parameters()[i], app->arguments()[i]);
  return dps;
}

static expression
inline_nonvariadic_application(context& ctx, ptr<application_expression> app,
                               ptr<lambda_expression> target) {
  assert(app->arguments().size() == target->parameters().size());
  std::vector<definition_pair_expression> dps
    = make_definition_pairs_for_mandatory_args(app, target,
                                               app->arguments().size());

  return clone_ast(ctx,
                   make<let_expression>(ctx, std::move(dps), target->body()),
                   target->name());
}

static expression
make_tail_args_expression(context& ctx, ptr<application_expression> app,
                          std::size_t mandatory_args) {
  std::vector<expression> tail_args;
  for (std::size_t i = mandatory_args; i < app->arguments().size(); ++i)
    tail_args.push_back(app->arguments()[i]);

  return make<application_expression>(
    ctx,
    make_internal_reference(ctx, "list"),
    std::move(tail_args)
  );
}

static expression
inline_variadic_application(context& ctx, ptr<application_expression> app,
                            ptr<lambda_expression> target) {
  assert(!target->parameters().empty());
  assert(app->arguments().size() >= target->parameters().size() - 1);

  std::size_t mandatory_args = target->parameters().size() - 1;
  std::vector<definition_pair_expression> dps
    = make_definition_pairs_for_mandatory_args(app, target, mandatory_args);

  dps.emplace_back(target->parameters().back(),
                   make_tail_args_expression(ctx, app, mandatory_args));

  return clone_ast(ctx,
                   make<let_expression>(ctx, std::move(dps), target->body()),
                   target->name());
}

static expression
inline_application(context& ctx, ptr<application_expression> app,
                   ptr<lambda_expression> target) {
  if (target->has_rest())
    return inline_variadic_application(ctx, app, target);
  else
    return inline_nonvariadic_application(ctx, app, target);
}

static bool
arity_matches(ptr<application_expression> app, ptr<lambda_expression> lambda) {
  if (!lambda->has_rest())
    return app->arguments().size() == lambda->parameters().size();
  else
    return app->arguments().size() >= lambda->parameters().size() - 1;
}

static ptr<lambda_expression>
find_constant_lambda_initialiser(auto var) {
  if (var->constant_initialiser()
      && is<lambda_expression>(var->constant_initialiser()))
    return assume<lambda_expression>(var->constant_initialiser());
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

static bool
is_small_enough_to_inline(ptr<lambda_expression> lambda) {
  return size_estimate(lambda->body()) < inline_size_limit;
}

static bool
is_self_call(ptr<local_variable> self_var,
             ptr<application_expression> app) {
  if (auto local_ref = match<local_reference_expression>(app->target()))
    return local_ref->variable() == self_var;
  else
    return false;
}

namespace {
  struct self_tail_call_visitor {
    ptr<local_variable>                             self;
    std::unordered_set<ptr<application_expression>> result;

    explicit
    self_tail_call_visitor(ptr<local_variable> self)
      : self{self}
    { }

    void
    enter(expression e, dfs_stack<expression>& stack) {
      visit([&] (auto expr) { enter_expression(expr, stack); }, e);
    }

    void
    enter_expression(ptr<sequence_expression> seq,
                     dfs_stack<expression>& stack) {
      if (!seq->expressions().empty())
        stack.push_back(seq->expressions().back());
    }

    void
    enter_expression(ptr<if_expression> ifexpr,
                     dfs_stack<expression>& stack) {
      stack.push_back(ifexpr->consequent());
      stack.push_back(ifexpr->alternative());
    }

    void
    enter_expression(ptr<let_expression> let,
                     dfs_stack<expression>& stack) {
      stack.push_back(let->body());
    }

    void
    enter_expression(auto, dfs_stack<expression>&) { }

    bool
    leave(expression e, dfs_stack<expression>&) {
      visit([&] (auto expr) { leave_expression(expr); }, e);
      return true;
    }

    void
    leave_expression(ptr<application_expression> app) {
      if (is_self_call(self, app))
        result.emplace(app);
    }

    void
    leave_expression(auto) { }
  };
}

static std::unordered_set<ptr<application_expression>>
find_self_tail_calls(ptr<lambda_expression> lambda) {
  self_tail_call_visitor v{lambda->self_variable()};
  depth_first_search(lambda->body(), v);
  return v.result;
}

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

namespace {
  struct loop_substitutor {
    context&                                               ctx;
    ptr<lambda_expression>                                 lambda;
    std::unordered_set<ptr<application_expression>> const& calls;
    ptr<loop_body>                                         loop;

    loop_substitutor(
      context& ctx, ptr<lambda_expression> lambda,
      std::unordered_set<ptr<application_expression>> const& calls,
      ptr<loop_body> loop
    )
      : ctx{ctx}
      , lambda{lambda}
      , calls{calls}
      , loop{loop}
    { }

    void
    enter(auto) { }

    expression
    leave(ptr<application_expression> app) {
      if (calls.contains(app)
          && app->arguments().size() == lambda->parameters().size())
        return substitute_self_call(app);
      else
        return app;
    }

    expression
    leave(auto e) { return e; }

    expression
    substitute_self_call(ptr<application_expression> app) const {
      lambda->remove_self_reference();

      auto loop_vars = make_loop_variables(app);
      auto to_precalc = find_variables_to_precalculate(loop_vars);
      if (to_precalc.empty())
        return make<loop_continue>(ctx, loop->id(), std::move(loop_vars));
      else
        return make_continue_with_precalculation(loop_vars, to_precalc);
    }

    std::vector<definition_pair_expression>
    make_loop_variables(ptr<application_expression> app) const {
      assert(app->arguments().size() == lambda->parameters().size());

      std::vector<definition_pair_expression> result;
      result.reserve(app->arguments().size());

      for (std::size_t i = 0; i < app->arguments().size(); ++i)
        result.emplace_back(lambda->parameters()[i], app->arguments()[i]);

      return result;
    }

    std::unordered_set<ptr<local_variable>>
    find_variables_to_precalculate(
      std::vector<definition_pair_expression> const& vars
    ) const {
      assert(vars.size() == lambda->parameters().size());

      if (lambda->parameters().empty())
        return {};

      std::unordered_set<ptr<local_variable>> result;
      std::unordered_set<ptr<local_variable>> dead_vars{
        lambda->parameters().front()
      };

      for (std::size_t i = 1; i < lambda->parameters().size(); ++i) {
        if (uses_any_of(dead_vars, vars[i].expression()))
          result.emplace(lambda->parameters()[i]);
        dead_vars.emplace(lambda->parameters()[i]);
      }

      return result;
    }

    expression
    make_continue_with_precalculation(
      std::vector<definition_pair_expression> const& vars,
      std::unordered_set<ptr<local_variable>> const& to_precalc
    ) const {
      std::vector<definition_pair_expression> new_vars;
      new_vars.reserve(vars.size());

      std::vector<definition_pair_expression> precalc_vars;
      precalc_vars.reserve(to_precalc.size());

      for (definition_pair_expression const& dp : vars)
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
        make<loop_continue>(ctx, loop->id(), std::move(new_vars))
      );
    }
  };
}

static void
mark_lambda_parameters_as_loop_variables(ptr<lambda_expression> lambda) {
  for (auto var : lambda->parameters())
    var->flags().is_loop_variable = true;
}

static void
replace_lambda_body_with_loop(
  context& ctx, ptr<lambda_expression> lambda,
  std::unordered_set<ptr<application_expression>> const& calls
) {
  auto body = make<loop_body>(ctx, lambda->body(), make<loop_id>(ctx),
                              lambda->parameters());
  transform_ast(ctx, body, loop_substitutor{ctx, lambda, calls, body});
  lambda->update_body(ctx.store, body);
  mark_lambda_parameters_as_loop_variables(lambda);
}

static void
replace_self_calls_with_loops(context& ctx, ptr<lambda_expression> lambda) {
  if (lambda->has_rest())
    return;

  auto calls = find_self_tail_calls(lambda);
  if (calls.empty())
    return;

  replace_lambda_body_with_loop(ctx, lambda, calls);
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
    is_self_referential(ptr<lambda_expression> called_lambda) {
      return called_lambda->num_self_references() != 0u;
    }

    bool
    can_inline(ptr<application_expression> app, ptr<lambda_expression> lambda) {
      return !is_self_recursive_call(lambda)
             && !is_self_referential(lambda)
             && arity_matches(app, lambda)
             && is_small_enough_to_inline(lambda);
    }

    expression
    leave(ptr<application_expression> app) {
      auto lambda = visit(
        [&] (auto e) {
          return find_constant_lambda_initialiser_for_application_target(e);
        },
        app->target()
      );

      if (lambda && can_inline(app, lambda))
        return inline_application(ctx, app, lambda);
      else
        return app;
    }

    expression
    leave(ptr<lambda_expression> lambda) {
      assert(entered_lambdas.back() == lambda);
      entered_lambdas.pop_back();
      replace_self_calls_with_loops(ctx, lambda);
      return lambda;
    }

    expression
    leave(auto expr) {
      return expr;
    }
  };
}

namespace {
  struct top_level_procedure_info {
    std::unordered_set<ptr<top_level_variable>> callers;
    std::unordered_set<ptr<top_level_variable>> callees;
  };

  using top_level_procedure_graph
    = std::unordered_map<ptr<top_level_variable>, top_level_procedure_info>;
}

static void
add_edge(top_level_procedure_graph& graph, ptr<top_level_variable> from,
         ptr<top_level_variable> to) {
  graph[from].callees.emplace(to);
  graph[to].callers.emplace(from);
}

static void
build_top_level_procedure_graph(top_level_procedure_graph& graph,
                                ptr<top_level_variable> current_proc,
                                ptr<application_expression> app) {
  if (auto ref = match<top_level_reference_expression>(app->target()))
    if (graph.contains(ref->variable()))
      add_edge(graph, current_proc, ref->variable());
}

static void
build_top_level_procedure_graph(
  top_level_procedure_graph&,
  ptr<top_level_variable>,
  [[maybe_unused]] ptr<top_level_set_expression> set
) {
  assert(!set->is_initialisation());
}

static void
build_top_level_procedure_graph(top_level_procedure_graph&,
                                ptr<top_level_variable>,
                                auto)
{ }

static auto
top_level_procedures(ptr<sequence_expression> top_level) {
  return
    top_level->expressions()
      | std::views::filter([] (expression e) {
          return is<top_level_set_expression>(e);
        })
      | std::views::transform([] (expression e) {
          return assume<top_level_set_expression>(e);
        })
      | std::views::filter([] (ptr<top_level_set_expression> set) {
          return set->is_initialisation()
                 && is<lambda_expression>(set->expression());
        });
}

static top_level_procedure_graph
build_top_level_procedure_graph(ptr<sequence_expression> top_level) {
  top_level_procedure_graph graph;

  for (ptr<top_level_set_expression> set : top_level_procedures(top_level))
    graph.emplace(set->target(), top_level_procedure_info{});

  for (ptr<top_level_set_expression> set : top_level_procedures(top_level))
    traverse_postorder(
      set->expression(),
      [&] (auto expr) {
        build_top_level_procedure_graph(graph, set->target(), expr);
      }
    );

  return graph;
}

static ptr<top_level_variable>
find_procedure_with_least_number_of_callees(
  top_level_procedure_graph const& graph,
  ptr<sequence_expression> top_level
) {
  std::size_t min_num_callees = std::numeric_limits<std::size_t>::max();
  ptr<top_level_variable> result;

  // Iterating over the top level procedures might seem pointless and
  // inefficient here, and indeed it is inefficient. However, the order in which
  // things appear in top_level is well-defined, unlike the order of iteration
  // of graph because that's an unordered_map and it uses variable addresses
  // as keys.
  //
  // Having a well-defined order of inlining makes the intepreter deterministic,
  // which is helpful for debugging. The top level should be relatively small
  // anyway, likely few hundred to low thousands of definitions on the top end.

  for (ptr<top_level_set_expression> set : top_level_procedures(top_level)) {
    ptr<top_level_variable> var = set->target();
    if (auto info = graph.find(var); info != graph.end())
      if (info->second.callees.size() < min_num_callees) {
        result = var;
        min_num_callees = info->second.callees.size();
      }
  }

  return result;
}

static void
remove_procedure(top_level_procedure_graph& graph,
                 ptr<top_level_variable> proc) {
  top_level_procedure_info const& info = graph[proc];

  for (auto callee : info.callees)
    graph[callee].callers.erase(proc);

  for (auto caller : info.callers)
    graph[caller].callees.erase(proc);

  graph.erase(proc);
}

static std::vector<ptr<top_level_variable>>
sort_top_level_procedure_graph(top_level_procedure_graph graph,
                               ptr<sequence_expression> top_level) {
  std::vector<ptr<top_level_variable>> result;
  while (!graph.empty()) {
    auto proc = find_procedure_with_least_number_of_callees(graph, top_level);
    result.push_back(proc);
    remove_procedure(graph, proc);
  }

  return result;
}

using top_level_procedure_map
  = std::unordered_map<ptr<top_level_variable>, expression>;

static top_level_procedure_map
build_top_level_procedure_map(ptr<sequence_expression> top_level) {
  top_level_procedure_map result;
  for (auto set : top_level_procedures(top_level))
    result.emplace(set->target(), set->expression());
  return result;
}

static expression
inline_top_level_expression(context& ctx, expression e,
                            top_level_procedure_map const& map) {
  if (auto set = match<top_level_set_expression>(e))
    if (auto mapped = map.find(set->target()); mapped != map.end())
      return make<top_level_set_expression>(ctx, set->target(), mapped->second,
                                            set->is_initialisation());
  return transform_ast(ctx, e, inline_visitor{ctx});
}

static expression
inline_top_level(context& ctx,
                 ptr<sequence_expression> top_level,
                 top_level_procedure_map const& map) {
  std::vector<expression> exprs;
  for (expression e : top_level->expressions())
    exprs.push_back(inline_top_level_expression(ctx, e, map));
  return make<sequence_expression>(ctx, std::move(exprs));
}

expression
inline_top_level(context& ctx, ptr<sequence_expression> top_level) {
  auto inlining_order
    = sort_top_level_procedure_graph(build_top_level_procedure_graph(top_level),
                                     top_level);
  auto procedure_map = build_top_level_procedure_map(top_level);

  for (auto var : inlining_order)
    procedure_map[var] = transform_ast(ctx, procedure_map[var],
                                       inline_visitor{ctx});

  return inline_top_level(ctx, top_level, procedure_map);
}

expression
inline_procedures(context& ctx, expression e, analysis_context) {
  if (auto top_level = match<sequence_expression>(e))
    return inline_top_level(ctx, top_level);
  else
    return transform_ast(ctx, e, inline_visitor{ctx});
}

} // namespace insider
