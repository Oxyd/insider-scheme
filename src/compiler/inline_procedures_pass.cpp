#include "compiler/inline_procedures_pass.hpp"

#include "compiler/ast.hpp"
#include "compiler/clone_ast.hpp"
#include "compiler/parsing_context.hpp"
#include "context.hpp"

#include <fmt/format.h>

namespace insider {

static constexpr std::size_t inline_size_limit = 50;

static std::vector<definition_pair_expression>
make_definition_pairs_for_leading_args(context& ctx,
                                       ptr<application_expression> app,
                                       ptr<lambda_expression> lambda,
                                       std::size_t num_args) {
  std::vector<definition_pair_expression> dps;
  for (std::size_t i = 0; i < num_args; ++i)
    dps.emplace_back(lambda->parameters()[i].variable, app->arguments()[i]);
  for (std::size_t i = num_args; i < leading_parameter_count(lambda); ++i)
    dps.emplace_back(
      lambda->parameters()[i].variable,
      make<literal_expression>(ctx, ctx.constants->default_value)
    );
  return dps;
}

static expression
inline_nonvariadic_application(context& ctx, ptr<application_expression> app,
                               ptr<lambda_expression> target) {
  std::vector<definition_pair_expression> dps
    = make_definition_pairs_for_leading_args(ctx, app, target,
                                             app->arguments().size());

  return clone_ast(ctx,
                   make<let_expression>(ctx, std::move(dps), target->body()),
                   target->name());
}

static expression
make_tail_args_expression(context& ctx, ptr<application_expression> app,
                          std::size_t leading_args) {
  std::vector<expression> tail_args;
  for (std::size_t i = leading_args; i < app->arguments().size(); ++i)
    tail_args.push_back(app->arguments()[i]);

  return make<application_expression>(
    ctx,
    app->origin_location(),
    make_internal_reference(ctx, "list"),
    std::move(tail_args)
  );
}

static expression
inline_variadic_application(context& ctx, ptr<application_expression> app,
                            ptr<lambda_expression> target) {
  assert(!target->parameters().empty());

  std::size_t leading_args = leading_parameter_count(target);
  std::vector<definition_pair_expression> dps
    = make_definition_pairs_for_leading_args(ctx, app, target,
                                             std::min(leading_args,
                                                      app->arguments().size()));

  dps.emplace_back(target->parameters().back().variable,
                   make_tail_args_expression(ctx, app, leading_args));

  return clone_ast(ctx,
                   make<let_expression>(ctx, std::move(dps), target->body()),
                   target->name());
}

static expression
inline_application(parsing_context& pc,
                   ptr<application_expression> app,
                   ptr<lambda_expression> target) {
  auto fixed_app = reorder_supplement_and_validate_application(
    pc.ctx, pc.config.diagnostics, app, target
  );
  if (fixed_app) {
    if (target->has_rest())
      return inline_variadic_application(pc.ctx, fixed_app, target);
    else
      return inline_nonvariadic_application(pc.ctx, fixed_app, target);
  } else
    return app;
}

static bool
arity_matches(ptr<application_expression> app, ptr<lambda_expression> lambda) {
  if (!lambda->has_rest())
    return app->arguments().size() >= required_parameter_count(lambda)
           && app->arguments().size() <= leading_parameter_count(lambda);
  else
    return app->arguments().size() >= required_parameter_count(lambda);
}

static void
emit_invalid_arity_diagnostic(parsing_context& pc,
                              ptr<application_expression> app,
                              ptr<lambda_expression> lambda) {
  std::string message = fmt::format("Wrong number of arguments in call to {}: ",
                                    lambda->name());
  if (app->arguments().size() < required_parameter_count(lambda))
    message += fmt::format("Expected at least {}",
                           required_parameter_count(lambda));
  else
    // app->arguments.size() > leading_parameter_count(lambda)
    message += fmt::format("Expected at most {}",
                           leading_parameter_count(lambda));

  message += fmt::format(", got {}. Call will raise an exception at run-time",
                         app->arguments().size());
  pc.config.diagnostics.show(app->origin_location(), message);
}

static ptr<lambda_expression>
find_constant_lambda_initialiser(auto var) {
  if (auto init = var->constant_initialiser()) {
    if (auto lambda = match<lambda_expression>(init))
      return lambda;
    else if (auto ref = match<top_level_reference_expression>(init))
      return find_constant_lambda_initialiser(ref->variable());
  }

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
      return make<loop_continue>(ctx, loop->id(), make_loop_variables(app));
    }

    std::vector<definition_pair_expression>
    make_loop_variables(ptr<application_expression> app) const {
      assert(app->arguments().size() == lambda->parameters().size());

      std::vector<definition_pair_expression> result;
      result.reserve(app->arguments().size());

      for (std::size_t i = 0; i < app->arguments().size(); ++i)
        result.emplace_back(lambda->parameters()[i].variable,
                            app->arguments()[i]);

      return result;
    }
  };
}

static void
mark_lambda_parameters_as_loop_variables(ptr<lambda_expression> lambda) {
  for (auto const& param : lambda->parameters())
    param.variable->flags().is_loop_variable = true;
}

static std::vector<ptr<local_variable>>
make_loop_variables(ptr<lambda_expression> lambda) {
  std::vector<ptr<local_variable>> result;
  for (auto const& param : lambda->parameters())
    result.push_back(param.variable);
  return result;
}

static void
replace_lambda_body_with_loop(
  context& ctx, ptr<lambda_expression> lambda,
  std::unordered_set<ptr<application_expression>> const& calls
) {
  auto body = make<loop_body>(ctx, lambda->body(), make<loop_id>(ctx),
                              make_loop_variables(lambda));
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
    parsing_context& pc;
    std::vector<ptr<lambda_expression>> entered_lambdas;

    inline_visitor(parsing_context& pc)
      : pc{pc}
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

      if (lambda) {
        if (can_inline(app, lambda))
          return inline_application(pc, app, lambda);
        else if (!arity_matches(app, lambda))
          emit_invalid_arity_diagnostic(pc, app, lambda);
      }

      return app;
    }

    expression
    leave(ptr<lambda_expression> lambda) {
      assert(entered_lambdas.back() == lambda);
      entered_lambdas.pop_back();
      replace_self_calls_with_loops(pc.ctx, lambda);
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
inline_top_level_expression(parsing_context& pc, expression e,
                            top_level_procedure_map const& map) {
  if (auto set = match<top_level_set_expression>(e))
    if (auto mapped = map.find(set->target()); mapped != map.end())
      return make<top_level_set_expression>(pc.ctx,
                                            set->target(),
                                            mapped->second,
                                            set->is_initialisation());
  return transform_ast(pc.ctx, e, inline_visitor{pc});
}

static expression
inline_top_level(parsing_context& pc,
                 ptr<sequence_expression> top_level,
                 top_level_procedure_map const& map) {
  std::vector<expression> exprs;
  for (expression e : top_level->expressions())
    exprs.push_back(inline_top_level_expression(pc, e, map));
  return make<sequence_expression>(pc.ctx, std::move(exprs));
}

expression
inline_top_level(parsing_context& pc, ptr<sequence_expression> top_level) {
  auto inlining_order
    = sort_top_level_procedure_graph(build_top_level_procedure_graph(top_level),
                                     top_level);
  auto procedure_map = build_top_level_procedure_map(top_level);

  for (auto var : inlining_order)
    procedure_map[var] = transform_ast(pc.ctx, procedure_map[var],
                                       inline_visitor{pc});

  return inline_top_level(pc, top_level, procedure_map);
}

expression
inline_procedures(parsing_context& pc, expression e) {
  if (auto top_level = match<sequence_expression>(e))
    return inline_top_level(pc, top_level);
  else
    return transform_ast(pc.ctx, e, inline_visitor{pc});
}

} // namespace insider
