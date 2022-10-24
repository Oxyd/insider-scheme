#include "compiler/evaluate_constants_pass.hpp"

#include "compiler/ast.hpp"
#include "compiler/clone_ast.hpp"
#include "context.hpp"
#include "runtime/basic_types.hpp"
#include "vm/vm.hpp"

#include <stdexcept>
#include <unordered_map>

namespace insider {

static constexpr std::size_t max_loop_iterations = 1000;

namespace {
  struct variable_info {
    variable   var;
    expression init_expr;
    expression loop_init_expr;

    explicit
    variable_info(ptr<local_variable> var)
      : var{var}
      , init_expr{var->constant_initialiser()}
    { }

    explicit
    variable_info(ptr<top_level_variable> var)
      : var{var}
      , init_expr{var->constant_initialiser()}
    { }
  };

  class static_environment {
  public:
    bool
    empty() const { return scopes_.empty(); }

    void
    push_scope() {
      scopes_.emplace_back();
    }

    void
    pop_scope() {
      assert(!scopes_.empty());
      scopes_.pop_back();
    }

    variable_info&
    add_variable(ptr<local_variable> var) {
      return scopes_.back().emplace_back(variable_info{var});
    }

    variable_info&
    find_variable(ptr<local_variable> v) {
      for (auto& scope : scopes_ | std::views::reverse)
        for (auto& info : scope)
          if (info.var == v)
            return info;

      assert(false);
      throw std::logic_error{"Unknown variable"};
    }

    variable_info
    find_variable(ptr<local_variable> var) const {
      return const_cast<static_environment*>(this)->find_variable(var);
    }

    variable_info
    find_variable(ptr<top_level_variable> var) const {
      return variable_info{var};
    }

  private:
    std::vector<std::vector<variable_info>> scopes_;
  };
}

static bool
is_reference_to_constant_variable(expression e) {
  if (auto local_ref = match<local_reference_expression>(e))
    return !local_ref->variable()->flags().is_set;
  else if (auto top_level_ref = match<top_level_reference_expression>(e))
    return !top_level_ref->variable()->flags().is_set;
  else
    return false;
}

static bool
is_constant_propagable_expression(expression e) {
  return is<literal_expression>(e) || is_reference_to_constant_variable(e);
}

static ptr<>
constant_value_for_expression(expression e, static_environment const& env);

static ptr<>
constant_value_for_expression(ptr<literal_expression> lit,
                              static_environment const&) {
  return lit->value();
}

static ptr<>
constant_value_for_expression(ptr<local_reference_expression> ref,
                              static_environment const& env) {
  auto info = env.find_variable(ref->variable());
  if (info.init_expr)
    if (auto lit = match<literal_expression>(info.init_expr))
      return lit->value();
  return {};
}

static ptr<>
constant_value_for_expression(ptr<top_level_reference_expression> ref,
                              static_environment const& env) {
  auto info = env.find_variable(ref->variable());
  if (info.init_expr)
    if (auto lit = match<literal_expression>(info.init_expr))
      return lit->value();
  return {};
}

static ptr<>
constant_value_for_expression(ptr<sequence_expression> seq,
                              static_environment const& env) {
  // Technically the correct condition is "all subexpressions are constant",
  // but it's quite unlikely that a sequence with multiple subexpressions will
  // be all-constant.

  if (seq->expressions().size() == 1)
    return constant_value_for_expression(seq->expressions().front(), env);
  else
    return {};
}

static ptr<>
constant_value_for_expression(auto, static_environment const&) { return {}; }

static ptr<>
constant_value_for_expression(expression e, static_environment const& env) {
  return visit(
    [&] (auto expr) { return constant_value_for_expression(expr, env); },
    e
  );
}

static expression
ignore_unitary_sequences(expression e) {
  while (true) {
    if (auto seq = match<sequence_expression>(e))
      if (seq->expressions().size() == 1) {
        e = seq->expressions().front();
        continue;
      }

    return e;
  }
}

static expression
constant_initialiser_expression(expression e) {
  if (auto seq = match<sequence_expression>(e)) {
    assert(seq->expressions().size() == 1);
    return constant_initialiser_expression(seq->expressions().front());
  } else
    return e;
}

static ptr<>
find_constant_evaluable_callable(context& ctx,
                                 ptr<application_expression> app) {
  if (auto ref = match<top_level_reference_expression>(app->target())) {
    ptr<> callable = ctx.get_top_level(ref->variable()->index);
    if (auto np = match<native_procedure>(callable))
      if (np->constant_evaluable)
        return np;
  }

  return {};
}

static bool
can_be_constant_evaluated(auto const& operands,
                          static_environment const& env) {
  return std::ranges::all_of(
    operands,
    [&] (expression e) {
      return visit(
        [&] (auto expr) {
          return constant_value_for_expression(expr, env) != nullptr;
        },
        e
      );
    }
  );
}

static void
coalesce_eqv_values(context& ctx, std::vector<ptr<>>& values) {
  // Values which are eqv? are meant to be coalesced within the whole program.
  // This coalescing normally happens due to the compiler calling
  // context::intern_static, which hasn't happened yet.
  //
  // Technically, this could produce invalid results when a value is meant to
  // be eqv? to another value not in this call, but for that to matter the
  // called procedure would somehow have to have access to objects outside
  // its arguments, which shouldn't happen for constant-evaluable procedures.

  for (std::size_t i = 0; i < values.size(); ++i)
    for (std::size_t j = i + 1; j < values.size(); ++j)
      if (values[i] != values[j] && eqv(ctx, values[i], values[j]))
        values[j] = values[i];
}

static std::vector<ptr<>>
make_arguments_for_constant_evaluation(context& ctx,
                                       auto const& arguments,
                                       static_environment const& env) {
  std::vector<ptr<>> result;
  result.reserve(arguments.size());
  for (expression arg : arguments)
    result.push_back(constant_value_for_expression(arg, env));

  coalesce_eqv_values(ctx, result);
  return result;
}

static expression
evaluate_constant_application(context& ctx, ptr<> callable,
                              auto const& arguments,
                              static_environment const& env) {
  try {
    ptr<> result = call_with_continuation_barrier(
      ctx, callable, make_arguments_for_constant_evaluation(ctx, arguments, env)
    );
    return make<literal_expression>(ctx, result);
  } catch (...) {
    return {};
  }
}

namespace {
  struct evaluate_constants_visitor {
    struct node {
      expression  expr;
      std::size_t visit_count = 0;

      node(expression e) : expr{e} { }
    };

    context&           ctx;
    result_stack       results;
    static_environment env;

    explicit
    evaluate_constants_visitor(context& ctx)
      : ctx{ctx}
    { }

    void
    enter(node& n, dfs_stack<node>& stack) {
      visit([&] (auto expr) { enter_expression(expr, n, stack); },
            n.expr);
    }

    bool
    leave(node& n, dfs_stack<node>& stack) {
      return visit([&] (auto expr) { return leave_expression(expr, n, stack); },
                   n.expr);
    }

    void
    enter_expression(ptr<lambda_expression> lambda, node&,
                     dfs_stack<node>& stack) {
      env.push_scope();
      for (auto p : lambda->parameters())
        env.add_variable(p);
      env.add_variable(lambda->self_variable());

      push_children(lambda, stack);
    }

    void
    enter_expression(ptr<let_expression> let, node&, dfs_stack<node>& stack) {
      // Process definition expressions first so that constant bindings for the
      // introduced variables are known when processing the body.

      for (auto const& def : let->definitions() | std::views::reverse)
        stack.push_back({def.expression()});
    }

    void
    enter_expression(auto expr, node&, dfs_stack<node>& stack) {
      push_children(expr, stack);
    }

    void
    push_children(auto expr, dfs_stack<node>& stack) {
      expr->visit_subexpressions([&] (expression child) {
        stack.push_back({child});
      });
    }

    bool
    leave_expression(ptr<lambda_expression> lambda, node&, dfs_stack<node>&) {
      env.pop_scope();
      return leave_expression_for_final_time(lambda);
    }

    bool
    leave_expression(ptr<let_expression> let, node& n, dfs_stack<node>& stack) {
      if (n.visit_count++ == 0) {
        update_let_bound_variables(let);
        stack.push_back({let->body()});
        return false;
      } else {
        env.pop_scope();
        return leave_expression_for_final_time(let);
      }
    }

    bool
    leave_expression(ptr<loop_body> loop, node& n, dfs_stack<node>& stack) {
      ++n.visit_count;

      if (!can_be_folded(loop) && n.visit_count == 1)
        return leave_expression_for_final_time(loop);
      else if (n.visit_count == 1) {
        fold_loop_entry(loop, stack);
        return false;
      } else if (n.visit_count > max_loop_iterations) {
        results.pop_back();
        push_loop_result(loop);
        return true;
      } else
        return fold_loop_iteration(loop, stack);
    }

    bool
    can_be_folded(ptr<loop_body> loop) {
      return std::ranges::all_of(
        loop->variables(),
        [&] (ptr<local_variable> var) -> bool {
          return static_cast<bool>(env.find_variable(var).loop_init_expr);
        }
      );
    }

    void
    bind_initial_loop_values(ptr<loop_body> loop) {
      env.push_scope();
      for (ptr<local_variable> var : loop->variables()) {
        variable_info info = env.find_variable(var);
        env.add_variable(var).init_expr = info.loop_init_expr;
      }
    }

    void
    fold_loop_entry(ptr<loop_body> loop, dfs_stack<node>& stack) {
      loop->update(ctx, results);
      bind_initial_loop_values(loop);
      stack.push_back(clone_ast(ctx, loop->body()));
    }

    void
    push_loop_result(expression result) {
      results.push_back(result);
      env.pop_scope();
    }

    void
    make_loop_iteration(ptr<loop_body> loop, ptr<loop_continue> cont,
                        dfs_stack<node>& stack) {
      for (auto const& dp : cont->variables())
        env.find_variable(dp.variable()).init_expr = dp.expression();
      stack.push_back(clone_ast(ctx, loop->body()));
    }

    bool
    fold_loop_iteration(ptr<loop_body> loop, dfs_stack<node>& stack) {
      auto body = ignore_unitary_sequences(results.back());
      results.pop_back();

      if (auto cont = match<loop_continue>(body)) {
        make_loop_iteration(loop, cont, stack);
        return false;
      } else if (auto lit = match<literal_expression>(body))
        push_loop_result(lit);
      else 
        push_loop_result(loop);

      return true;
    }

    bool
    leave_expression(auto e, node&, dfs_stack<node>&) {
      return leave_expression_for_final_time(e);
    }

    bool
    leave_expression_for_final_time(auto e) {
      e->update(ctx, results);
      results.push_back(combine(e));
      return true;
    }

    void
    update_let_bound_variables(ptr<let_expression> let) {
      env.push_scope();

      auto expressions = results
        | std::views::drop(results.size() - let->definitions().size());

      for (std::size_t i = 0; i < let->definitions().size(); ++i) {
        auto var = let->definitions()[i].variable();
        auto expr = expressions[i];
        auto& info = env.add_variable(var);

        if ((constant_value_for_expression(expr, env)
             || is_constant_propagable_expression(expr))
            && !var->flags().is_set) {
          if (!var->flags().is_loop_variable)
            info.init_expr = constant_initialiser_expression(expr);
          else
            info.loop_init_expr = constant_initialiser_expression(expr);
        }
      }
    }

    expression
    combine(ptr<top_level_reference_expression> ref) {
      auto info = env.find_variable(ref->variable());
      if (info.init_expr && is_constant_propagable_expression(info.init_expr))
        return info.init_expr;
      else
        return ref;
    }

    expression
    combine(ptr<local_reference_expression> ref) {
      auto info = env.find_variable(ref->variable());
      if (info.init_expr && is_constant_propagable_expression(info.init_expr))
        return info.init_expr;
      else
        return ref;
    }

    expression
    combine(ptr<application_expression> app) {
      if (can_be_constant_evaluated(app->arguments(), env))
        if (auto callable = find_constant_evaluable_callable(ctx, app))
          if (auto result = evaluate_constant_application(ctx, callable,
                                                          app->arguments(),
                                                          env))
            return result;
      return app;
    }

    expression
    combine(ptr<built_in_operation_expression> bio) {
      if (bio->procedure()->constant_evaluable
          && can_be_constant_evaluated(bio->operands(), env))
        if (auto result = evaluate_constant_application(ctx, bio->procedure(),
                                                        bio->operands(), env))
          return result;
      return bio;
    }

    expression
    combine(ptr<if_expression> ifexpr) {
      if (auto test_value = constant_value_for_expression(ifexpr->test(), env)) {
        if (test_value == ctx.constants->f)
          return ifexpr->alternative();
        else
          return ifexpr->consequent();
      } else
        return ifexpr;
    }

    expression
    combine(auto e) {
      return e;
    }
  };
}

expression
propagate_and_evaluate_constants(context& ctx, expression e, analysis_context) {
  evaluate_constants_visitor v{ctx};
  depth_first_search(evaluate_constants_visitor::node{e}, v);
  assert(v.env.empty());
  assert(v.results.size() == 1);
  return v.results.back();
}

} // namespace insider
