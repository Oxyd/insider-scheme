#include "compiler/ast_transforms.hpp"

#include "compiler/ast.hpp"
#include "context.hpp"
#include "runtime/basic_types.hpp"
#include "vm/vm.hpp"

#include <limits>
#include <ranges>
#include <unordered_map>
#include <unordered_set>

namespace insider {

pass_list const all_passes{
  analyse_variables,
  find_self_variables,
  inline_procedures,
  propagate_and_evaluate_constants,
  remove_unnecessary_procedure_definitions,
  box_set_variables,
  inline_built_in_operations,
  analyse_free_variables
};

pass_list const no_optimisations{
  analyse_variables,
  find_self_variables,
  box_set_variables,
  inline_built_in_operations,
  analyse_free_variables
};

static constexpr std::size_t inline_size_limit = 50;

expression
apply_passes(context& ctx, expression e, analysis_context ac,
             pass_list const& ps) {
  for (pass p : ps)
    e = p(ctx, e, ac);
  return e;
}

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

    void
    leave(expression e) {
      visit(
        [&] (auto e) {
          visit_variables(e);
          leave_expression(e);
        },
        e
      );
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

namespace {
  struct variable_substitutor {
    context&            ctx;
    variable            from;
    ptr<local_variable> to;
    std::size_t         substitutions_count = 0;

    variable_substitutor(context& ctx, variable from, ptr<local_variable> to)
      : ctx{ctx}
      , from{from}
      , to{to}
    { }

    expression
    leave(ptr<local_reference_expression> ref) {
      if (ref->variable() == from) {
        ++substitutions_count;
        return make<local_reference_expression>(ctx, to);
      } else
        return ref;
    }

    expression
    leave(ptr<top_level_reference_expression> ref) {
      if (ref->variable() == from) {
        ++substitutions_count;
        return make<local_reference_expression>(ctx, to);
      } else
        return ref;
    }

    expression
    leave(ptr<local_set_expression> set) const {
      assert(set->target() != from);
      return set;
    }

    expression
    leave(ptr<top_level_set_expression> set) const {
      assert(set->target() != from);
      return set;
    }

    expression
    leave(auto e) const { return e; }

    void
    enter(auto) { }
  };
} // anonymous namespace

static ptr<lambda_expression>
substitute_self_variable(context& ctx, ptr<lambda_expression> lambda,
                         variable var) {
  variable_substitutor s{ctx, var, lambda->self_variable()};
  auto new_lambda = assume<lambda_expression>(transform_ast(ctx, lambda, s));
  new_lambda->set_num_self_references(s.substitutions_count);
  return new_lambda;
}

static expression
find_self_variables(context& ctx, ptr<local_set_expression> set) {
  if (set->target()->flags().is_set_eliminable
      && is<lambda_expression>(set->expression()))
    return make<local_set_expression>(
      ctx, set->target(),
      substitute_self_variable(ctx,
                               assume<lambda_expression>(set->expression()),
                               set->target())
    );
  else
    return set;
}

static expression
find_self_variables(context& ctx, ptr<top_level_set_expression> set) {
  if (set->is_initialisation() && !set->target()->flags().is_set
      && is<lambda_expression>(set->expression()))
    return make<top_level_set_expression>(
      ctx,
      set->target(),
      substitute_self_variable(ctx,
                               assume<lambda_expression>(set->expression()),
                               set->target()),
      true
    );
  else
    return set;
}

static expression
find_self_variables(context&, auto e) { return e; }

expression
find_self_variables(context& ctx, expression expr, analysis_context) {
  return map_ast(ctx, expr,
                 [&] (auto e) { return find_self_variables(ctx, e); });
}

static bool
is_initialised_to_constant(auto var) {
  return var->constant_initialiser()
         && is<literal_expression>(var->constant_initialiser());
}

static expression
propagate_and_evaluate_constants(context&,
                                 ptr<local_reference_expression> ref,
                                 bool&) {
  if (is_initialised_to_constant(ref->variable()))
    return ref->variable()->constant_initialiser();
  else
    return ref;
}

static expression
propagate_and_evaluate_constants(context&,
                                 ptr<top_level_reference_expression> ref,
                                 bool&) {
  if (is_initialised_to_constant(ref->variable()))
    return ref->variable()->constant_initialiser();
  else
    return ref;
}

static bool
is_const(definition_pair_expression const& dp) {
  return is_initialised_to_constant(dp.variable());
}

static ptr<>
constant_value_for_expression(expression e);

static ptr<>
constant_value_for_expression(ptr<literal_expression> lit) {
  return lit->value();
}

static ptr<>
constant_value_for_expression(ptr<local_reference_expression> ref) {
  if (expression k = ref->variable()->constant_initialiser())
    if (auto lit = match<literal_expression>(k))
      return lit->value();
  return {};
}

static ptr<>
constant_value_for_expression(ptr<top_level_reference_expression> ref) {
  if (expression k = ref->variable()->constant_initialiser())
    if (auto lit = match<literal_expression>(k))
      return lit->value();
  return {};
}

static ptr<>
constant_value_for_expression(ptr<sequence_expression> seq) {
  // Technically the correct condition is "all subexpressions are constant",
  // but it's quite unlikely that a sequence with multiple subexpressions will
  // be all-constant.

  if (seq->expressions().size() == 1)
    return constant_value_for_expression(seq->expressions().front());
  else
    return {};
}

static ptr<>
constant_value_for_expression(auto) { return {}; }

static ptr<>
constant_value_for_expression(expression e) {
  return visit([] (auto expr) { return constant_value_for_expression(expr); },
               e);
}

static expression
constant_initialiser_expression(expression e) {
  if (auto lit = match<literal_expression>(e))
    return lit;
  else if (auto seq = match<sequence_expression>(e)) {
    assert(seq->expressions().size() == 1);
    return constant_initialiser_expression(seq->expressions().front());
  } else {
    assert(false);
    return {};
  }
}

static std::vector<definition_pair_expression>
update_let_definitions(context& ctx,
                       std::vector<definition_pair_expression> const& dps,
                       bool& go_again) {
  std::vector<definition_pair_expression> result;
  for (definition_pair_expression dp : dps) {
    // set!-eliminable variable definitions need to be retained because they're
    // still going to be set!.
    if (!is_const(dp)
        || dp.variable()->flags().is_set_eliminable
        || dp.variable()->flags().is_loop_variable)
      result.push_back(dp);

    if (!is_const(dp) && constant_value_for_expression(dp.expression())
        && !dp.variable()->flags().is_set
        && !dp.variable()->flags().is_loop_variable) {
      // The variable was made constant in this pass. We need to mark it as
      // constant and do another pass to propagate it into the body of this let.

      dp.variable()->set_constant_initialiser(
        ctx.store, constant_initialiser_expression(dp.expression())
      );
      go_again = true;
    }
  }

  return result;
}

static expression
propagate_and_evaluate_constants(context& ctx, ptr<let_expression> let,
                                 bool& go_again) {
  auto new_dps = update_let_definitions(ctx, let->definitions(), go_again);
  if (new_dps.empty())
    return let->body();
  else if (new_dps.size() < let->definitions().size())
    return make<let_expression>(ctx, std::move(new_dps), let->body());
  else
    return let;
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
can_be_constant_evaluated(ptr<application_expression> app) {
  return std::ranges::all_of(
    app->arguments(),
    [] (expression e) {
      return visit(
        [] (auto expr) {
          return constant_value_for_expression(expr) != nullptr;
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
                                       ptr<application_expression> app) {
  std::vector<ptr<>> result;
  result.reserve(app->arguments().size());
  for (expression arg : app->arguments())
    result.push_back(constant_value_for_expression(arg));

  coalesce_eqv_values(ctx, result);
  return result;
}

static expression
evaluate_constant_application(context& ctx, ptr<> callable,
                              ptr<application_expression> app) {
  try {
    ptr<> result = call_with_continuation_barrier(
      ctx, callable, make_arguments_for_constant_evaluation(ctx, app)
    );
    return make<literal_expression>(ctx, result);
  } catch (...) {
    return {};
  }
}

static expression
propagate_and_evaluate_constants(context& ctx,
                                 ptr<application_expression> app,
                                 bool&) {
  if (can_be_constant_evaluated(app))
    if (auto callable = find_constant_evaluable_callable(ctx, app))
      if (auto result = evaluate_constant_application(ctx, callable, app))
        return result;
  return app;
}

static expression
propagate_and_evaluate_constants(context& ctx, ptr<if_expression> ifexpr,
                                 bool&) {
  if (auto test_value = constant_value_for_expression(ifexpr->test())) {
    if (test_value == ctx.constants->f)
      return ifexpr->alternative();
    else
      return ifexpr->consequent();
  } else
    return ifexpr;
}

static expression
propagate_and_evaluate_constants(context&, auto x, bool&) { return x; }

expression
propagate_and_evaluate_constants(context& ctx, expression e, analysis_context) {
  bool go_again = false;
  do {
    go_again = false;
    e = map_ast(
      ctx, e,
      [&] (expression expr) {
        return visit(
          [&] (auto e) {
            return propagate_and_evaluate_constants(ctx, e, go_again);
          },
          expr
        );
      }
    );
  } while (go_again);
  return e;
}

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

    void
    leave(expression) { }
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

using variable_map
  = std::unordered_map<ptr<local_variable>, ptr<local_variable>>;

static std::vector<definition_pair_expression>
map_definition_pairs(variable_map const& map,
                     std::vector<definition_pair_expression> const& dps) {
  std::vector<definition_pair_expression> result;
  result.reserve(dps.size());

  for (definition_pair_expression const& dp : dps)
    if (auto mapping = map.find(dp.variable()); mapping != map.end())
      result.emplace_back(mapping->second, dp.expression());
    else
      result.emplace_back(dp);

  return result;
}

static debug_info&
ensure_debug_info(ptr<application_expression> app) {
  if (!app->debug_info())
    app->debug_info().emplace();
  return *app->debug_info();
}

static void
append_procedure_name_to_debug_info(ptr<application_expression> app,
                                    std::string name) {
  ensure_debug_info(app).inlined_call_chain.emplace_back(std::move(name));
}

using loop_map = std::unordered_map<ptr<loop_id>, ptr<loop_id>>;

namespace {
  // Visitor that clones the variables in a part of an AST. Inlining can cause
  // the same subtree to appear multiple times in an AST, but in different
  // contexts. These different contexts may require separate variables.
  struct clone_variables_visitor {
    context&                   ctx;
    variable_map               variables;
    loop_map                   loops;
    std::optional<std::string> procedure_name_to_append;

    explicit
    clone_variables_visitor(context& ctx,
                            std::optional<std::string> procedure_name_to_append)
      : ctx{ctx}
      , procedure_name_to_append{std::move(procedure_name_to_append)}
    { }

    void
    enter(ptr<let_expression> let) {
      for (auto const& dp : let->definitions()) {
        assert(!variables.contains(dp.variable()));
        auto copy = make<local_variable>(ctx, *dp.variable());
        variables.emplace(dp.variable(), copy);
      }
    }

    void
    enter(ptr<lambda_expression> lambda) {
      auto copy = make<local_variable>(ctx, *lambda->self_variable());
      variables.emplace(lambda->self_variable(), copy);
    }

    void
    enter(ptr<loop_body> loop) {
      auto new_id = make<loop_id>(ctx);
      loops.emplace(loop->id(), new_id);
    }

    void
    enter(auto) { }

    expression
    leave(ptr<local_reference_expression> ref) {
      if (auto mapping = variables.find(ref->variable());
          mapping != variables.end())
        return make<local_reference_expression>(ctx, mapping->second);
      else
        return ref;
    }

    expression
    leave(ptr<local_set_expression> set) {
      if (auto mapping = variables.find(set->target());
          mapping != variables.end())
        return make<local_set_expression>(ctx, mapping->second,
                                          set->expression());
      else
        return set;
    }

    expression
    leave(ptr<let_expression> let) {
      return make<let_expression>(
        ctx,
        map_definition_pairs(variables, let->definitions()),
        let->body()
      );
    }

    expression
    leave(ptr<lambda_expression> lambda) {
      auto new_self = variables.find(lambda->self_variable());
      assert(new_self != variables.end());
      return make<lambda_expression>(ctx, lambda, new_self->second);
    }

    expression
    leave(ptr<application_expression> app) {
      if (procedure_name_to_append)
        append_procedure_name_to_debug_info(app, *procedure_name_to_append);
      return app;
    }

    expression
    leave(ptr<loop_continue> cont) {
      std::vector<definition_pair_expression> new_dps;
      new_dps.reserve(cont->variables().size());

      for (definition_pair_expression const& var : cont->variables())
        if (auto mapping = variables.find(var.variable());
            mapping != variables.end())
          new_dps.emplace_back(mapping->second, var.expression());
        else
          new_dps.emplace_back(var);

      return make<loop_continue>(ctx, loops.at(cont->id()), std::move(new_dps));
    }

    expression
    leave(ptr<loop_body> loop) {
      ptr<loop_id> new_id = loops.at(loop->id());
      return make<loop_body>(ctx, loop->body(), new_id);
    }

    expression
    leave(auto e) { return e; }
  };
}

static expression
clone_ast(context& ctx, expression e,
          std::optional<std::string> procedure_name_to_append = {}) {
  return transform_ast_copy(
    ctx, e,
    clone_variables_visitor{ctx,
                            std::move(procedure_name_to_append)}
  );
}

template <typename T>
static ptr<T>
clone_ast(context& ctx, ptr<T> e,
          std::optional<std::string> procedure_name_to_append = {}) {
  return assume<T>(clone_ast(ctx, expression{e},
                             std::move(procedure_name_to_append)));
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

    void
    leave(expression e) {
      visit([&] (auto expr) { leave_expression(expr); }, e);
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

    void
    leave(expression e) {
      visit([&] (auto expr) { leave_expression(expr); }, e);
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
  auto body = make<loop_body>(ctx, lambda->body(), make<loop_id>(ctx));
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

static bool
is_small_enough_to_inline(ptr<lambda_expression> lambda) {
  return size_estimate(lambda->body()) < inline_size_limit;
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

namespace {
  struct built_in_operations_visitor {
    struct operation {
      std::string name;
      opcode      operation;
      std::size_t arity;
      bool        has_result;
    };

    static operation operations[];

    context& ctx;
    std::unordered_map<ptr<top_level_variable>, operation> operations_map;

    explicit
    built_in_operations_visitor(context& ctx);

    void
    enter(auto) { }

    expression
    leave(ptr<application_expression> app) {
      if (auto ref = match<top_level_reference_expression>(app->target()))
        if (auto op = operations_map.find(ref->variable());
            op != operations_map.end())
          if (app->arguments().size() == op->second.arity)
            return substitute_operation(op->second, app);
      return app;
    }

    expression
    leave(auto e) { return e; }

    expression
    substitute_operation(operation const& op, ptr<application_expression> app) {
      return make<built_in_operation_expression>(
        ctx,
        op.operation,
        app->arguments(),
        op.has_result
      );
    }
  };
}

built_in_operations_visitor::operation
built_in_operations_visitor::operations[]{
  {"+", opcode::add, 2, true},
  {"-", opcode::subtract, 2, true},
  {"*", opcode::multiply, 2, true},
  {"/", opcode::divide, 2, true},
  {"=", opcode::arith_equal, 2, true},
  {"<", opcode::less, 2, true},
  {">", opcode::greater, 2, true},
  {"<=", opcode::less_or_equal, 2, true},
  {">=", opcode::greater_or_equal, 2, true},
  {"box", opcode::box, 1, true},
  {"unbox", opcode::unbox, 1, true},
  {"box-set!", opcode::box_set, 2, false},
  {"cons", opcode::cons, 2, true},
  {"car", opcode::car, 1, true},
  {"cdr", opcode::cdr, 1, true},
  {"vector-set!", opcode::vector_set, 3, false},
  {"vector-ref", opcode::vector_ref, 2, true},
  {"type", opcode::type, 1, true},
  {"eq?", opcode::eq, 2, true}
};

built_in_operations_visitor::built_in_operations_visitor(context& ctx)
  : ctx{ctx}
{
  for (operation const& op : operations) {
    auto binding = ctx.internal_module()->find(ctx.intern(op.name));
    operations_map.emplace(assume<top_level_variable>(binding->variable),
                           op);
  }
}

expression
inline_built_in_operations(context& ctx, expression e, analysis_context) {
  return transform_ast(ctx, e, built_in_operations_visitor{ctx});
}

static expression
box_variable_reference(context& ctx,
                       ptr<local_reference_expression> local_ref,
                       ptr<local_variable> var) {
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
                       ptr<local_variable> var) {
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
box_variable_reference(context&, auto e, ptr<local_variable>) {
  return e;
}

static expression
box_variable_references(context& ctx, expression s, ptr<local_variable> var) {
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

static std::vector<ptr<local_variable>>
find_set_variables(ptr<let_expression> let) {
  auto rng = let->definitions()
    | std::views::transform([] (auto const& dp) { return dp.variable(); })
    | std::views::filter([] (ptr<local_variable> v) {
        return v->flags().is_set && !v->flags().is_set_eliminable;
      });
  return std::vector<ptr<local_variable>>(rng.begin(), rng.end());
}

static definition_pair_expression
box_definition_pair(context& ctx, definition_pair_expression const& dp) {
  dp.variable()->flags().is_set = false;
  return {dp.variable(), make_application(ctx, "box", dp.expression())};
}

static std::vector<definition_pair_expression>
box_definition_pairs(context& ctx,
                     std::vector<definition_pair_expression> const& dps) {
  auto rng = dps | std::views::transform(
    [&] (definition_pair_expression const& dp) {
      if (dp.variable()->flags().is_set)
        return box_definition_pair(ctx, dp);
      else
        return dp;
    }
  );
  return std::vector(rng.begin(), rng.end());
}

static expression
box_set_variables(context& ctx, ptr<let_expression> let) {
  std::vector<ptr<local_variable>> set_vars = find_set_variables(let);
  if (!set_vars.empty()) {
    std::vector<definition_pair_expression> dps
      = box_definition_pairs(ctx, let->definitions());
    expression body = let->body();
    for (ptr<local_variable> v : set_vars)
      body = box_variable_references(ctx, body, v);
    return make<let_expression>(ctx, std::move(dps), body);
  } else
    return let;
}

static bool
any_param_set(ptr<lambda_expression> lambda) {
  return std::ranges::any_of(
    lambda->parameters(),
    [] (ptr<local_variable> v) { return v->flags().is_set; }
  );
}

static expression
box_lambda(context& ctx, ptr<lambda_expression> lambda) {
  std::vector<expression> new_body;
  expression body = lambda->body();
  for (ptr<local_variable> param : lambda->parameters())
    if (param->flags().is_set) {
      body = box_variable_references(ctx, body, param);
      new_body.emplace_back(
        make<local_set_expression>(
          ctx, param,
          make_application(
            ctx, "box",
            make<local_reference_expression>(ctx, param)
          )
        )
      );
    }

  new_body.emplace_back(lambda->body());
  return make<lambda_expression>(
    ctx, lambda, make<sequence_expression>(ctx, std::move(new_body))
  );
}

static expression
box_set_variables(context& ctx, ptr<lambda_expression> lambda) {
  if (any_param_set(lambda))
    return box_lambda(ctx, clone_ast(ctx, lambda));
  else
    return lambda;
}

static definition_pair_expression
box_loop_variable(context& ctx, definition_pair_expression const& var) {
  return definition_pair_expression{
    var.variable(),
    make<application_expression>(ctx, make_internal_reference(ctx, "box"),
                                 var.expression())
  };
}

static std::vector<definition_pair_expression>
box_loop_variables(context& ctx, ptr<loop_continue> cont) {
  std::vector<definition_pair_expression> new_vars;
  for (auto const& var : cont->variables())
    if (var.variable()->flags().is_set)
      new_vars.emplace_back(box_loop_variable(ctx, var));
    else
      new_vars.emplace_back(var);
  return new_vars;
}

static expression
box_set_variables(context& ctx, ptr<loop_continue> cont) {
  auto new_vars = box_loop_variables(ctx, cont);
  if (new_vars != cont->variables())
    return make<loop_continue>(ctx, cont->id(), std::move(new_vars));
  else
    return cont;
}

static expression
box_set_variables(context&, auto e) {
  return e;
}

expression
box_set_variables(context& ctx, expression s, analysis_context) {
  return map_ast(
    ctx, s,
    [&] (expression expr) {
      return visit([&] (auto e) { return box_set_variables(ctx, e); },
                   expr);
    }
  );
}

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
      for (ptr<local_variable> param : lambda->parameters())
        bound_vars_stack.back().emplace(param);
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
