#include "compiler/find_self_variables_pass.hpp"

#include "compiler/ast.hpp"
#include "compiler/variable.hpp"
#include "context.hpp"

namespace insider {

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

} // namespace insider
