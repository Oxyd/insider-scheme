#include "compiler/clone_ast.hpp"

#include "compiler/ast.hpp"
#include "compiler/variable.hpp"
#include "context.hpp"

namespace insider {

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
      for (auto const& dp : let->definitions())
        copy_variable(dp.variable());
    }

    void
    enter(ptr<lambda_expression> lambda) {
      copy_variable(lambda->self_variable());
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
      return update_local_reference(ref);
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

      auto id = cont->id();
      if (auto mapped_id = loops.find(id); mapped_id != loops.end())
        id = mapped_id->second;

      return make<loop_continue>(ctx, id, std::move(new_dps));
    }

    expression
    leave(ptr<loop_body> loop) {
      ptr<loop_id> new_id = loops.at(loop->id());
      return make<loop_body>(ctx, loop->body(), new_id,
                             map_variables(loop->variables()));
    }

    expression
    leave(auto e) { return e; }

    void
    copy_variable(ptr<local_variable> var) {
      assert(!variables.contains(var));
      auto copy = make<local_variable>(ctx, *var);
      update_variable_initialiser(copy);
      variables.emplace(var, copy);
    }

    void
    update_variable_initialiser(ptr<local_variable> var) {
      if (expression init = var->constant_initialiser())
        if (auto ref = match<local_reference_expression>(init))
          var->set_constant_initialiser(ctx.store, update_local_reference(ref));
    }

    ptr<local_reference_expression>
    update_local_reference(ptr<local_reference_expression> ref) {
      if (auto mapping = variables.find(ref->variable());
          mapping != variables.end())
        return make<local_reference_expression>(ctx, mapping->second);
      else
        return ref;
    }

    std::vector<ptr<local_variable>>
    map_variables(std::vector<ptr<local_variable>> const& vars) const {
      std::vector<ptr<local_variable>> result;
      result.reserve(vars.size());

      for (ptr<local_variable> v : vars)
        if (auto mapping = variables.find(v); mapping != variables.end())
          result.push_back(mapping->second);
        else
          result.push_back(v);

      return result;
    }
  };
}

expression
clone_ast(context& ctx, expression e,
          std::optional<std::string> procedure_name_to_append) {
  return transform_ast_copy(
    ctx, e,
    clone_variables_visitor{ctx,
                            std::move(procedure_name_to_append)}
  );
}

} // namespace insider
