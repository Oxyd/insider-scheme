#include "compiler/ast_transforms.hpp"

#include "compiler/analyse_free_variables_pass.hpp"
#include "compiler/analyse_variables_pass.hpp"
#include "compiler/ast.hpp"
#include "compiler/box_set_variables_pass.hpp"
#include "compiler/clone_ast.hpp"
#include "compiler/evaluate_constants_pass.hpp"
#include "compiler/find_self_variables_pass.hpp"
#include "compiler/inline_built_in_operations_pass.hpp"
#include "compiler/inline_procedures_pass.hpp"
#include "compiler/remove_unnecessary_procedure_definitions_pass.hpp"

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

expression
apply_passes(context& ctx, expression e, analysis_context ac,
             pass_list const& ps) {
  for (pass p : ps)
    e = p(ctx, e, ac);
  return e;
}

} // namespace insider
