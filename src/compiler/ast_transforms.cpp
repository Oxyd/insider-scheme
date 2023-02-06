#include "compiler/ast_transforms.hpp"

#include "compiler/analyse_free_variables_pass.hpp"
#include "compiler/analyse_variables_pass.hpp"
#include "compiler/box_set_variables_pass.hpp"
#include "compiler/evaluate_constants_pass.hpp"
#include "compiler/find_self_variables_pass.hpp"
#include "compiler/inline_built_in_operations_pass.hpp"
#include "compiler/inline_procedures_pass.hpp"
#include "compiler/make_loop_temporaries_pass.hpp"
#include "compiler/optimise_applications_pass.hpp"
#include "compiler/parsing_context.hpp"
#include "compiler/remove_unnecessary_definitions_pass.hpp"
#include "compiler/update_variables_pass.hpp"
#include "context.hpp"
#include "memory/preserve.hpp"

namespace insider {

pass_list const all_passes{
  analyse_variables,
  find_self_variables,
  inline_procedures,
  evaluate_constants,
  update_variables,
  remove_unnecessary_definitions,
  make_loop_temporaries,
  box_set_variables,
  optimise_applications,
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
apply_passes(parsing_context& pc, expression e) {
  auto p = preserve(pc.ctx, e);

  for (pass p : pc.config.passes) {
    e = p(pc, e);
    pc.ctx.store.update();
  }
  return e;
}

} // namespace insider
