#include "compiler/update_variables_pass.hpp"

#include "compiler/ast.hpp"

namespace insider {

void
clear_read_flag(ptr<let_expression> let) {
  for (definition_pair_expression const& dp : let->definitions())
    dp.variable()->flags().is_read = false;
}

void
clear_read_flag(auto) { }

void
mark_read_variables(ptr<local_reference_expression> ref) {
  ref->variable()->flags().is_read = true;
}

void
mark_read_variables(auto) { }

expression
update_variables(parsing_context&, expression e) {
  traverse_postorder(e, [] (auto expr) { clear_read_flag(expr); });
  traverse_postorder(e, [] (auto expr) { mark_read_variables(expr); });
  return e;
}

} // namespace insider
