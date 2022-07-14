#ifndef INSIDER_COMPILER_AST_TRANSFORMS_HPP
#define INSIDER_COMPILER_AST_TRANSFORMS_HPP

#include "compiler/expression.hpp"

namespace insider {

class context;

void
analyse_variables(expression expr);

expression
box_set_variables(context& ctx, expression s);

expression
propagate_constants(context&, expression);

void
analyse_free_variables(context& ctx, expression& e);

} // namespace insider

#endif
