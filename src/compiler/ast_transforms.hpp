#ifndef INSIDER_COMPILER_AST_TRANSFORMS_HPP
#define INSIDER_COMPILER_AST_TRANSFORMS_HPP

#include "compiler/expression.hpp"

namespace insider {

class context;

expression
box_set_variables(context& ctx, expression s);

void
analyse_free_variables(context& ctx, expression& e);

} // namespace insider

#endif
