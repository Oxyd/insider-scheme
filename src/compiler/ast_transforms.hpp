#ifndef INSIDER_COMPILER_AST_TRANSFORMS_HPP
#define INSIDER_COMPILER_AST_TRANSFORMS_HPP

namespace insider {

class context;
class expression;

void
box_set_variables(context& ctx, expression* s);

void
analyse_free_variables(context& ctx, expression* e);

} // namespace insider

#endif
