#ifndef INSIDER_COMPILER_AST_TRANSFORMS_HPP
#define INSIDER_COMPILER_AST_TRANSFORMS_HPP

#include "compiler/expression.hpp"

#include <vector>

namespace insider {

class context;

using pass = expression (*)(context&, expression);
using pass_list = std::vector<pass>;

extern pass_list const all_passes;

expression
apply_passes(context&, expression, pass_list const&);

expression
analyse_variables(context&, expression expr);

expression
box_set_variables(context& ctx, expression s);

expression
propagate_constants(context&, expression);

expression
analyse_free_variables(context& ctx, expression e);

} // namespace insider

#endif
