#ifndef INSIDER_COMPILER_AST_TRANSFORMS_HPP
#define INSIDER_COMPILER_AST_TRANSFORMS_HPP

#include "compiler/analysis_context.hpp"
#include "compiler/expression.hpp"

#include <vector>

namespace insider {

class context;

using pass = expression (*)(context&, expression, analysis_context);
using pass_list = std::vector<pass>;

extern pass_list const all_passes;

expression
apply_passes(context&, expression, analysis_context, pass_list const&);

expression
analyse_variables(context&, expression, analysis_context);

expression
propagate_constants(context&, expression, analysis_context);

expression
inline_procedures(context&, expression, analysis_context);

expression
box_set_variables(context&, expression, analysis_context);

expression
analyse_free_variables(context&, expression, analysis_context);

} // namespace insider

#endif
