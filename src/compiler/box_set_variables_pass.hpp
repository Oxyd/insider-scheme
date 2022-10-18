#ifndef INSIDER_COMPILER_BOX_SET_VARIABLES_PASS_HPP
#define INSIDER_COMPILER_BOX_SET_VARIABLES_PASS_HPP

#include "compiler/analysis_context.hpp"
#include "compiler/expression.hpp"

namespace insider {

class context;

expression
box_set_variables(context&, expression, analysis_context);

} // namespace insider

#endif
