#ifndef INSIDER_COMPILER_ANALYSE_VARIABLES_PASS_HPP
#define INSIDER_COMPILER_ANALYSE_VARIABLES_PASS_HPP

#include "compiler/analysis_context.hpp"
#include "compiler/expression.hpp"

namespace insider {

class context;

expression
analyse_variables(context& ctx, expression expr, analysis_context ac);

} // namespace insider

#endif
