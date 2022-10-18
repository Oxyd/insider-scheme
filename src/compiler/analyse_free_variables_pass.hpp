#ifndef INSIDER_COMPILER_ANALYSE_FREE_VARIABLES_PASS_HPP
#define INSIDER_COMPILER_ANALYSE_FREE_VARIABLES_PASS_HPP

#include "compiler/analysis_context.hpp"
#include "compiler/expression.hpp"

namespace insider {

class context;

expression
analyse_free_variables(context&, expression, analysis_context);

} // namespace insider

#endif
