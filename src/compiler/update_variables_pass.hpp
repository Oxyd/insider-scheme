#ifndef INSIDER_COMPILER_UPDATE_VARIABLES_PASS_HPP
#define INSIDER_COMPILER_UPDATE_VARIABLES_PASS_HPP

#include "compiler/analysis_context.hpp"
#include "compiler/expression.hpp"

namespace insider {

class context;

expression
update_variables(context&, expression, analysis_context);

}

#endif
