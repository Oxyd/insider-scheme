#ifndef INSIDER_COMPILER_EVALUATE_CONSTANTS_PASS_HPP
#define INSIDER_COMPILER_EVALUATE_CONSTANTS_PASS_HPP

#include "compiler/analysis_context.hpp"
#include "compiler/expression.hpp"

namespace insider {

class context;

expression
propagate_and_evaluate_constants(context&, expression, analysis_context);

} // namespace insider

#endif
