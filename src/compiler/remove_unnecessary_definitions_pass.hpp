#ifndef INSIDER_COMPILER_REMOVE_UNNECESSARY_DEFINITIONS_PASS_HPP
#define INSIDER_COMPILER_REMOVE_UNNECESSARY_DEFINITIONS_PASS_HPP

#include "compiler/analysis_context.hpp"
#include "compiler/expression.hpp"

namespace insider {

class context;

expression
remove_unnecessary_definitions(context&, expression, analysis_context);

} // namespace insider

#endif
