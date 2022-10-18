#ifndef INSIDER_COMPILER_INLINE_BUILT_IN_OPERATIONS_PASS_HPP
#define INSIDER_COMPILER_INLINE_BUILT_IN_OPERATIONS_PASS_HPP

#include "compiler/analysis_context.hpp"
#include "compiler/expression.hpp"

namespace insider {

class context;

expression
inline_built_in_operations(context&, expression, analysis_context);

} // namespace insider

#endif
