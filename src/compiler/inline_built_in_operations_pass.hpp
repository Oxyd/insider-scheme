#ifndef INSIDER_COMPILER_INLINE_BUILT_IN_OPERATIONS_PASS_HPP
#define INSIDER_COMPILER_INLINE_BUILT_IN_OPERATIONS_PASS_HPP

#include "compiler/expression.hpp"

namespace insider {

class parsing_context;

expression
inline_built_in_operations(parsing_context&, expression);

} // namespace insider

#endif
