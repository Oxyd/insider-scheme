#ifndef INSIDER_COMPILER_INLINE_PROCEDURES_PASS_HPP
#define INSIDER_COMPILER_INLINE_PROCEDURES_PASS_HPP

#include "compiler/analysis_context.hpp"
#include "compiler/expression.hpp"

namespace insider {

class context;

expression
inline_procedures(context& ctx, expression expr, analysis_context ac);

} // namespace insider

#endif
