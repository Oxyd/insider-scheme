#ifndef INSIDER_COMPILER_INLINE_PROCEDURES_PASS_HPP
#define INSIDER_COMPILER_INLINE_PROCEDURES_PASS_HPP

#include "compiler/expression.hpp"

namespace insider {

class parsing_context;

expression
inline_procedures(parsing_context& pc, expression expr);

} // namespace insider

#endif
