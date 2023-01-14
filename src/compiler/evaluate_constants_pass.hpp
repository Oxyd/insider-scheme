#ifndef INSIDER_COMPILER_EVALUATE_CONSTANTS_PASS_HPP
#define INSIDER_COMPILER_EVALUATE_CONSTANTS_PASS_HPP

#include "compiler/expression.hpp"

namespace insider {

class parsing_context;

expression
evaluate_constants(parsing_context&, expression);

} // namespace insider

#endif
