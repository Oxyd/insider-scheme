#ifndef INSIDER_COMPILER_FIND_SELF_VARIABLES_PASS_HPP
#define INSIDER_COMPILER_FIND_SELF_VARIABLES_PASS_HPP

#include "compiler/expression.hpp"

namespace insider {

class parsing_context;

expression
find_self_variables(parsing_context& pc, expression expr);

} // namespace insider

#endif
