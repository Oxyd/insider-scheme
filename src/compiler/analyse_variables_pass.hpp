#ifndef INSIDER_COMPILER_ANALYSE_VARIABLES_PASS_HPP
#define INSIDER_COMPILER_ANALYSE_VARIABLES_PASS_HPP

#include "compiler/expression.hpp"

namespace insider {

class parsing_context;

expression
analyse_variables(parsing_context& pc, expression expr);

} // namespace insider

#endif
