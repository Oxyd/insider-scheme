#ifndef INSIDER_COMPILER_ANALYSE_FREE_VARIABLES_PASS_HPP
#define INSIDER_COMPILER_ANALYSE_FREE_VARIABLES_PASS_HPP

#include "compiler/expression.hpp"

namespace insider {

class parsing_context;

expression
analyse_free_variables(parsing_context&, expression);

} // namespace insider

#endif
