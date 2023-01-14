#ifndef INSIDER_COMPILER_BOX_SET_VARIABLES_PASS_HPP
#define INSIDER_COMPILER_BOX_SET_VARIABLES_PASS_HPP

#include "compiler/expression.hpp"

namespace insider {

class parsing_context;

expression
box_set_variables(parsing_context&, expression);

} // namespace insider

#endif
