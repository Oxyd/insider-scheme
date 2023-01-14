#ifndef INSIDER_COMPILER_UPDATE_VARIABLES_PASS_HPP
#define INSIDER_COMPILER_UPDATE_VARIABLES_PASS_HPP

#include "compiler/expression.hpp"

namespace insider {

class parsing_context;

expression
update_variables(parsing_context&, expression);

}

#endif
