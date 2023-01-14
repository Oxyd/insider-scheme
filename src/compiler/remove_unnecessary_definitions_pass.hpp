#ifndef INSIDER_COMPILER_REMOVE_UNNECESSARY_DEFINITIONS_PASS_HPP
#define INSIDER_COMPILER_REMOVE_UNNECESSARY_DEFINITIONS_PASS_HPP

#include "compiler/expression.hpp"

namespace insider {

class parsing_context;

expression
remove_unnecessary_definitions(parsing_context&, expression);

} // namespace insider

#endif
