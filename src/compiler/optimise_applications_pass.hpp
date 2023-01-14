#ifndef INSIDER_COMPILER_OPTIMISE_APPLICATIONS_PASS_HPP
#define INSIDER_COMPILER_OPTIMISE_APPLICATIONS_PASS_HPP

#include "compiler/expression.hpp"

namespace insider {

class parsing_context;

expression
optimise_applications(parsing_context&, expression);

} // namespace insider

#endif
