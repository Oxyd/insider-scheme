#ifndef INSIDER_COMPILER_OPTIMISE_APPLICATIONS_PASS_HPP
#define INSIDER_COMPILER_OPTIMISE_APPLICATIONS_PASS_HPP

#include "compiler/analysis_context.hpp"
#include "compiler/expression.hpp"

namespace insider {

class context;

expression
optimise_applications(context&, expression, analysis_context);

} // namespace insider

#endif
