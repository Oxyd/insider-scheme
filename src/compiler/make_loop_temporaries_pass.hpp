#ifndef INSIDER_COMPILER_MAKE_LOOP_TEMPORARIES_PASS_HPP
#define INSIDER_COMPILER_MAKE_LOOP_TEMPORARIES_PASS_HPP

#include "compiler/analysis_context.hpp"
#include "compiler/expression.hpp"

namespace insider {

class context;

expression
make_loop_temporaries(context&, expression, analysis_context);

} // namespace insider

#endif
