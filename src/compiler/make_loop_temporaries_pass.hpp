#ifndef INSIDER_COMPILER_MAKE_LOOP_TEMPORARIES_PASS_HPP
#define INSIDER_COMPILER_MAKE_LOOP_TEMPORARIES_PASS_HPP

#include "compiler/expression.hpp"

namespace insider {

class parsing_context;

expression
make_loop_temporaries(parsing_context&, expression);

} // namespace insider

#endif
