#ifndef SCHEME_COMPILER_HPP
#define SCHEME_COMPILER_HPP

#include "bytecode.hpp"
#include "scheme.hpp"

namespace game::scm {

// Translate a single expression into bytecode. The resulting procedure will
// take no arguments and will return the value of the expression.
ptr<procedure>
compile_expression(context&, generic_ptr const& datum, ptr<module> const&);

} // namespace game::scm

#endif
