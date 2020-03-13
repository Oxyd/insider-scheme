#ifndef SCHEME_COMPILER_HPP
#define SCHEME_COMPILER_HPP

#include "bytecode.hpp"
#include "scheme.hpp"

namespace scm {

// Translate a single expression into bytecode. The resulting procedure will
// take no arguments and will return the value of the expression. The module is
// modified by adding a top-level binding if the datum is a top-level
// definition.
ptr<procedure>
compile_expression(context&, generic_ptr const& datum, module&);

// Translate a list of expressions into a module.
module
compile_module(context&, std::vector<generic_ptr> const& data);

} // namespace scm

#endif
