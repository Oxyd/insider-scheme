#ifndef SCHEME_COMPILER_HPP
#define SCHEME_COMPILER_HPP

#include "bytecode.hpp"
#include "scheme.hpp"

namespace insider {

// Translate a single expression into bytecode. The resulting procedure will
// take no arguments and will return the value of the expression. The module is
// modified by adding a top-level binding if the datum is a top-level
// definition.
ptr<procedure>
compile_expression(context&, generic_ptr const& datum, module&);

// Interpret a list of expressions and import declarations as a main module and
// create the module.
module
compile_main_module(context&, std::vector<generic_ptr> const& data);

// Translate a protomodule's body.
void
compile_module_body(context&, module&, protomodule const&);

} // namespace insider

#endif
