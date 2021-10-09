#ifndef INSIDER_COMPILER_HPP
#define INSIDER_COMPILER_HPP

#include "bytecode.hpp"
#include "module.hpp"
#include "ptr.hpp"
#include "source_file_origin.hpp"

#include <vector>

namespace insider {

class expression;
class module_;
class procedure;
class syntax;
struct protomodule;

// Translate a single expression into bytecode. The resulting procedure will
// take no arguments and will return the value of the expression. The module is
// modified by adding a top-level binding if the datum is a top-level
// definition.
ptr<procedure>
compile_expression(context&, ptr<syntax> datum, module_&, source_file_origin const&);

ptr<procedure>
compile_syntax(context&, std::unique_ptr<expression>, module_&);

// Interpret a list of expressions and import declarations as a main module and
// create the module.
module_
compile_main_module(context&, std::vector<tracked_ptr<syntax>> const& data, source_file_origin const&);

module_
compile_main_module(context&, std::filesystem::path const&);

// Translate a protomodule's body.
void
compile_module_body(context&, module_&, protomodule const&);

} // namespace insider

#endif
