#ifndef INSIDER_COMPILER_COMPILER_HPP
#define INSIDER_COMPILER_COMPILER_HPP

#include "compiler/compilation_config.hpp"
#include "compiler/expression.hpp"
#include "compiler/source_file_origin.hpp"
#include "module.hpp"
#include "object.hpp"

#include <vector>

namespace insider {

class module_;
class module_specifier;
class procedure;
class syntax;

// Translate a single expression into bytecode. The resulting procedure will
// take no arguments and will return the value of the expression. The module is
// modified by adding a top-level binding if the datum is a top-level
// definition.
ptr<procedure>
compile_expression(context&, ptr<syntax> datum, root_ptr<module_> const&,
                   source_file_origin const&, compilation_config const& config);

ptr<procedure>
compile_syntax(context&, expression, root_ptr<module_> const&);

// Interpret a list of expressions and import declarations as a module and
// create the module.
root_ptr<module_>
compile_module(context&, std::vector<ptr<syntax>> const& data,
               source_file_origin const&,
               compilation_config const& config,
               bool main_module = false);

root_ptr<module_>
compile_module(context&, std::filesystem::path const&,
               compilation_config const& config, bool main_module = false);

// Translate a module's body.
void
compile_module_body(context&, root_ptr<module_> const&,
                    module_specifier const&, compilation_config const& config,
                    bool main_module = false);

} // namespace insider

#endif
