#ifndef INSIDER_COMPILER_ANALYSER_HPP
#define INSIDER_COMPILER_ANALYSER_HPP

#include "expression.hpp"
#include "source_file_origin.hpp"

#include <memory>
#include <optional>
#include <string>
#include <variant>
#include <vector>

namespace insider {

class textual_input_port;
class module_;

// The analyser expects a Scheme datum (or a list of data) that represents a
// program, and turns it into an internal representation, defined in expression.hpp.

// Analyse a datum within a given module. The module provides the top-level
// bindings visible to S-expression. The module is modified by adding a new
// top-level binding if the datum is a top-level definition. The input syntax is
// modified by updating the scope sets of it and all the syntaxes it recursively
// contains.
std::unique_ptr<expression>
analyse(context&, ptr<syntax> stx, module_&, source_file_origin const&);

module_name
parse_module_name(context&, ptr<syntax>);

// Interpret the given list of data as a module. This can be a library module, or main module.
protomodule
read_module(context&, std::vector<ptr<syntax>> const& contents,
            source_file_origin const&);

std::optional<module_name>
read_library_name(context&, ptr<textual_input_port>);

// Analyse a protomodule's body in the given module.
sequence_expression
analyse_module(context&, module_&, protomodule const&, bool main_module = false);

void
init_analyser(context&);

} // namespace insider

#endif