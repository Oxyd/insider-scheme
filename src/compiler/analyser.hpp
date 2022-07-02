#ifndef INSIDER_COMPILER_ANALYSER_HPP
#define INSIDER_COMPILER_ANALYSER_HPP

#include "compiler/module_specifier.hpp"
#include "expression.hpp"
#include "source_file_origin.hpp"

#include <memory>
#include <optional>
#include <string>
#include <variant>
#include <vector>

namespace insider {

class module_;
class module_specifier;
class parsing_context;
class textual_input_port;

// The analyser expects a Scheme datum (or a list of data) that represents a
// program, and turns it into an internal representation, defined in
// expression.hpp. The actual parsing is performed by the parser/expander,
// this module is responsible for additional transformations such as boxing
// set! variables.

// Analyse a datum within a given module. The module provides the top-level
// bindings visible to S-expression. The module is modified by adding a new
// top-level binding if the datum is a top-level definition. The input syntax is
// modified by updating the scope sets of it and all the syntaxes it recursively
// contains.
expression
analyse(context&, ptr<syntax> stx, tracked_ptr<module_> const&,
        source_file_origin const&);

module_name
parse_module_name(context&, ptr<syntax>);

import_specifier
parse_import_specifier(context& ctx, ptr<syntax> stx);

// Interpret the given list of data as a module. This can be a library module,
// or main module.
module_specifier
read_module(context&, std::vector<ptr<syntax>> const& contents,
            source_file_origin const&);

std::optional<module_name>
read_library_name(context&, ptr<textual_input_port>);

// Analyse a module's body in the given module.
expression
analyse_module(context&, tracked_ptr<module_> const&, module_specifier const&,
               bool main_module = false);

expression
analyse_transformer(parsing_context& pc, ptr<syntax> stx);

expression
analyse_meta(parsing_context& pc, ptr<syntax> stx);

} // namespace insider

#endif
