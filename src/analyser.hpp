#ifndef INSIDER_ANALYSER_HPP
#define INSIDER_ANALYSER_HPP

#include "scheme.hpp"
#include "syntax.hpp"

#include <memory>
#include <optional>
#include <string>
#include <variant>
#include <vector>

namespace insider {

// The analyser expects a Scheme datum (or a list of data) that represents a
// program, and turns it into an internal representation, defined in syntax.hpp.

// Analyse a datum within a given module. The module provides the top-level
// bindings visible to S-expression. The module is modified by adding a new
// top-level binding if the datum is a top-level definition.
std::unique_ptr<syntax>
analyse(context&, generic_ptr const& datum, module&);

// Interpret the given list of data as a main (program) module.
protomodule
read_main_module(context&, std::vector<generic_ptr> const& data);

// Interpret the given list of data as a library.
protomodule
read_library(context&, std::vector<generic_ptr> const& data);

std::optional<module_name>
read_library_name(context&, ptr<port> const&);

// Analyse a protomodule's body in the given module.
sequence_syntax
analyse_module(context&, module&, protomodule const&);

} // namespace insider

#endif
