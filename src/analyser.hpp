#ifndef SCHEME_ANALYSER_HPP
#define SCHEME_ANALYSER_HPP

#include "scheme.hpp"
#include "syntax.hpp"

#include <memory>
#include <optional>
#include <string>
#include <variant>
#include <vector>

namespace scm {

// The analyser expects a Scheme datum (or a list of data) that represents a
// program, and turns it into an internal representation, defined in syntax.hpp.

// Analyse a datum within a given module. The module provides the top-level
// bindings visible to S-expression. The module is modified by adding a new
// top-level binding if the datum is a top-level definition.
std::unique_ptr<syntax>
analyse(context&, generic_ptr const& datum, module&);

// Interpret the given list of data as a main (program) module.
protomodule
read_main_module(std::vector<generic_ptr> const& data);

// Interpret the given list of data as a library.
protomodule
read_library(std::vector<generic_ptr> const& data);

std::optional<module_name>
read_library_name(context&, ptr<port> const&);

// Analyse a list of data that forms a module body.
sequence_syntax
analyse_module(context&, module&, std::vector<generic_ptr> const& data);

} // namespace scm

#endif
