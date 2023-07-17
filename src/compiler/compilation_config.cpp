#include "compiler/compilation_config.hpp"

#include "compiler/ast_transforms.hpp"
#include "compiler/source_location.hpp"

#include <format>
#include <iostream>
#include <memory>

namespace insider {

void
diagnostic_sink::show(source_location const& loc, std::string const& message) {
  if (!emitted_locations_.contains(loc)) {
    output(loc, message);
    emitted_locations_.emplace(loc);
  }
}

void
stdout_diagnostic_sink::output(source_location const& loc,
                               std::string const& message) {
  std::cout << std::format("Warning: {}: {}\n", format_location(loc), message);
}

null_diagnostic_sink
null_diagnostic_sink::instance;

compilation_config
compilation_config::optimisations_config(diagnostic_sink& diagnostics) {
  return compilation_config{all_passes, diagnostics};
}

compilation_config
compilation_config::debug_config(diagnostic_sink& diagnostics) {
  return compilation_config{no_optimisations, diagnostics};
}

} // namespace insider
