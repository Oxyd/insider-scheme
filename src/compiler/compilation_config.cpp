#include "compiler/compilation_config.hpp"

#include "compiler/ast_transforms.hpp"
#include "compiler/source_location.hpp"

#include <fmt/format.h>
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
  fmt::print(stdout, "Warning: {}: {}\n", format_location(loc), message);
}

delegate_diagnostic_sink::delegate_diagnostic_sink(diagnostic_sink& other)
  : target_{other}
{ }

void
delegate_diagnostic_sink::output(source_location const& loc,
                                 std::string const& msg) {
  target_.show(loc, msg);
}

compilation_config
compilation_config::optimisations_config(
  std::unique_ptr<diagnostic_sink> diagnostics
) {
  return compilation_config{all_passes, std::move(diagnostics)};
}

compilation_config
compilation_config::debug_config(std::unique_ptr<diagnostic_sink> diagnostics) {
  return compilation_config{no_optimisations, std::move(diagnostics)};
}

} // namespace insider
