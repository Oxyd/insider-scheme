#include "source_location.hpp"

#include <fmt/format.h>

namespace insider {

std::string
format_location(source_location const& loc) {
  return fmt::format("{}:{}:{}",
                     loc.file_name.empty() ? "<unknown>" : loc.file_name,
                     loc.line, loc.column);
}

} // namespace insider
