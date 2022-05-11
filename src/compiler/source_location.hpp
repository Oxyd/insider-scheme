#ifndef INSIDER_COMPILER_SOURCE_LOCATION_HPP
#define INSIDER_COMPILER_SOURCE_LOCATION_HPP

#include <string>

namespace insider {

struct source_location {
  static source_location const unknown;

  std::string file_name;
  unsigned    line;
  unsigned    column;

  friend bool
  operator == (source_location const&, source_location const&) = default;
};

inline source_location const source_location::unknown{"<unknown>", 0, 0};

std::string
format_location(source_location const&);

} // namespace insider

#endif
