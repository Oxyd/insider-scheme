#ifndef SCHEME_IO_HPP
#define SCHEME_IO_HPP

#include "scheme.hpp"

#include <istream>
#include <stdexcept>
#include <string>
#include <vector>

namespace scm {

struct parse_error : std::runtime_error {
  parse_error(std::string const& message);
};

// Read a single S-expression from the given input stream. Returns a null
// pointer if there is no expression in the stream.
generic_ptr
read(context&, std::istream&);

generic_ptr
read(context&, std::string const&);

// Read multiple S-expressions until the end of the stream or string.
std::vector<generic_ptr>
read_multiple(context&, std::istream&);

std::vector<generic_ptr>
read_multiple(context&, std::string const&);

} // namespace game::csm

#endif
