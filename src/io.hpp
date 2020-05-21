#ifndef SCHEME_IO_HPP
#define SCHEME_IO_HPP

#include "scheme.hpp"

#include <stdexcept>
#include <string>
#include <vector>

namespace insider {

struct parse_error : std::runtime_error {
  parse_error(std::string const& message);
};

// Read a single S-expression from the given input stream. Returns a null
// pointer if there is no expression in the stream.
generic_ptr
read(context&, ptr<port> const&);

generic_ptr
read(context&, std::string);

// Read multiple S-expressions until the end of the stream or string.
std::vector<generic_ptr>
read_multiple(context&, ptr<port> const&);

std::vector<generic_ptr>
read_multiple(context&, std::string);

// Write a representation of the given datum to the given output port. This does
// not check for cycles in the datum, so if it is a cyclic data structure, this
// will result in an infinite loop.
void
write_simple(context& ctx, generic_ptr const&, ptr<port> const&);

} // namespace game::csm

#endif
