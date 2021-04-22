#ifndef INSIDER_IO_HPP
#define INSIDER_IO_HPP

#include "scheme.hpp"

#include <stdexcept>
#include <string>
#include <vector>

namespace insider {

struct parse_error : std::runtime_error {
  parse_error(std::string const& message, source_location const&);
};

// Read a single S-expression from the given input stream. Returns a null
// pointer if there is no expression in the stream.
ptr<>
read(context&, ptr<port>);

ptr<>
read(context&, std::string);

ptr<syntax>
read_syntax(context&, ptr<port>);

ptr<syntax>
read_syntax(context&, std::string);

// Read multiple S-expressions until the end of the stream or string.
std::vector<generic_tracked_ptr>
read_multiple(context&, ptr<port>);

std::vector<generic_tracked_ptr>
read_multiple(context&, std::string);

std::vector<tracked_ptr<syntax>>
read_syntax_multiple(context&, ptr<port>);

std::vector<tracked_ptr<syntax>>
read_syntax_multiple(context&, std::string);

// Write a representation of the given datum to the given output port. This does
// not check for cycles in the datum, so if it is a cyclic data structure, this
// will result in an infinite loop.
void
write_simple(context& ctx, ptr<>, ptr<port>);

void
display(context& ctx, ptr<>, ptr<port>);

std::string
datum_to_string(context& ctx, ptr<>);

} // namespace game::csm

#endif
