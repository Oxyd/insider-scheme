#ifndef INSIDER_WRITE_HPP
#define INSIDER_WRITE_HPP

#include "ptr.hpp"

#include <stdexcept>
#include <string>
#include <vector>

namespace insider {

class context;
class port;
class source_location;
class syntax;

// Write a representation of the given datum to the given output port. This will
// use datum labels if and only if the datum contains a cyclic data structure.
void
write(context&, ptr<>, ptr<port>);

// Write a representation of the given datum to the given output port. This does
// not check for cycles in the datum, so if it is a cyclic data structure, this
// will result in an infinite loop.
void
write_simple(context& ctx, ptr<>, ptr<port>);

// Write a representation of the given datum to the given output port. This will
// use datum labels for all pairs and vectors that appear more than once in the
// output.
void
write_shared(context&, ptr<>, ptr<port>);

void
display(context& ctx, ptr<>, ptr<port>);

std::string
datum_to_string(context& ctx, ptr<>);

} // namespace game::csm

#endif
