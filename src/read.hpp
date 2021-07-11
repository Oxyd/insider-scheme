#ifndef INSIDER_READ_HPP
#define INSIDER_READ_HPP

#include "ptr.hpp"

#include <stdexcept>
#include <string>

namespace insider {

class context;
class port;
class source_location;
class syntax;

struct read_error : std::runtime_error {
  read_error(std::string const& message, source_location const&);
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
std::vector<tracked_ptr<>>
read_multiple(context&, ptr<port>);

std::vector<tracked_ptr<>>
read_multiple(context&, std::string);

std::vector<tracked_ptr<syntax>>
read_syntax_multiple(context&, ptr<port>);

std::vector<tracked_ptr<syntax>>
read_syntax_multiple(context&, std::string);

} // namespace insider

#endif
