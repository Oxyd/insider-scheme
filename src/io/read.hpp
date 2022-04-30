#ifndef INSIDER_IO_READ_HPP
#define INSIDER_IO_READ_HPP

#include "runtime/error.hpp"
#include "ptr.hpp"

#include <stdexcept>
#include <string>

namespace insider {

class context;
class module_;
struct source_location;
class syntax;
class textual_input_port;

class read_error : public translatable_runtime_error {
public:
  class scheme_error : public leaf_object<scheme_error> {
  public:
    static constexpr char const* scheme_name = "insider::read_error::scheme_error";

    explicit
    scheme_error(std::string msg) : message_{std::move(msg)} { }

    std::string
    message() const { return message_; }

  private:
    std::string message_;
  };

  read_error(std::string const& message, source_location const&);

  ptr<>
  translate(context&) const override;
};

// Read a single S-expression from the given input stream. Returns a null
// pointer if there is no expression in the stream.
ptr<>
read(context&, ptr<textual_input_port>);

ptr<>
read(context&, std::string);

ptr<syntax>
read_syntax(context&, ptr<textual_input_port>);

ptr<syntax>
read_syntax(context&, std::string);

// Read multiple S-expressions until the end of the stream or string.
std::vector<ptr<>>
read_multiple(context&, ptr<textual_input_port>);

std::vector<ptr<>>
read_multiple(context&, std::string);

std::vector<ptr<syntax>>
read_syntax_multiple(context&, ptr<textual_input_port>);

std::vector<ptr<syntax>>
read_syntax_multiple_ci(context&, ptr<textual_input_port>);

std::vector<ptr<syntax>>
read_syntax_multiple(context&, std::string);

ptr<>
string_to_number(context&, std::string const&, unsigned base = 10);

void
init_read(context&);

} // namespace insider

#endif
