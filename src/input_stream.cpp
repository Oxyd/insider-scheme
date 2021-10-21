#include "input_stream.hpp"

#include "port.hpp"

#include <fmt/format.h>

namespace insider {

input_stream::input_stream(ptr<textual_input_port> p)
  : port_{p}
{ }

std::optional<char32_t>
input_stream::peek_character() {
  return port_->peek_character();
}

std::optional<char32_t>
input_stream::read_character() {
  if (auto c = port_->read_character()) {
    if (*c == '\n') {
      ++line_;
      column_ = 1;
    } else
      ++column_;

    return c;
  } else
    return std::nullopt;
}

void
input_stream::put_back(char32_t c) {
  if (c == '\n') {
    assert(line_ > 1);
    --line_;
    column_ = 1;
  } else {
    assert(column_ > 1);
    --column_;
  }

  port_->put_back(c);
}

std::optional<char32_t>
input_stream::advance_and_peek_character() {
  read_character();
  return peek_character();
}

source_location
input_stream::current_location() const {
  return source_location{port_->name(), line_, column_};
}

} // namespace insider
