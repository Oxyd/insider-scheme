#include "input_stream.hpp"

#include "port.hpp"

#include <fmt/format.h>

namespace insider {

input_stream::input_stream(insider::ptr<textual_input_port> p)
  : port_{p}
{ }

std::optional<character>
input_stream::peek_char() {
  return port_->peek_character();
}

std::optional<character>
input_stream::read_char() {
  if (auto c = port_->read_character()) {
    if (c->value() == '\n') {
      ++line_;
      column_ = 1;
    } else
      ++column_;

    return c;
  } else
    return std::nullopt;
}

void
input_stream::put_back(character c) {
  if (c.value() == '\n') {
    assert(line_ > 1);
    --line_;
    column_ = 1;
  } else {
    assert(column_ > 1);
    --column_;
  }

  port_->put_back(c);
}

std::optional<character>
input_stream::advance_and_peek_char() {
  read_char();
  return peek_char();
}

source_location
input_stream::current_location() const {
  return source_location{port_->name(), line_, column_};
}

} // namespace insider
