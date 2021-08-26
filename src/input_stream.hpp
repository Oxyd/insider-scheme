#ifndef INSIDER_INPUT_STREAM_HPP
#define INSIDER_INPUT_STREAM_HPP

#include "character.hpp"
#include "ptr.hpp"
#include "source_location.hpp"

#include <optional>
#include <string>

namespace insider {

class textual_input_port;

class input_stream {
public:
  explicit
  input_stream(insider::ptr<textual_input_port>);

  std::optional<character>
  peek_char();

  std::optional<character>
  read_char();

  void
  put_back(character);

  std::optional<character>
  advance_and_peek_char();

  source_location
  current_location() const;

private:
  insider::ptr<textual_input_port> port_;
  unsigned                         line_   = 1;
  unsigned                         column_ = 1;
};

} // namespace insider

#endif
