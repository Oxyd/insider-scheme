#ifndef INSIDER_INPUT_STREAM_HPP
#define INSIDER_INPUT_STREAM_HPP

#include "ptr.hpp"
#include "source_location.hpp"

#include <optional>
#include <string>

namespace insider {

class port;

class input_stream {
public:
  explicit
  input_stream(insider::ptr<port>);

  std::optional<char>
  peek_char();

  std::optional<char>
  read_char();

  void
  put_back(char);

  std::optional<char>
  advance_and_peek_char();

  source_location
  current_location() const;

private:
  insider::ptr<port> port_;
  unsigned           line_   = 1;
  unsigned           column_ = 1;
};

} // namespace insider

#endif
