#ifndef INSIDER_INPUT_STREAM_HPP
#define INSIDER_INPUT_STREAM_HPP

#include "ptr.hpp"
#include "source_location.hpp"

#include <optional>
#include <string>

namespace insider {

class textual_input_port;

class input_stream {
public:
  explicit
  input_stream(ptr<textual_input_port>);

  std::optional<char32_t>
  peek_character();

  std::optional<char32_t>
  read_character();

  void
  put_back(char32_t);

  std::optional<char32_t>
  advance_and_peek_character();

  source_location
  current_location() const;

  bool
  fold_case() const { return fold_case_; }

  void
  enable_fold_case() { fold_case_ = true; }

  void
  disable_fold_case() { fold_case_ = false; }

private:
  ptr<textual_input_port> port_;
  unsigned                line_      = 1;
  unsigned                column_    = 1;
  bool                    fold_case_ = false;
};

} // namespace insider

#endif
