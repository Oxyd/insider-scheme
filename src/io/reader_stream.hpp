#ifndef INSIDER_IO_READER_STREAM_HPP
#define INSIDER_IO_READER_STREAM_HPP

#include "compiler/source_location.hpp"
#include "memory/tracked_ptr.hpp"
#include "ptr.hpp"

#include <optional>

namespace insider {

class textual_input_port;

class reader_stream {
public:
  class checkpoint {
  public:
    ~checkpoint();

    checkpoint(checkpoint const&) = delete;
    void operator = (checkpoint const&) = delete;

    void
    commit();

    void
    revert();

  private:
    friend class reader_stream;

    checkpoint(reader_stream&);

    reader_stream&  stream_;
    source_location loc_;
    std::size_t     position_;
    bool            committed_ = false;
  };

  bool fold_case = false;

  explicit
  reader_stream(tracked_ptr<textual_input_port>);

  std::optional<char32_t>
  read();

  std::optional<char32_t>
  peek();

  source_location
  location() const;

  checkpoint
  make_checkpoint();

private:
  tracked_ptr<textual_input_port> port_;
  source_location                 loc_;
  std::vector<char32_t>           rollback_buffer_;
  std::size_t                     rollback_pos_ = 0;
  unsigned                        checkpoints_active_ = 0;

  bool
  reading_from_rollback_buffer() const {
    return rollback_pos_ < rollback_buffer_.size();
  }

  std::optional<char32_t>
  read_source();

  std::optional<char32_t>
  read_rollback_buffer();

  void
  check_discard_rollback_buffer();

  std::optional<char32_t>
  read_stream();

  void
  update_location(char32_t);

  void
  revert_to(checkpoint const&);
};

std::optional<char32_t>
advance_and_peek(reader_stream&);

} // namespace insider

#endif
