#include "reader_stream.hpp"

#include "port.hpp"

namespace insider {

reader_stream::checkpoint::~checkpoint() {
  revert();
  --stream_.checkpoints_active_;
  stream_.check_discard_rollback_buffer();
}

void
reader_stream::checkpoint::commit() {
  committed_ = true;
}

void
reader_stream::checkpoint::revert() {
  if (!committed_)
    stream_.revert_to(*this);
  committed_ = true;
}

reader_stream::checkpoint::checkpoint(reader_stream& stream)
  : stream_{stream}
  , loc_{stream.loc_}
  , position_{stream.rollback_pos_}
{ }

reader_stream::reader_stream(tracked_ptr<textual_input_port> p)
  : port_{std::move(p)}
  , loc_{port_->name(), 1, 1}
{ }

std::optional<char32_t>
reader_stream::read() {
  std::optional<char32_t> c = read_source();
  if (c)
    update_location(*c);
  return c;
}

std::optional<char32_t>
reader_stream::peek() {
  if (reading_from_rollback_buffer())
    return rollback_buffer_[rollback_pos_];
  else
    return port_->peek_character();
}

source_location
reader_stream::location() const {
  return loc_;
}

std::optional<char32_t>
reader_stream::read_source() {
  if (reading_from_rollback_buffer())
    return read_rollback_buffer();
  else
    return read_stream();
}

std::optional<char32_t>
reader_stream::read_rollback_buffer() {
  auto result = rollback_buffer_[rollback_pos_++];
  check_discard_rollback_buffer();
  return result;
}

void
reader_stream::check_discard_rollback_buffer() {
  if (checkpoints_active_ == 0 && rollback_pos_ >= rollback_buffer_.size()) {
    rollback_buffer_.clear();
    rollback_pos_ = 0;
  }
}

std::optional<char32_t>
reader_stream::read_stream() {
  auto c = port_->read_character();

  if (c && checkpoints_active_) {
    rollback_buffer_.push_back(*c);
    ++rollback_pos_;
  }

  return c;
}

auto
reader_stream::make_checkpoint() -> checkpoint {
  ++checkpoints_active_;
  return {*this};
}

void
reader_stream::update_location(char32_t c) {
  if (c == U'\n') {
    ++loc_.line;
    loc_.column = 1;
  } else
    ++loc_.column;
}

void
reader_stream::revert_to(checkpoint const& cp) {
  if (cp.position_ < rollback_pos_) {
    loc_ = cp.loc_;
    rollback_pos_ = cp.position_;
    assert(rollback_pos_ < rollback_buffer_.size());
  }
}

std::optional<char32_t>
advance_and_peek(reader_stream& stream) {
  stream.read();
  return stream.peek();
}

} // namespace insider
