#include "port.hpp"

#include "context.hpp"

#include <fmt/format.h>

namespace insider {

file_port_source::file_port_source(FILE* f, bool should_close)
  : f_{f}
  , should_close_{should_close}
{ }

file_port_source::~file_port_source() {
  if (should_close_)
    std::fclose(f_);
}

std::optional<std::uint8_t>
file_port_source::read() {
  int byte = std::getc(f_);
  if (byte != EOF)
    return byte;
  else
    return {};
}

std::optional<std::uint8_t>
file_port_source::peek() {
  int byte = std::getc(f_);
  if (byte == EOF)
    return {};

  std::ungetc(byte, f_);
  return byte;
}

void
file_port_source::rewind() {
  std::rewind(f_);
}

string_port_source::string_port_source(std::string data)
  : data_{std::move(data)}
{ }

std::optional<std::uint8_t>
string_port_source::read() {
  if (position_ == data_.length())
    return {};
  else
    return data_[position_++];
}

std::optional<std::uint8_t>
string_port_source::peek() {
  if (position_ == data_.length())
    return {};
  else
    return data_[position_];
}

void
string_port_source::rewind() {
  position_ = 0;
}

textual_input_port::textual_input_port(std::unique_ptr<port_source> source, std::string name)
  : source_{std::move(source)}
  , name_{std::move(name)}
{ }

std::optional<char32_t>
textual_input_port::peek_character() {
  if (!put_back_buffer_.empty())
    return put_back_buffer_.back();

  if (!fill_read_buffer())
    return {};
  else
    return decode_read_buffer();
}

std::optional<char32_t>
textual_input_port::read_character() {
  if (!put_back_buffer_.empty()) {
    char32_t c = put_back_buffer_.back();
    put_back_buffer_.pop_back();
    return c;
  }

  if (!fill_read_buffer())
    return {};
  else
    return flush_read_buffer();
}

void
textual_input_port::put_back(char32_t c) {
  put_back_buffer_.push_back(c);
}

void
textual_input_port::rewind() {
  source_->rewind();
  put_back_buffer_.clear();
  read_buffer_length_ = 0;
}

bool
textual_input_port::read_byte() {
  if (auto maybe_byte = source_->read()) {
    read_buffer_[read_buffer_length_++] = *maybe_byte;
    return true;
  } else
    return false;
}

bool
textual_input_port::fill_read_buffer() {
  if (read_buffer_length_ != 0)
    return true;

  if (!read_byte())
    return false;

  std::size_t required = utf8_code_point_byte_length(read_buffer_[0]);
  while (read_buffer_length_ < required)
    if (!read_byte())
      return false;

  return true;
}

char32_t
textual_input_port::decode_read_buffer() {
  return from_utf8(read_buffer_.begin(), read_buffer_.begin() + read_buffer_length_).code_point;
}

char32_t
textual_input_port::flush_read_buffer() {
  char32_t result = decode_read_buffer();
  read_buffer_length_ = 0;
  return result;
}

ptr<textual_input_port>
make_string_input_port(context& ctx, std::string data) {
  return make<textual_input_port>(ctx, std::make_unique<string_port_source>(std::move(data)),
                                  "<memory buffer>");
}

std::string
port_sink::get_string() const {
  throw std::runtime_error{"Not a string port"};
}

file_port_sink::file_port_sink(FILE* f, bool should_close)
  : f_{f}
  , should_close_{should_close}
{ }

file_port_sink::~file_port_sink() {
  if (should_close_)
    std::fclose(f_);
}

void
file_port_sink::write(std::uint8_t byte) {
  std::fputc(byte, f_);
}

void
string_port_sink::write(std::uint8_t byte) {
  data_.push_back(byte);
}

textual_output_port::textual_output_port(std::unique_ptr<port_sink> sink)
  : sink_{std::move(sink)}
{ }

void
textual_output_port::write(char32_t c) {
  to_utf8(c, [&] (char byte) { sink_->write(byte); });
}

void
textual_output_port::write(std::string const& s) {
  for (char c : s)
    sink_->write(c);
}

} // namespace insider

