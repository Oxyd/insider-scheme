#include "port.hpp"

#include "context.hpp"
#include "define_procedure.hpp"

#include <fmt/format.h>

#include <cstdio>

#ifdef WIN32
#include <tchar.h>
#else
#define _T(x) x
#endif

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
    return static_cast<std::uint8_t>(byte);
  else
    return {};
}

std::optional<std::uint8_t>
file_port_source::peek() const {
  int byte = std::getc(f_);
  if (byte == EOF)
    return {};

  std::ungetc(byte, f_);
  return static_cast<std::uint8_t>(byte);
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
string_port_source::peek() const {
  if (position_ == data_.length())
    return {};
  else
    return data_[position_];
}

void
string_port_source::rewind() {
  position_ = 0;
}

bytevector_port_source::bytevector_port_source(std::vector<std::uint8_t> data)
  : data_{std::move(data)}
{ }

std::optional<std::uint8_t>
bytevector_port_source::read() {
  if (position_ == data_.size())
    return {};
  else
    return data_[position_++];
}

std::optional<std::uint8_t>
bytevector_port_source::peek() const {
  if (position_ == data_.size())
    return {};
  else
    return data_[position_];
}

void
bytevector_port_source::rewind() {
  position_ = 0;
}

textual_input_port::textual_input_port(std::unique_ptr<port_source> source, std::string name)
  : source_{std::move(source)}
  , name_{std::move(name)}
{ }

std::optional<char32_t>
textual_input_port::peek_character() {
  if (!source_)
    return {};

  if (!fill_read_buffer())
    return {};
  else
    return decode_read_buffer();
}

std::optional<char32_t>
textual_input_port::read_character() {
  if (!source_)
    return {};

  if (!fill_read_buffer())
    return {};
  else
    return flush_read_buffer();
}

void
textual_input_port::rewind() {
  if (source_)
    source_->rewind();
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

binary_input_port::binary_input_port(std::unique_ptr<port_source> source)
  : source_{std::move(source)}
{ }

std::optional<std::uint8_t>
binary_input_port::read_u8() {
  return source_->read();
}

std::optional<std::uint8_t>
binary_input_port::peek_u8() const {
  return source_->peek();
}

ptr<textual_input_port>
open_input_string(context& ctx, std::string data) {
  return make<textual_input_port>(ctx, std::make_unique<string_port_source>(std::move(data)),
                                  "<memory buffer>");
}

static FILE*
open_file(std::filesystem::path const& path, std::filesystem::path::value_type const* mode) {
#ifndef WIN32
  return std::fopen(path.c_str(), mode);
#else
  return _wfopen(path.c_str(), mode);
#endif
}

ptr<textual_input_port>
open_file_for_text_input(context& ctx, std::filesystem::path const& path) {
  FILE* f = open_file(path, _T("r"));
  if (f)
    return make<textual_input_port>(ctx, std::make_unique<file_port_source>(f), path.string());
  else
    return {};
}

std::string
port_sink::get_string() const {
  throw std::runtime_error{"Not a string port"};
}

std::vector<std::uint8_t>
port_sink::get_bytevector() const {
  throw std::runtime_error{"Not a bytevector port"};
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

void
bytevector_port_sink::write(std::uint8_t byte) {
  data_.push_back(byte);
}

textual_output_port::textual_output_port(std::unique_ptr<port_sink> sink)
  : sink_{std::move(sink)}
{ }

void
textual_output_port::write(char32_t c) {
  if (sink_)
    to_utf8(c, [&] (char byte) { sink_->write(byte); });
}

void
textual_output_port::write(std::string const& s) {
  if (sink_)
    for (char c : s)
      sink_->write(c);
}

binary_output_port::binary_output_port(std::unique_ptr<port_sink> sink)
  : sink_{std::move(sink)}
{ }

void
binary_output_port::write(std::uint8_t byte) {
  if (sink_)
    sink_->write(byte);
}

static ptr<textual_input_port>
open_input_file(context& ctx, std::filesystem::path const& path) {
  if (std::FILE* f = open_file(path, _T("r")))
    return make<textual_input_port>(ctx, std::make_unique<file_port_source>(f), path.string());
  else
    throw make<file_error>(ctx, fmt::format("Can't open {} for reading: {}", path.string(), strerror(errno)));
}

static ptr<textual_output_port>
open_output_file(context& ctx, std::string const& path) {
  if (std::FILE* f = open_file(path, _T("w")))
    return make<textual_output_port>(ctx, std::make_unique<file_port_sink>(f));
  else
    throw make<file_error>(ctx, fmt::format("Can't open {} for writing: {}", path, strerror(errno)));
}

static void
close(ptr<> port) {
  if (auto tip = match<textual_input_port>(port))
    tip->close();
  else if (auto top = match<textual_output_port>(port))
    top->close();
  else
    throw std::runtime_error{"Expected a port"};
}

static ptr<textual_output_port>
open_output_string(context& ctx) {
  return make<textual_output_port>(ctx, std::make_unique<string_port_sink>());
}

void
export_port(context& ctx, module_& result) {
  define_procedure(ctx, "open-input-file", result, true, open_input_file);
  define_procedure(ctx, "open-output-file", result, true, open_output_file);
  define_procedure(ctx, "close", result, true, close);
  define_procedure(ctx, "close-output-port", result, true, close);
  define_procedure(ctx, "close-input-port", result, true, close);
  define_procedure(ctx, "open-input-string", result, true, open_input_string);
  define_procedure(ctx, "open-output-string", result, true, open_output_string);
  define_procedure(ctx, "get-output-string", result, true, &textual_output_port::get_string);
}

} // namespace insider

