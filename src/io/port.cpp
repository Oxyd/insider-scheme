#include "port.hpp"

#include "context.hpp"
#include "util/define_procedure.hpp"

#include <fmt/format.h>

#include <cstdio>

#ifdef WIN32
#include <tchar.h>
#else
#include <poll.h>
#define _T(x) x
#endif

namespace insider {

std::optional<std::uint8_t>
port_source::read_if_available() {
  if (byte_ready())
    return read();
  else
    return std::nullopt;
}

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

bool
file_port_source::byte_ready() const {
#ifndef WIN32
  pollfd pfd{};
  pfd.fd = fileno(f_);
  pfd.events = POLLIN;
  return poll(&pfd, 1, 0) == 1;
#else
  // TODO: Fix this. This only makes sense for "interactive" ports -- the
  // console and pipes, so they could be special-cased somehow.
  return true;
#endif
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

textual_input_port::textual_input_port(std::unique_ptr<port_source> source,
                                       std::string name)
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
textual_input_port::char_ready() {
  return !source_ || fill_read_buffer_if_available();
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
textual_input_port::read_byte_if_available() {
  if (auto maybe_byte = source_->read_if_available()) {
    read_buffer_[read_buffer_length_++] = *maybe_byte;
    return true;
  } else
    return false;
}

bool
textual_input_port::fill_read_buffer() {
  return do_fill_read_buffer<&textual_input_port::read_byte>();
}

bool
textual_input_port::fill_read_buffer_if_available() {
  return do_fill_read_buffer<&textual_input_port::read_byte_if_available>();
}

template <auto Read>
bool
textual_input_port::do_fill_read_buffer() {
  if (read_buffer_length_ == 0)
    if (!(this->*Read)())
      return false;

  return fill_subsequent_bytes_of_read_buffer<Read>();
}

template <auto Read>
bool
textual_input_port::fill_subsequent_bytes_of_read_buffer() {
  std::size_t required
    = utf8_code_point_byte_length(static_cast<char>(read_buffer_[0]));
  while (read_buffer_length_ < required)
    if (!(this->*Read)())
      return false;

  return true;
}

char32_t
textual_input_port::decode_read_buffer() {
  return from_utf8(read_buffer_.begin(),
                   read_buffer_.begin() + read_buffer_length_).code_point;
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

bool
binary_input_port::u8_ready() const {
  return !source_ || source_->byte_ready();
}

ptr<textual_input_port>
open_input_string(context& ctx, std::string data) {
  return make<textual_input_port>(
    ctx,
    std::make_unique<string_port_source>(std::move(data)),
    "<memory buffer>"
  );
}

static ptr<binary_input_port>
open_input_bytevector(context& ctx, ptr<bytevector> data) {
  return make<binary_input_port>(
    ctx,
    std::make_unique<bytevector_port_source>(bytevector_data(data))
  );
}

static ptr<binary_output_port>
open_output_bytevector(context& ctx) {
  return make<binary_output_port>(ctx, std::make_unique<bytevector_port_sink>());
}

static FILE*
open_file(std::filesystem::path const& path,
          std::filesystem::path::value_type const* mode) {
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
    return make<textual_input_port>(
      ctx,
      std::make_unique<file_port_source>(f), path.string()
    );
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
file_port_sink::flush() {
  std::fflush(f_);
}

void
string_port_sink::write(std::uint8_t byte) {
  data_.push_back(static_cast<char>(byte));
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
textual_output_port::write_utf8(char c) {
  if (sink_)
    sink_->write(c);
}

void
textual_output_port::write(std::string const& s) {
  if (sink_)
    for (char c : s)
      sink_->write(c);
}

void
textual_output_port::flush() {
  if (sink_)
    sink_->flush();
}

binary_output_port::binary_output_port(std::unique_ptr<port_sink> sink)
  : sink_{std::move(sink)}
{ }

void
binary_output_port::write(std::uint8_t byte) {
  if (sink_)
    sink_->write(byte);
}

void
binary_output_port::flush() {
  if (sink_)
    sink_->flush();
}

ptr<bytevector>
binary_output_port::get_bytevector(context& ctx) const {
  if (sink_)
    return make_bytevector_from_std_vector(ctx, sink_->get_bytevector());
  else
    return make<bytevector>(ctx, 0);
}

static ptr<textual_input_port>
open_input_file(context& ctx, std::filesystem::path const& path) {
  if (std::FILE* f = open_file(path, _T("r")))
    return make<textual_input_port>(
      ctx,
      std::make_unique<file_port_source>(f), path.string()
    );
  else
    throw make<file_error>(
      ctx,
      fmt::format("Can't open {} for reading: {}",
                  path.string(), strerror(errno))
    );
}

static ptr<binary_input_port>
open_binary_input_file(context& ctx, std::filesystem::path const& path) {
  if (std::FILE* f = open_file(path, _T("rb")))
    return make<binary_input_port>(ctx, std::make_unique<file_port_source>(f));
  else
    throw make<file_error>(
      ctx,
      fmt::format("Can't open {} for reading: {}",
                  path.string(), strerror(errno))
    );
}

static ptr<textual_output_port>
open_output_file(context& ctx, std::string const& path) {
  if (std::FILE* f = open_file(path, _T("w")))
    return make<textual_output_port>(ctx, std::make_unique<file_port_sink>(f));
  else
    throw make<file_error>(
      ctx,
      fmt::format("Can't open {} for writing: {}", path, strerror(errno))
    );
}

static ptr<binary_output_port>
open_binary_output_file(context& ctx, std::string const& path) {
  if (std::FILE* f = open_file(path, _T("wb")))
    return make<binary_output_port>(ctx, std::make_unique<file_port_sink>(f));
  else
    throw make<file_error>(
      ctx,
      fmt::format("Can't open {} for writing: {}", path, strerror(errno))
    );
}

static void
close(port p) {
  visit([] (auto x) { x->close(); }, p);
}

static ptr<textual_output_port>
open_output_string(context& ctx) {
  return make<textual_output_port>(ctx, std::make_unique<string_port_sink>());
}

static void
flush_port(output_port port) {
  visit([] (auto x) { x->flush(); }, port);
}

static bool
is_port_open(port p) {
  return visit([] (auto x) { return x->open(); }, p);
}

auto
guard_filesystem_error(context& ctx, auto&& thunk) {
  try {
    return thunk();
  } catch (std::filesystem::filesystem_error const& e) {
    throw make<file_error>(ctx, e.what());
  }
}

static bool
file_exists(context& ctx, std::filesystem::path const& p) {
  return guard_filesystem_error(ctx, [&] { return std::filesystem::exists(p); });
}

static void
delete_file(context& ctx, std::filesystem::path const& p) {
  guard_filesystem_error(ctx, [&] { return std::filesystem::remove(p); });
}

ptr<textual_input_port>
get_current_textual_input_port(context& ctx) {
  return expect<textual_input_port>(
    find_parameter_value(ctx, ctx.constants->current_input_port_tag)
  );
}

ptr<textual_output_port>
get_current_textual_output_port(context& ctx) {
  return expect<textual_output_port>(
    find_parameter_value(ctx, ctx.constants->current_output_port_tag)
  );
}

static ptr<>
read_char(context& ctx, ptr<textual_input_port> port) {
  if (auto c = port->read_character())
    return character_to_ptr(*c);
  else
    return ctx.constants->eof;
}

static ptr<>
peek_char(context& ctx, ptr<textual_input_port> port) {
  if (auto c = port->peek_character())
    return character_to_ptr(*c);
  else
    return ctx.constants->eof;
}

static void
write_char_proc(char32_t c, ptr<textual_output_port> out) {
  out->write(c);
}

static ptr<>
read_u8(context& ctx, ptr<binary_input_port> port) {
  if (auto b = port->read_u8())
    return integer_to_ptr(*b);
  else
    return ctx.constants->eof;
}

static ptr<>
peek_u8(context& ctx, ptr<binary_input_port> port) {
  if (auto b = port->peek_u8())
    return integer_to_ptr(*b);
  else
    return ctx.constants->eof;
}

static void
write_u8(std::uint8_t byte, ptr<binary_output_port> port) {
  port->write(byte);
}

void
export_port(context& ctx, ptr<module_> result) {
  define_procedure<open_input_file>(ctx, "open-input-file", result);
  define_procedure<open_output_file>(ctx, "open-output-file", result);
  define_procedure<open_binary_input_file>(ctx, "open-binary-input-file",
                                           result);
  define_procedure<open_binary_output_file>(ctx, "open-binary-output-file",
                                            result);
  define_procedure<close>(ctx, "close-port", result);
  define_procedure<close>(ctx, "close-output-port", result);
  define_procedure<close>(ctx, "close-input-port", result);
  define_procedure<open_input_string>(ctx, "open-input-string", result);
  define_procedure<open_output_string>(ctx, "open-output-string", result);
  define_procedure<open_input_bytevector>(ctx, "open-input-bytevector", result);
  define_procedure<open_output_bytevector>(ctx, "open-output-bytevector",
                                           result);
  define_procedure<&textual_output_port::get_string>(ctx, "get-output-string",
                                                     result);
  define_procedure<&binary_output_port::get_bytevector>(ctx,
                                                        "get-output-bytevector",
                                                        result);
  define_procedure<read_char>(ctx, "read-char", result);
  define_procedure<peek_char>(ctx, "peek-char", result);
  define_procedure<write_char_proc>(ctx, "write-char", result);
  define_procedure<read_u8>(ctx, "read-u8", result);
  define_procedure<peek_u8>(ctx, "peek-u8", result);
  define_procedure<write_u8>(ctx, "write-u8", result);
  define_procedure<flush_port>(ctx, "flush-output-port", result);
  define_procedure<is_port_open>(ctx, "port-open?", result);
  define_procedure<&textual_input_port::char_ready>(ctx, "char-ready?", result);
  define_procedure<&binary_input_port::u8_ready>(ctx, "u8-ready?", result);
  define_procedure<file_exists>(ctx, "file-exists?", result);
  define_procedure<delete_file>(ctx, "delete-file", result);
}

} // namespace insider
