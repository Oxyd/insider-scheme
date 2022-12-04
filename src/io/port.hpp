#ifndef INSIDER_IO_PORT_HPP
#define INSIDER_IO_PORT_HPP

#include "object.hpp"
#include "runtime/basic_types.hpp"
#include "util/sum_type.hpp"

#include <array>
#include <filesystem>
#include <memory>
#include <variant>
#include <vector>

namespace insider {

class module_;

class port_source {
public:
  virtual
  ~port_source() = default;

  virtual std::optional<std::uint8_t>
  read() = 0;

  virtual std::optional<std::uint8_t>
  peek() = 0;

  virtual void
  rewind() = 0;

  virtual bool
  byte_ready() const = 0;

  std::optional<std::uint8_t>
  read_if_available();
};

class file_port_source final : public port_source {
public:
  static std::unique_ptr<file_port_source>
  open(std::filesystem::path const&, bool binary);

  ~file_port_source() override;

  std::optional<std::uint8_t>
  read() override;

  std::optional<std::uint8_t>
  peek() override;

  void
  rewind() override;

  bool
  byte_ready() const override;

private:
  FILE*                           f_;
  std::unique_ptr<std::uint8_t[]> buffer_;
  std::size_t                     buffer_size_ = 0;
  std::size_t                     buffer_pos_  = 0;

  explicit
  file_port_source(FILE*);

  void
  fill_buffer();

  bool
  buffer_empty() const { return buffer_pos_ == buffer_size_; }
};

class stdin_source final : public port_source {
public:
  std::optional<std::uint8_t>
  read() override;

  std::optional<std::uint8_t>
  peek() override;

  void
  rewind() override;

  bool
  byte_ready() const override;
};

class string_port_source final : public port_source {
public:
  explicit
  string_port_source(std::string);

  std::optional<std::uint8_t>
  read() override;

  std::optional<std::uint8_t>
  peek() override;

  void
  rewind() override;

  bool
  byte_ready() const override { return true; }

private:
  std::string data_;
  std::size_t position_ = 0;
};

class bytevector_port_source final : public port_source {
public:
  explicit
  bytevector_port_source(std::vector<std::uint8_t> data);

  std::optional<std::uint8_t>
  read() override;

  std::optional<std::uint8_t>
  peek() override;

  void
  rewind() override;

  bool
  byte_ready() const override { return true; }

private:
  std::vector<std::uint8_t> data_;
  std::size_t               position_ = 0;
};

class textual_input_port : public leaf_object<textual_input_port> {
public:
  static constexpr char const* scheme_name = "insider::textual_input_port";
  static constexpr word_type static_type_index
    = type_indexes::textual_input_port;

  textual_input_port(std::unique_ptr<port_source>, std::string name);

  bool
  open() const { return static_cast<bool>(source_); }

  void
  close() { source_.reset(); }

  std::optional<char32_t>
  peek_character();

  std::optional<char32_t>
  read_character();

  std::optional<std::string>
  read_line();

  void
  rewind();

  bool
  char_ready();

  std::string const&
  name() const { return name_; }

private:
  std::unique_ptr<port_source> source_;
  std::array<std::uint8_t, 4>  read_buffer_{};
  std::size_t                  read_buffer_length_ = 0;
  std::string                  name_;

  bool
  read_byte();

  bool
  read_byte_if_available();

  bool
  fill_read_buffer();

  bool
  fill_read_buffer_if_available();

  template <auto Read>
  bool
  do_fill_read_buffer();

  template <auto Read>
  bool
  fill_subsequent_bytes_of_read_buffer();

  char32_t
  decode_read_buffer();

  char32_t
  flush_read_buffer();
};

ptr<textual_input_port>
open_input_string(context&, std::string);

ptr<textual_input_port>
open_file_for_text_input(context&, std::filesystem::path const&);

ptr<textual_input_port>
get_current_textual_input_port(context& ctx);

ptr<textual_output_port>
get_current_textual_output_port(context& ctx);

ptr<>
read_char(context& ctx, ptr<textual_input_port> port);

ptr<>
peek_char(context& ctx, ptr<textual_input_port> port);

void
write_char(char32_t c, ptr<textual_output_port> out);

class binary_input_port : public leaf_object<binary_input_port> {
public:
  static constexpr char const* scheme_name = "insider::binary_input_port";
  static constexpr word_type static_type_index = type_indexes::binary_input_port;

  explicit
  binary_input_port(std::unique_ptr<port_source>);

  bool
  open() const { return static_cast<bool>(source_); }

  void
  close() { source_.reset(); }

  std::optional<std::uint8_t>
  read_u8();

  std::optional<std::uint8_t>
  peek_u8() const;

  bool
  u8_ready() const;

private:
  std::unique_ptr<port_source> source_;
};

class port_sink {
public:
  virtual
  ~port_sink() = default;

  virtual void
  write(std::uint8_t) = 0;

  virtual void
  flush() { }

  virtual std::string
  get_string() const;

  virtual std::vector<std::uint8_t>
  get_bytevector() const;
};

class file_port_sink final : public port_sink {
public:
  explicit
  file_port_sink(FILE*, bool should_close = true);

  ~file_port_sink() override;

  void
  write(std::uint8_t) override;

  void
  flush() override;

private:
  FILE* f_;
  bool  should_close_;
};

class string_port_sink final : public port_sink {
public:
  void
  write(std::uint8_t) override;

  std::string
  get_string() const override { return data_; }

private:
  std::string data_;
};

class bytevector_port_sink final : public port_sink {
public:
  void
  write(std::uint8_t) override;

  std::vector<std::uint8_t>
  get_bytevector() const override { return data_; }

private:
  std::vector<std::uint8_t> data_;
};

class textual_output_port : public leaf_object<textual_output_port> {
public:
  static constexpr char const* scheme_name = "insider::textual_output_port";
  static constexpr word_type static_type_index
    = type_indexes::textual_output_port;

  explicit
  textual_output_port(std::unique_ptr<port_sink>);

  bool
  open() const { return static_cast<bool>(sink_); }

  void
  close() { sink_.reset(); }

  void
  write(char32_t);

  void
  write_utf8(char);

  void
  write(std::string const&);

  void
  flush();

  std::string
  get_string() const { return sink_ ? sink_->get_string() : ""; }

private:
  std::unique_ptr<port_sink> sink_;
};

class binary_output_port : public leaf_object<binary_output_port> {
public:
  static constexpr char const* scheme_name = "insider::binary_output_port";
  static constexpr word_type static_type_index
    = type_indexes::binary_output_port;

  explicit
  binary_output_port(std::unique_ptr<port_sink>);

  bool
  open() const { return static_cast<bool>(sink_); }

  void
  close() { sink_.reset(); }

  void
  write(std::uint8_t);

  void
  flush();

  ptr<bytevector>
  get_bytevector(context&) const;

private:
  std::unique_ptr<port_sink> sink_;
};

ptr<>
read_u8(context& ctx, ptr<binary_input_port> port);

ptr<>
peek_u8(context& ctx, ptr<binary_input_port> port);

void
write_u8(std::uint8_t byte, ptr<binary_output_port> port);

template <typename PortPtr>
class unique_port_handle {
public:
  unique_port_handle() = default;

  explicit
  unique_port_handle(PortPtr p) : ptr_{std::move(p)} { }

  unique_port_handle(unique_port_handle&& other) noexcept
    : ptr_{std::move(other.ptr_)}
  {
    other.ptr_.reset();
  }

  ~unique_port_handle() { if (ptr_) ptr_->close(); }

  unique_port_handle&
  operator = (unique_port_handle&& other) noexcept {
    if (this == &other)
      return *this;

    if (ptr_)
      ptr_->close();

    ptr_ = std::move(other.ptr_);
    other.ptr_.reset();
    return *this;
  }

  PortPtr
  operator * () const { return ptr_; }

  auto
  operator -> () const { return ptr_.operator -> (); }

  PortPtr
  get() const { return ptr_; }

  PortPtr
  release() {
    PortPtr result = std::move(ptr_);
    ptr_.reset();
    return result;
  }

private:
  PortPtr ptr_;
};

using port = sum_type<textual_input_port, textual_output_port,
                      binary_input_port, binary_output_port>;

using output_port = sum_type<textual_output_port, binary_output_port>;

} // namespace insider

#endif
