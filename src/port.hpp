#ifndef INSIDER_PORT_HPP
#define INSIDER_PORT_HPP

#include "basic_types.hpp"
#include "object.hpp"

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
  ~port_source() { }

  virtual std::optional<std::uint8_t>
  read() = 0;

  virtual std::optional<std::uint8_t>
  peek() const = 0;

  virtual void
  rewind() = 0;
};

class file_port_source final : public port_source {
public:
  explicit
  file_port_source(FILE*, bool should_close = true);

  ~file_port_source() override;

  std::optional<std::uint8_t>
  read() override;

  std::optional<std::uint8_t>
  peek() const override;

  void
  rewind() override;

private:
  FILE* f_;
  bool  should_close_;
};

class string_port_source final : public port_source {
public:
  explicit
  string_port_source(std::string);

  std::optional<std::uint8_t>
  read() override;

  std::optional<std::uint8_t>
  peek() const override;

  void
  rewind() override;

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
  peek() const override;

  void
  rewind() override;

private:
  std::vector<std::uint8_t> data_;
  std::size_t               position_ = 0;
};

class textual_input_port : public leaf_object<textual_input_port> {
public:
  static constexpr char const* scheme_name = "insider::textual_input_port";

  textual_input_port(std::unique_ptr<port_source>, std::string name);

  bool
  open() const { return static_cast<bool>(source_); }

  void
  close() { source_.reset(); }

  std::optional<char32_t>
  peek_character();

  std::optional<char32_t>
  read_character();

  void
  rewind();

  std::string const&
  name() const { return name_; }

private:
  std::unique_ptr<port_source> source_;
  std::array<char, 4>          read_buffer_;
  std::size_t                  read_buffer_length_ = 0;
  std::string                  name_;

  bool
  read_byte();

  bool
  fill_read_buffer();

  char32_t
  decode_read_buffer();

  char32_t
  flush_read_buffer();
};

ptr<textual_input_port>
open_input_string(context&, std::string);

ptr<textual_input_port>
open_file_for_text_input(context&, std::filesystem::path const&);

class binary_input_port : public leaf_object<binary_input_port> {
public:
  static constexpr char const* scheme_name = "insider::binary_input_port";

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

private:
  std::unique_ptr<port_source> source_;
};

class port_sink {
public:
  virtual
  ~port_sink() { }

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

template <typename PortPtr>
class unique_port_handle {
public:
  unique_port_handle() = default;

  explicit
  unique_port_handle(PortPtr p) : ptr_{std::move(p)} { }

  unique_port_handle(unique_port_handle&& other)
    : ptr_{std::move(other.ptr_)}
  {
    other.ptr_.reset();
  }

  ~unique_port_handle() { if (ptr_) ptr_->close(); }

  unique_port_handle&
  operator = (unique_port_handle&& other) {
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

} // namespace insider

#endif
