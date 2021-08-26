#ifndef INSIDER_PORT_HPP
#define INSIDER_PORT_HPP

#include "object.hpp"

#include <array>
#include <memory>
#include <variant>
#include <vector>

namespace insider {

class port_source {
public:
  virtual
  ~port_source() { }

  virtual std::optional<std::uint8_t>
  read() = 0;

  virtual std::optional<std::uint8_t>
  peek() = 0;

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
  peek() override;

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
  peek() override;

  void
  rewind() override;

private:
  std::string data_;
  std::size_t position_ = 0;
};

class textual_input_port : public leaf_object<textual_input_port> {
public:
  static constexpr char const* scheme_name = "insider::textual_input_port";

  textual_input_port(std::unique_ptr<port_source>, std::string name);

  std::optional<char32_t>
  peek_character();

  std::optional<char32_t>
  read_character();

  void
  put_back(char32_t);

  void
  rewind();

  std::string const&
  name() const { return name_; }

private:
  std::unique_ptr<port_source> source_;
  std::vector<char32_t>        put_back_buffer_;
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
make_string_input_port(context&, std::string);

class port_sink {
public:
  virtual
  ~port_sink() { }

  virtual void
  write(std::uint8_t) = 0;

  virtual std::string
  get_string() const;
};

class file_port_sink final : public port_sink {
public:
  explicit
  file_port_sink(FILE*, bool should_close = true);

  ~file_port_sink() override;

  void
  write(std::uint8_t) override;

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

class textual_output_port : public leaf_object<textual_output_port> {
public:
  static constexpr char const* scheme_name = "insider::textual_output_port";

  explicit
  textual_output_port(std::unique_ptr<port_sink>);

  void
  write(char32_t);

  void
  write(std::string const&);

  std::string
  get_string() const { return sink_->get_string(); }

private:
  std::unique_ptr<port_sink> sink_;
};

} // namespace insider

#endif
