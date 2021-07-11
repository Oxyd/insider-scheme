#ifndef INSIDER_PORT_HPP
#define INSIDER_PORT_HPP

#include "object.hpp"

#include <variant>

namespace insider {

// I/O port or a string port. Can be read or write, binary or text.
class port : public leaf_object<port> {
public:
  static constexpr char const* scheme_name = "insider::port";

  port(FILE*, std::string name, bool input, bool output, bool should_close = true);
  port(std::string value, bool input, bool output);
  port(port&&);
  ~port();

  port&
  operator = (port const&) = delete;
  port&
  operator = (port&&) = delete;

  void
  write_string(std::string const&);
  void
  write_char(char c);

  std::optional<char>
  peek_char();
  std::optional<char>
  read_char();

  void
  put_back(char);

  std::string
  get_string() const;

  void
  rewind();

  std::string const&
  name() const { return name_; }

private:
  struct string_buffer {
    std::string data;
    std::size_t read_index = 0;
  };

  std::variant<FILE*, string_buffer> buffer_;
  std::vector<char> put_back_buffer_;
  bool input_ = false;
  bool output_ = false;
  bool should_close_ = false;
  std::string name_;

  void
  destroy();
};

} // namespace insider

#endif
