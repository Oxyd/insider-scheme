#ifndef INSIDER_RUNTIME_STRING_HPP
#define INSIDER_RUNTIME_STRING_HPP

#include "object.hpp"
#include "type_indexes.hpp"

#include <string>

namespace insider {

class bytevector;

class string : public leaf_object<string> {
public:
  static constexpr char const* scheme_name = "insider::string";
  static constexpr word_type static_type_index = type_indexes::string;

  explicit
  string(std::size_t size) : data_(size, '\0') {}

  explicit
  string(std::string value) : data_{std::move(value)} { }

  void
  set(std::size_t i, char32_t c);

  void
  set_byte_index(std::size_t byte_index, char32_t c);

  char32_t
  ref(std::size_t) const;

  void
  append_char(char32_t);

  void
  append(std::string const&);

  std::string const&
  value() const { return data_; }

  std::size_t
  length() const;

  std::size_t
  hash() const;

private:
  std::string data_;
};

inline bool
string_equal(ptr<string> x , ptr<string> y) { return x->value() == y->value(); }

ptr<string>
string_upcase(context&, ptr<string>);

ptr<string>
string_downcase(context&, ptr<string>);

ptr<string>
string_foldcase(context&, ptr<string>);

std::u32string
string_foldcase(std::u32string const&);

ptr<string>
utf8_to_string(context&, ptr<bytevector>, std::size_t start, std::size_t end);

ptr<bytevector>
string_to_utf8_byte_indexes(context&, ptr<string>, std::size_t start,
                            std::size_t end);

ptr<string>
string_reverse(context&, ptr<string>, std::size_t begin, std::size_t end);

integer
next_code_point_byte_index(ptr<string> s, std::size_t index);

integer
previous_code_point_byte_index(ptr<string> s, std::size_t index);

inline std::size_t
string_byte_length(ptr<string> s) {
  return s->value().size();
}

inline bool
is_string_null(ptr<string> s) {
  return s->value().empty();
}

} // namespace insider

#endif
