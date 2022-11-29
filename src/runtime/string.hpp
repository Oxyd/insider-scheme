#ifndef INSIDER_RUNTIME_STRING_HPP
#define INSIDER_RUNTIME_STRING_HPP

#include "object.hpp"
#include "runtime/character.hpp"
#include "type_indexes.hpp"

#include <string>

namespace insider {

class bytevector;

class string_cursor {
public:
  using value_type = word_type;
  value_type value;

  bool
  operator == (string_cursor const&) const = default;
};

inline string_cursor
ptr_to_string_cursor(ptr<> x) {
  assert(is_string_cursor(x));
  return {tagged_payload(x) >> string_cursor_payload_offset};
}

inline ptr<>
string_cursor_to_ptr(string_cursor c) {
  return immediate_to_ptr((c.value << string_cursor_payload_offset)
                          | string_cursor_tag);
}

class string : public leaf_object<string> {
public:
  static constexpr char const* scheme_name = "insider::string";
  static constexpr word_type static_type_index = type_indexes::string;

  explicit
  string(std::size_t size)
    : data_(size, '\0')
    , codepoint_length_{size}
  { }

  explicit
  string(std::string value);

  void
  set_cursor(string_cursor, char32_t);

  void
  append_char(char32_t);

  void
  append(std::string const&);

  std::string const&
  value() const { return data_; }

  std::size_t
  length() const { return codepoint_length_; }

  std::size_t
  hash() const;

private:
  std::string data_;
  std::size_t codepoint_length_;
};

inline bool
string_equal(ptr<string> x , ptr<string> y) { return x->value() == y->value(); }

inline string_cursor
string_cursor_start(ptr<string>) { return {0}; }

inline string_cursor
string_cursor_end(ptr<string> s) { return {s->value().size()}; }

inline string_cursor
string_cursor_next(ptr<string> s, string_cursor c) {
  if (c.value >= s->value().size())
    throw std::runtime_error{"Can't advance cursor past end of string"};
  else
    return {c.value + utf8_code_point_byte_length(s->value()[c.value])};
}

string_cursor
string_cursor_prev(ptr<string> s, string_cursor c);

char32_t
string_ref_cursor(ptr<string>, string_cursor);

void
string_set(ptr<string> s, ptr<> i, char32_t c);

void
string_set_nth(ptr<string> s, std::size_t i, char32_t c);

char32_t
string_ref_nth(ptr<string>, std::size_t);

char32_t
string_ref(ptr<string> s, ptr<> i);

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
string_to_utf8(context&, ptr<string>, string_cursor start, string_cursor end);

ptr<string>
string_reverse_byte_indexes(context&, ptr<string>,
                            std::size_t begin, std::size_t end);

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
