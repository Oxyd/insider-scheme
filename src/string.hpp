#ifndef INSIDER_STRING_HPP
#define INSIDER_STRING_HPP

#include "object.hpp"

#include <string>

namespace insider {

class bytevector;

class string : public leaf_object<string> {
public:
  static constexpr char const* scheme_name = "insider::string";

  explicit
  string(std::size_t size) : data_(size, '\0') {}

  explicit
  string(std::string value) : data_{std::move(value)} { }

  void
  set(std::size_t i, char32_t c);

  char32_t
  ref(std::size_t) const;

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
upcase(context&, ptr<string>);

ptr<string>
downcase(context&, ptr<string>);

ptr<string>
foldcase(context&, ptr<string>);

std::u32string
foldcase(std::u32string const&);

ptr<string>
utf8_to_string(context&, ptr<bytevector>, std::size_t start, std::size_t end);

ptr<bytevector>
string_to_utf8(context&, ptr<string>, std::size_t start, std::size_t end);

} // namespace insider

#endif
