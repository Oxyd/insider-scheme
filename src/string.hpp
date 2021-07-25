#ifndef INSIDER_STRING_HPP
#define INSIDER_STRING_HPP

#include "object.hpp"

namespace insider {

// Fixed-length string. TODO: Support Unicode.
class string : public dynamic_size_object<string, char> {
public:
  static constexpr char const* scheme_name = "insider::string";

  static std::size_t
  extra_elements(std::size_t size) { return size; }

  explicit
  string(std::size_t size) : size_{size} { }

  string(string&& other);

  void
  set(std::size_t i, char c);

  std::string
  value() const;

  std::size_t
  size() const { return size_; }

  std::size_t
  hash() const;

  void
  visit_members(member_visitor const&) { }

private:
  std::size_t size_;
};

ptr<string>
make_string(context&, std::string_view value);

} // namespace insider

#endif
