#ifndef INSIDER_STRING_HPP
#define INSIDER_STRING_HPP

#include "object.hpp"

#include <string>

namespace insider {

class string : public leaf_object<string> {
public:
  static constexpr char const* scheme_name = "insider::string";

  explicit
  string(std::size_t size) : data_(size, '\0') {}

  explicit
  string(std::string value) : data_{std::move(value)} { }

  void
  set(std::size_t i, char c);

  character
  ref(std::size_t) const;

  std::string const&
  value() const { return data_; }

  std::size_t
  size() const { return data_.size(); }

  std::size_t
  hash() const;

private:
  std::string data_;
};

} // namespace insider

#endif
