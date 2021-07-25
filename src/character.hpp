#ifndef INSIDER_CHARACTER_HPP
#define INSIDER_CHARACTER_HPP

#include "object.hpp"

namespace insider {

class character : public leaf_object<character> {
public:
  static constexpr char const* scheme_name = "insider::character";
  using value_type = char32_t;

  explicit
  character(value_type c) : value_{c} { }

  value_type
  value() const { return value_; }

  std::size_t
  hash() const { return std::hash<value_type>{}(value_); }

private:
  value_type value_;
};

} // namespace insider

#endif
