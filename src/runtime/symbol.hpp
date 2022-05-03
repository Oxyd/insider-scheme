#ifndef INSIDER_RUNTIME_SYMBOL_HPP
#define INSIDER_RUNTIME_SYMBOL_HPP

#include "object.hpp"

#include <string>

namespace insider {

// An immutable string, used for identifying Scheme objects.
class symbol : public leaf_object<symbol> {
public:
  static constexpr char const* scheme_name = "insider::symbol";

  explicit
  symbol(std::string value) : value_{std::move(value)} { }

  std::string
  value() const { return value_; }

private:
  std::string value_;
};

} // namespace insider

#endif
