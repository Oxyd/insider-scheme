#ifndef INSIDER_COMPILER_VARIABLE_HPP
#define INSIDER_COMPILER_VARIABLE_HPP

#include "vm/operand.hpp"

#include <optional>
#include <string>

namespace insider {

// The binding between a name and its value. For top-level values, this directly
// contains the index of the value. Otherwise, it's just an object representing
// the binding itself and the compiler will use these to translate them to local
// registers.
struct variable {
  std::string                     name;
  bool                            is_set = false;
  std::optional<insider::operand> global;

  explicit
  variable(std::string n) : name{std::move(n)} { }

  variable(std::string n, insider::operand index)
    : name{std::move(n)}
    , global{index}
  { }
};

} // namespace insider

#endif
