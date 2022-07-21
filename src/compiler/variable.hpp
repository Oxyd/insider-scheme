#ifndef INSIDER_COMPILER_VARIABLE_HPP
#define INSIDER_COMPILER_VARIABLE_HPP

#include "object.hpp"
#include "ptr.hpp"
#include "util/sum_type.hpp"
#include "vm/operand.hpp"

#include <string>

namespace insider {

// The binding between a name and its value. For top-level values, this directly
// contains the index of the value. Otherwise, it's just an object representing
// the binding itself and the compiler will use these to translate them to local
// registers.
//
// This also contains compile-time information about each variable that's used
// during translation.

class variable_base {
public:
  std::string name;
  bool        is_set = false;
  ptr<>       constant_value;

  explicit
  variable_base(std::string n) : name{std::move(n)} { }

  void
  visit_members(member_visitor const& f) {
    f(constant_value);
  }
};

class local_variable
  : public composite_object<local_variable>
  , public variable_base
{
public:
  static constexpr char const* scheme_name = "insider::local_variable";

  explicit
  local_variable(std::string n) : variable_base{std::move(n)} { }
};

class top_level_variable
  : public composite_object<top_level_variable>
  , public variable_base
{
public:
  static constexpr char const* scheme_name = "insider::top_level_variable";

  insider::operand index;

  top_level_variable(std::string n, insider::operand index)
    : variable_base{std::move(n)}
    , index{index}
  { }
};

using variable = sum_type<local_variable, top_level_variable>;

} // namespace insider

#endif
