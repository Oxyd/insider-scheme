#ifndef INSIDER_COMPILER_VARIABLE_HPP
#define INSIDER_COMPILER_VARIABLE_HPP

#include "compiler/expression.hpp"
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
  explicit
  variable_base(std::string n) : name_{std::move(n)} { }

  std::string const&
  name() const { return name_; }

  bool
  is_set() const { return is_set_; }

  void
  mark_as_set() { is_set_ = true; }

  void
  mark_as_not_set() { is_set_ = false; }

  expression
  constant_initialiser() const { return constant_initialiser_; }

  void
  visit_members(member_visitor const& f) {
    constant_initialiser_.visit_members(f);
  }

protected:
  void
  set_constant_initialiser(free_store& fs, ptr<> self, expression e);

private:
  std::string name_;
  bool        is_set_ = false;
  expression  constant_initialiser_;
};

class local_variable
  : public composite_object<local_variable>
  , public variable_base
{
public:
  static constexpr char const* scheme_name = "insider::local_variable";

  explicit
  local_variable(std::string n) : variable_base{std::move(n)} { }

  void
  set_constant_initialiser(free_store& fs, expression e) {
    variable_base::set_constant_initialiser(fs, this, e);
  }
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

  void
  set_constant_initialiser(free_store& fs, expression e) {
    variable_base::set_constant_initialiser(fs, this, e);
  }
};

using variable = sum_type<local_variable, top_level_variable>;

} // namespace insider

#endif
