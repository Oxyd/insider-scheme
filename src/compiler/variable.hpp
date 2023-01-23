#ifndef INSIDER_COMPILER_VARIABLE_HPP
#define INSIDER_COMPILER_VARIABLE_HPP

#include "compiler/expression.hpp"
#include "object.hpp"
#include "util/sum_type.hpp"
#include "vm/operand.hpp"

#include <string>

namespace insider {

// A variable is set!-eliminable if it is never set! after being read. I.e. its
// lifetime consists of a single unconditional set!, followed by a read phase
// where it's only ever read.
//
// A set!-eliminable variable can be treated as if it were a constant
// initialised to the right-hand side of its last set!.
//
// Lambda expressions -- i.e. closure captures -- count as reads, with the
// exception of lambda expressions appearing in the right-hand side of a set!
// for this variable. I.e. a variable bound to a self-recursive procedure is
// set!-eliminable.
//
// A set! is unconditional if it is always executed, i.e. it isn't nested in an
// if expression that follows the variable declaration.
//
// Another special case is loop variables. They are mutated as the loop
// executes, but they don't require boxing.

struct variable_flags {
  bool is_set            = false;
  bool is_read           = false;
  bool is_set_eliminable = false;
  bool is_loop_variable  = false;
  bool is_self_variable  = false;
};

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

  variable_flags&
  flags() { return flags_; }

  expression
  constant_initialiser() const { return constant_initialiser_; }

  void
  visit_members(member_visitor const& f) const {
    constant_initialiser_.visit_members(f);
  }

protected:
  void
  set_constant_initialiser(free_store& fs, ptr<> self, expression e);

private:
  std::string    name_;
  variable_flags flags_;
  expression     constant_initialiser_;
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
