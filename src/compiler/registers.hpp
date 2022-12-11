#ifndef INSIDER_COMPILER_REGISTERS_HPP
#define INSIDER_COMPILER_REGISTERS_HPP

#include "compiler/variable.hpp"
#include "vm/operand.hpp"

#include <cassert>
#include <optional>
#include <set>
#include <unordered_map>

namespace insider {

class shared_register;

class register_allocator {
public:
  shared_register
  allocate_register();

  shared_register
  allocate_argument_register();

  void
  ref(operand);

  void
  unref(operand);

  unsigned
  registers_used() const { return registers_max_; }

  operand
  first_argument_register() const;

private:
  operand                               registers_max_ = 0;
  std::set<operand>                     registers_free_;
  std::unordered_map<operand, unsigned> register_ref_counts_;
};

class shared_register {
public:
  shared_register() = default;
  shared_register(register_allocator&, operand);
  shared_register(shared_register const&);
  shared_register(shared_register&&) noexcept;
  ~shared_register();

  shared_register&
  operator = (shared_register&&) noexcept;

  operand
  operator * () const { assert(has_value()); return *value_; }

  bool
  has_value() const { return (bool) value_; }

  explicit operator
  bool () const { return has_value(); }

  bool
  operator == (shared_register const& other) const {
    return alloc_ == other.alloc_ && value_ == other.value_;
  }

  bool
  operator != (shared_register const& other) const {
    return !operator == (other);
  }

private:
  register_allocator*    alloc_ = nullptr;
  std::optional<operand> value_;
};

class variable_bindings {
public:
  class binding {
  public:
    ptr<local_variable> variable;
    shared_register     destination;

    static binding
    register_binding(ptr<local_variable> variable, shared_register destination) {
      return binding{variable, std::move(destination)};
    }

  private:
    binding(ptr<local_variable> variable, shared_register destination)
      : variable{variable}
      , destination{std::move(destination)}
    { }
  };

  using scope = std::vector<binding>;
  using free_variables_map = std::unordered_map<ptr<local_variable>,
                                                shared_register>;

  class unique_scope {
  public:
    explicit
    unique_scope(variable_bindings* b) : b_{b} { }
    unique_scope(unique_scope const&) = delete;
    void operator = (unique_scope const&) = delete;

    ~unique_scope() { b_->pop_scope(); }

  private:
    variable_bindings* b_;
  };

  unique_scope
  push_scope(scope = {});

  void
  pop_scope();

  shared_register
  lookup(ptr<local_variable> const&) const;

  bool
  register_is_store_for_variable(shared_register const&) const;

private:
  std::vector<scope> scopes_;
};

} // namespace insider

#endif
