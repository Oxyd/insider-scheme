#ifndef INSIDER_MEMORY_ROOT_PROVIDER_HPP
#define INSIDER_MEMORY_ROOT_PROVIDER_HPP

#include "memory/member_visitor.hpp"

namespace insider {

class free_store;
class root_list;

// Base class for anything that contains GC roots.
class root_provider {
public:
  explicit
  root_provider(free_store&);

  explicit
  root_provider(root_list&);

  root_provider(root_provider const&);
  root_provider(root_provider&&) noexcept;

  root_provider&
  operator = (root_provider const&) { return *this; }

  root_provider&
  operator = (root_provider&& other) noexcept;

  virtual
  ~root_provider();

  virtual void
  visit_roots(member_visitor const&) = 0;

  void
  update_index(std::size_t new_index) { index_ = new_index; }

  root_list&
  list() const { return *list_; }

private:
  root_list*  list_  = nullptr;
  std::size_t index_ = 0;

  void
  add_to_list();

  void
  remove_from_list();
};

} // namespace insider

#endif
