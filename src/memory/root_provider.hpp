#ifndef INSIDER_MEMORY_ROOT_PROVIDER_HPP
#define INSIDER_MEMORY_ROOT_PROVIDER_HPP

#include "object.hpp"

namespace insider {

class free_store;
class root_provider;

// List of GC roots, i.e. of root_provider's.
class root_list {
public:
  std::size_t
  add_provider(root_provider*);

  void
  remove_provider(std::size_t);

  void
  update_provider(std::size_t, root_provider*);

  void
  visit_roots(member_visitor const&) const;

  void
  compact();

private:
  std::vector<root_provider*> providers_;
  std::vector<std::size_t>    free_indices_;

  std::size_t
  find_free_index();
};

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
