#ifndef INSIDER_MEMORY_ROOT_PROVIDER_HPP
#define INSIDER_MEMORY_ROOT_PROVIDER_HPP

#include "object.hpp"

namespace insider {

class free_store;
class root_list;

// Base class for anything that contains GC roots.
class root_provider {
public:
  explicit
  root_provider(root_list&) noexcept;

  root_provider(root_provider const& other) noexcept {
    link_after(*other.prev_);
  }

  root_provider(root_provider&& other) noexcept {
    link_after(other);
    other.unlink();
  }

  root_provider&
  operator = (root_provider const& other) noexcept {
    if (this != &other) {
      unlink();
      link_after(*other.prev_);
    }

    return *this;
  }

  root_provider&
  operator = (root_provider&& other) noexcept {
    if (this != &other) {
      unlink();
      link_after(other);
      other.unlink();
    }

    return *this;
  }

  virtual
  ~root_provider() {
    unlink();
  }

  virtual void
  visit_roots(member_visitor const&) = 0;

private:
  friend class root_list;

  root_provider* next_ = nullptr;
  root_provider* prev_ = nullptr;

  root_provider() = default;

  void
  link_after(root_provider& other) {
    next_ = other.next_;
    prev_ = &other;
    other.next_ = this;
    if (next_)
      next_->prev_ = this;
  }

  void
  unlink() {
    if (prev_)
      prev_->next_ = next_;
    if (next_)
      next_->prev_ = prev_;

    next_ = prev_ = nullptr;
  }
};

// List of GC roots, i.e. of root_provider's.
class root_list {
public:
  void
  visit_roots(member_visitor const&) const;

private:
  friend class root_provider;

  class : public root_provider {
    void
    visit_roots(member_visitor const&) override { }
  } head_;
};

inline
root_provider::root_provider(root_list& list) noexcept {
  link_after(list.head_);
}

} // namespace insider

#endif
