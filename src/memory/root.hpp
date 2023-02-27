#ifndef INSIDER_MEMORY_ROOT_HPP
#define INSIDER_MEMORY_ROOT_HPP

#include "memory/free_store.hpp"
#include "memory/member_visitor.hpp"
#include "memory/root_provider.hpp"

#include <utility>

namespace insider {

template <typename T>
class root : public root_provider {
public:
  explicit
  root(free_store& fs)
    : root_provider{fs}
  { }

  root(free_store& fs, T&& value)
    : root_provider{fs}
    , value_{std::forward<T>(value)}
  { }

  root(root const&) = default;

  root(root&&) noexcept = default;

  root&
  operator = (root const&) = default;

  root&
  operator = (root&&) noexcept = default;

  root&
  operator = (T&& value) {
    value_ = std::forward<T>(value);
    return *this;
  }

  T&
  get() { return value_; }

  T const&
  get() const { return value_; }

private:
  T value_;

  void
  visit_roots(member_visitor const& f) override {
    visit_members(f, value_);
  }
};

} // namespace insider

#endif
