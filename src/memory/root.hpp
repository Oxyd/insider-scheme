#ifndef INSIDER_MEMORY_ROOT_HPP
#define INSIDER_MEMORY_ROOT_HPP

#include "memory/free_store.hpp"
#include "memory/root_list.hpp"
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

  root(root_list& list, T const& value)
    : root_provider{list}
    , value_{value}
  { }

  root(root_list& list, T&& value)
    : root_provider{list}
    , value_{std::move(value)}
  { }

  root(free_store& fs, T const& value)
    : root_provider{fs}
    , value_{value}
  { }

  root(free_store& fs, T&& value)
    : root_provider{fs}
    , value_{std::move(value)}
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
  get() noexcept { return value_; }

  T const&
  get() const noexcept { return value_; }

  T&
  operator -> () noexcept { return value_; }

  T const&
  operator -> () const noexcept { return value_; }

  explicit operator bool () const { return value_.operator bool (); }

private:
  T value_;

  void
  visit_roots(member_visitor const& f) override {
    visit_members(f, value_);
  }
};

template <typename T = void>
using root_ptr = root<ptr<T>>;

} // namespace insider

#endif
