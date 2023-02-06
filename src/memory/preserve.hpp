#ifndef INSIDER_MEMORY_PRESERVER_HPP
#define INSIDER_MEMORY_PRESERVER_HPP

#include "context.hpp"
#include "memory/root_provider.hpp"

#include <tuple>

namespace insider {

template <typename T>
concept visitable = requires (T& t, member_visitor const& f) {
  t.visit_members(f);
};

namespace detail {

  inline void
  visit_members(member_visitor const& f, ptr<>& p) {
    f(p);
  }

  void
  visit_members(member_visitor const& f, visitable auto& x) {
    x.visit_members(f);
  }

  template <typename T>
  void
  visit_members(member_visitor const& f, std::vector<T>& v) {
    for (auto& p : v)
      visit_members(f, p);
  }

} // namespace detail

// Root provider for a list of local variables.
template <typename... Ts>
class scoped_roots : public root_provider {
public:
  [[nodiscard]] explicit
  scoped_roots(context& ctx, Ts&... xs)
    : root_provider(ctx.store)
    , ptrs_{xs...}
  { }

  void
  visit_roots(member_visitor const& f) override {
    std::apply([&] (auto&... elems) { (detail::visit_members(f, elems), ...); },
               ptrs_);
  }

private:
  std::tuple<Ts&...> ptrs_;
};

[[nodiscard]]
inline auto
preserve(context& ctx, auto&... objects) {
  return scoped_roots{ctx, objects...};
}

} // namespace insider

#endif
