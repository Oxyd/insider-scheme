#ifndef INSIDER_COMPARE_HPP
#define INSIDER_COMPARE_HPP

#include "ptr.hpp"

#include <cstdint>

namespace insider {

class context;

std::size_t
hash(ptr<> x);

struct generic_ptr_hash {
  std::size_t
  operator () (tracked_ptr<> const& p) const { return hash(p.get()); }
};

bool
eqv(context&, ptr<> x, ptr<> y);

bool
equal(context&, ptr<>, ptr<>);

class eqv_compare {
public:
  explicit
  eqv_compare(context& ctx) : ctx_{ctx} { }

  bool
  operator () (tracked_ptr<> const& x, tracked_ptr<> const& y) const {
    return eqv(ctx_, x.get(), y.get());
  }

private:
  context& ctx_;
};

} // namespace insider

#endif
