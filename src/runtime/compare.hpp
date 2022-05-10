#ifndef INSIDER_RUNTIME_COMPARE_HPP
#define INSIDER_RUNTIME_COMPARE_HPP

#include "memory/tracked_ptr.hpp"
#include "ptr.hpp"

#include <cstdint>
#include <unordered_map>
#include <unordered_set>

namespace insider {

class context;

// Get the object's hash value. Two distinct objects may have different hash
// values, even if they both represent the same value. I.e. this hash
// corresponds to the eq? predicate.
std::size_t
hash(ptr<> x);

// Get the object's hash value that is acceptable for the eqv?
// predicate. I.e. if two values are eqv? to each other, they have the same
// hasheqv value.
std::size_t
hasheqv(ptr<>);

struct ptr_hash {
  std::size_t
  operator () (ptr<> p) const { return hash(p); }

  std::size_t
  operator () (tracked_ptr<> const& p) const { return hash(p.get()); }
};

struct ptr_hasheqv {
  std::size_t
  operator () (ptr<> p) const { return hasheqv(p); }

  std::size_t
  operator () (tracked_ptr<> const& p) const { return hasheqv(p.get()); }
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
  operator () (ptr<> x, ptr<> y) const {
    return eqv(ctx_, x, y);
  }

  bool
  operator () (tracked_ptr<> const& x, tracked_ptr<> const& y) const {
    return eqv(ctx_, x.get(), y.get());
  }

private:
  context& ctx_;
};

template <typename Key, typename Value>
using eq_unordered_map = std::unordered_map<Key, Value, ptr_hash>;

template <typename Key>
using eq_unordered_set = std::unordered_set<Key, ptr_hash>;

template <typename Key, typename Value>
using eqv_unordered_map
  = std::unordered_map<Key, Value, ptr_hasheqv, eqv_compare>;

} // namespace insider

#endif
