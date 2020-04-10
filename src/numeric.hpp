#ifndef SCHEME_NUMERIC_HPP
#define SCHEME_NUMERIC_HPP

#include "free_store.hpp"

#include <cassert>
#include <cstdint>
#include <vector>

namespace scm {

class boolean;
class context;
class module;
class port;

namespace detail {
  constexpr std::size_t limb_storage_width = 64;
  constexpr std::size_t limb_value_width = 63;

  using limb_type = std::uint64_t;
  using signed_limb_type = std::int64_t;

  constexpr unsigned
  highest_storage_bit(limb_type x) {
    return x >> (limb_storage_width - 1);
  }

  constexpr unsigned
  highest_value_bit(limb_type x) {
    return (x >> (limb_value_width - 1)) & 1;
  }

  inline void
  assert_normal(limb_type x) {
    assert(highest_storage_bit(x) == highest_value_bit(x));
  }
}

// A signed, fixed size integer.
class integer : public object {
public:
  using storage_type = detail::limb_type;
  using value_type = detail::signed_limb_type;

  integer() = default;
  integer(std::uint64_t value) : value_{value} { detail::assert_normal(value_); }
  integer(std::int64_t value) : value_{static_cast<std::uint64_t>(value)} { detail::assert_normal(value_); }
  integer(int value) : value_{static_cast<std::uint64_t>(value)} { detail::assert_normal(value_); }

  std::int64_t
  value() const { return static_cast<std::int64_t>(value_); }

  std::size_t
  hash() const override { return static_cast<std::size_t>(value_); }

  bool
  eqv(generic_ptr const& other) const override;

private:
  storage_type value_ = 0;
};

// An arbitray-length signed integer. It is made up of 63-bit limbs. Negative
// numbers are represented using two's complement.
class big_integer : public dynamic_size_object<big_integer, detail::limb_type> {
public:
  static std::size_t
  extra_storage_size(std::size_t length);

  explicit
  big_integer(std::size_t length);

  detail::limb_type*
  begin();

  detail::limb_type*
  end();

  std::size_t
  length() const { return length_; }

private:
  std::size_t length_;
};

generic_ptr
read_number(context&, ptr<port> const&, bool negate = false);

ptr<integer>
add(context&, ptr<integer> const&, ptr<integer> const&);
generic_ptr
add(context&, std::vector<generic_ptr> const&);

ptr<integer>
subtract(context&, ptr<integer> const&, ptr<integer> const&);
generic_ptr
subtract(context&, std::vector<generic_ptr> const&);

ptr<integer>
multiply(context&, ptr<integer> const&, ptr<integer> const&);
generic_ptr
multiply(context&, std::vector<generic_ptr> const&);

ptr<integer>
divide(context&, ptr<integer> const&, ptr<integer> const&);
generic_ptr
divide(context&, std::vector<generic_ptr> const&);

ptr<boolean>
arith_equal(context&, ptr<integer> const&, ptr<integer> const&);
generic_ptr
arith_equal(context&, std::vector<generic_ptr> const&);

ptr<boolean>
less(context&, ptr<integer> const&, ptr<integer> const&);
generic_ptr
less(context&, std::vector<generic_ptr> const&);

ptr<boolean>
greater(context&, ptr<integer> const&, ptr<integer> const&);
generic_ptr
greater(context&, std::vector<generic_ptr> const&);

void
export_numeric(context&, module&);

} // namespace scm

#endif
