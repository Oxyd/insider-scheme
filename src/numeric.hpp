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
  constexpr std::size_t short_integer_storage_width = 64;
  constexpr std::size_t short_integer_value_width = 63;

#if defined __GNUC__ || defined __clang__
  #define INSIDER_SCHEME_LIMB_WIDTH 64
  using limb_type = std::uint64_t;
  using double_limb_type = __uint128_t;
#else
  #define INSIDER_SCHEME_LIMB_WIDTH 32
  using limb_type = std::uint32_t;
  using double_limb_type = std::uint64_t;
#endif

  using integer_storage_type = std::uint64_t;
  using integer_value_type = std::int64_t;

  constexpr unsigned
  highest_storage_bit(integer_storage_type x) {
    return x >> (short_integer_storage_width - 1);
  }

  constexpr unsigned
  highest_value_bit(integer_storage_type x) {
    return (x >> (short_integer_value_width - 1)) & 1;
  }

  inline void
  assert_normal(integer_storage_type x) {
    assert(highest_storage_bit(x) == highest_value_bit(x));
  }
}

// A signed, fixed size integer.
class integer : public object {
public:
  using storage_type = detail::integer_storage_type;
  using value_type = detail::integer_value_type;

  static constexpr value_type max = (value_type{1} << (detail::short_integer_value_width - 1)) - 1;
  static constexpr value_type min = -max - 1;

  integer() = default;
  integer(storage_type value) : value_{value} { detail::assert_normal(value_); }
  integer(value_type value) : value_{static_cast<storage_type>(value)} { detail::assert_normal(value_); }
  integer(int value) : value_{static_cast<storage_type>(value)} { detail::assert_normal(value_); }

  value_type
  value() const { return static_cast<value_type>(value_); }

  void
  set_value(storage_type v) { value_ = v; }

  storage_type
  data() const { return value_; }

  std::size_t
  hash() const override { return static_cast<std::size_t>(value_); }

  bool
  eqv(generic_ptr const& other) const override;

private:
  storage_type value_ = 0;
};

// An arbitray-length signed magnitude integer. It is made up of 64-bit unsigned
// limbs. The least-significant limb is stored first.
class big_integer : public dynamic_size_object<big_integer, detail::limb_type> {
public:
  struct dont_initialize_t { } static constexpr dont_initialize{};
  using iterator = detail::limb_type*;
  using reverse_iterator = std::reverse_iterator<iterator>;
  using limb_type = detail::limb_type;

  static std::size_t
  extra_storage_size(std::size_t length);

  static std::size_t
  extra_storage_size(std::size_t length, dont_initialize_t) { return big_integer::extra_storage_size(length); }

  static std::size_t
  extra_storage_size(std::vector<limb_type> const&, bool = true);

  static std::size_t
  extra_storage_size(ptr<integer> const&);

  static std::size_t
  extra_storage_size(ptr<big_integer> const&);

  explicit
  big_integer(std::size_t length);

  big_integer(std::size_t length, dont_initialize_t);

  explicit
  big_integer(std::vector<limb_type> const&, bool positive = true);

  explicit
  big_integer(ptr<integer> const&);

  explicit
  big_integer(ptr<big_integer> const&);

  iterator
  begin();

  iterator
  end();

  reverse_iterator
  rbegin();

  reverse_iterator
  rend();

  limb_type&
  front() { return *begin(); }

  limb_type&
  back() { return *(end() - 1); }

  limb_type*
  data() { return begin(); }

  std::size_t
  length() const { return length_; }

  bool
  zero() const { return length_ == 0; }

  bool
  positive() const { return positive_; }

  void
  set_positive(bool p) { positive_ = p; }

private:
  std::size_t length_;
  bool positive_ = true;
};

generic_ptr
add(context&, generic_ptr const&, generic_ptr const&);
generic_ptr
add(context&, std::vector<generic_ptr> const&);

generic_ptr
subtract(context&, generic_ptr const&, generic_ptr const&);
generic_ptr
subtract(context&, std::vector<generic_ptr> const&);

generic_ptr
multiply(context&, generic_ptr const&, generic_ptr const&);
generic_ptr
multiply(context&, std::vector<generic_ptr> const&);

generic_ptr
divide(context&, generic_ptr const&, generic_ptr const&);
generic_ptr
divide(context&, std::vector<generic_ptr> const&);

std::tuple<generic_ptr, generic_ptr>
quotient_remainder(context&, generic_ptr const&, generic_ptr const&);

ptr<boolean>
arith_equal(context&, generic_ptr const&, generic_ptr const&);
generic_ptr
arith_equal(context&, std::vector<generic_ptr> const&);

ptr<boolean>
less(context&, generic_ptr const&, generic_ptr const&);
generic_ptr
less(context&, std::vector<generic_ptr> const&);

ptr<boolean>
greater(context&, generic_ptr const&, generic_ptr const&);
generic_ptr
greater(context&, std::vector<generic_ptr> const&);

generic_ptr
read_number(context&, ptr<port> const&, bool negate = false);

void
write_number(ptr<integer> const& value, ptr<port> const& out);

void
export_numeric(context&, module&);

} // namespace scm

#endif
