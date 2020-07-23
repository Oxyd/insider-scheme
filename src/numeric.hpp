#ifndef SCHEME_NUMERIC_HPP
#define SCHEME_NUMERIC_HPP

#include "free_store.hpp"

#include <cassert>
#include <cstdint>
#include <vector>

namespace insider {

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
    (void) x;
    assert(highest_storage_bit(x) == highest_value_bit(x));
  }

  inline integer_storage_type
  make_normal(integer_storage_type x) {
    assert(highest_storage_bit(x) == 0);
    return x | (integer_storage_type{highest_value_bit(x)} << (short_integer_storage_width - 1));
  }
}

// A signed, fixed size integer.
class integer {
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

private:
  storage_type value_ = 0;
};

inline integer
ptr_to_integer(generic_ptr const& x) {
  assert(!is_object_ptr(x.get()));
  return integer{detail::make_normal(fixnum_payload(x.get()))};
}

inline generic_ptr
integer_to_ptr(integer i) {
  return generic_ptr{i.data()};
}

inline std::size_t
integer_hash(integer i) { return static_cast<std::size_t>(i.data()); }

// An arbitray-length signed magnitude integer. It is made up of 64-bit unsigned
// limbs. The least-significant limb is stored first.
class big_integer : public dynamic_size_object<big_integer, detail::limb_type> {
public:
  struct dont_initialize_t { } static constexpr dont_initialize{};
  using iterator = detail::limb_type*;
  using reverse_iterator = std::reverse_iterator<iterator>;
  using limb_type = detail::limb_type;

  static std::size_t
  extra_elements(std::size_t length);

  static std::size_t
  extra_elements(std::size_t length, dont_initialize_t) { return big_integer::extra_elements(length); }

  static std::size_t
  extra_elements(std::vector<limb_type> const&, bool = true);

  static std::size_t
  extra_elements(integer);

  static std::size_t
  extra_elements(ptr<big_integer> const&);

  explicit
  big_integer(std::size_t length);

  big_integer(std::size_t length, dont_initialize_t);

  explicit
  big_integer(std::vector<limb_type> const&, bool positive = true);

  explicit
  big_integer(integer);

  explicit
  big_integer(ptr<big_integer> const&);

  big_integer(big_integer&&);

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

  std::size_t
  size() const { return length(); }

  bool
  zero() const { return length_ == 0; }

  bool
  positive() const { return positive_; }

  void
  set_positive(bool p) { positive_ = p; }

  void
  trace(tracing_context&) { }

  void
  update_references() { }

private:
  std::size_t length_;
  bool positive_ = true;
};

class fraction : public composite_object<fraction> {
public:
  fraction(generic_ptr const& numerator, generic_ptr const& denominator);

  generic_ptr
  numerator(free_store& store) const { return {store, numerator_}; };

  generic_ptr
  denominator(free_store& store) const { return {store, denominator_}; }

  void
  set_numerator(generic_ptr const& n) { numerator_ = n.get(); }

  void
  set_denominator(generic_ptr const& d) { denominator_ = d.get(); }

  void
  trace(tracing_context& tc) { tc.trace(numerator_); tc.trace(denominator_); }

  void
  update_references() { update_reference(numerator_); update_reference(denominator_); }

private:
  object* numerator_;
  object* denominator_;
};

inline generic_ptr
fraction_numerator(ptr<fraction> const& f) { return f->numerator(f.store()); }

inline generic_ptr
fraction_denominator(ptr<fraction> const& f) { return f->denominator(f.store()); }

class floating_point : public leaf_object<floating_point> {
public:
  using value_type = double;

  value_type value;

  static constexpr value_type positive_infinity = std::numeric_limits<value_type>::infinity();
  static constexpr value_type negative_infinity = -std::numeric_limits<value_type>::infinity();
  static constexpr value_type positive_nan      = std::numeric_limits<value_type>::quiet_NaN();
  static constexpr value_type negative_nan      = -std::numeric_limits<value_type>::quiet_NaN();

  explicit
  floating_point(double v) : value{v} { }
};

bool
is_integer(generic_ptr const&);

bool
is_number(generic_ptr const&);

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
truncate_quotient(context&, generic_ptr const&, generic_ptr const&);
generic_ptr
truncate_quotient(context&, std::vector<generic_ptr> const&);

std::tuple<generic_ptr, generic_ptr>
quotient_remainder(context&, generic_ptr const&, generic_ptr const&);

generic_ptr
divide(context&, generic_ptr const&, generic_ptr const&);

generic_ptr
arithmetic_shift(context&, generic_ptr const&, generic_ptr const&);

generic_ptr
bitwise_and(context&, generic_ptr const&, generic_ptr const&);

generic_ptr
bitwise_or(context&, generic_ptr const&, generic_ptr const&);

generic_ptr
bitwise_not(context&, generic_ptr const&);

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
gcd(context&, generic_ptr const&, generic_ptr const&);

generic_ptr
read_integer(context& ctx, std::string const& digits, unsigned base = 10);

generic_ptr
read_number(context&, ptr<port> const&);

void
write_number(context&, generic_ptr const& value, ptr<port> const& out);

void
export_numeric(context&, module&);

} // namespace insider

#endif
