#ifndef INSIDER_NUMERIC_HPP
#define INSIDER_NUMERIC_HPP

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
ptr_to_integer(object* x) {
  assert(!is_object_ptr(x));
  return integer{detail::make_normal(fixnum_payload(x))};
}

inline object*
integer_to_ptr(integer i) {
  return fixnum_to_ptr(i.data());
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
  extra_elements(big_integer*);

  explicit
  big_integer(std::size_t length);

  big_integer(std::size_t length, dont_initialize_t);

  explicit
  big_integer(std::vector<limb_type> const&, bool positive = true);

  explicit
  big_integer(integer);

  explicit
  big_integer(big_integer*);

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
  fraction(object* numerator, object* denominator);

  object*
  numerator() const { return numerator_; };

  object*
  denominator() const { return denominator_; }

  void
  set_numerator(free_store& store, object* n) { numerator_ = n; store.notify_arc(this, n); }

  void
  set_denominator(free_store& store, object* d) { denominator_ = d; store.notify_arc(this, d); }

  void
  trace(tracing_context& tc) { tc.trace(numerator_); tc.trace(denominator_); }

  void
  update_references() { update_reference(numerator_); update_reference(denominator_); }

private:
  object* numerator_;
  object* denominator_;
};

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
is_integer(object*);

bool
is_number(object*);

object*
add(context&, object*, object*);
object*
add(context&, std::vector<object*> const&);

object*
subtract(context&, object*, object*);
object*
subtract(context&, std::vector<object*> const&);

object*
multiply(context&, object*, object*);
object*
multiply(context&, std::vector<object*> const&);

object*
truncate_quotient(context&, object*, object*);
object*
truncate_quotient(context&, std::vector<object*> const&);

std::tuple<object*, object*>
quotient_remainder(context&, object*, object*);

object*
divide(context&, object*, object*);

object*
arithmetic_shift(context&, object*, object*);

object*
bitwise_and(context&, object*, object*);

object*
bitwise_or(context&, object*, object*);

object*
bitwise_not(context&, object*);

boolean*
arith_equal(context&, object*, object*);
object*
arith_equal(context&, std::vector<object*> const&);

boolean*
less(context&, object*, object*);
object*
less(context&, std::vector<object*> const&);

boolean*
greater(context&, object*, object*);
object*
greater(context&, std::vector<object*> const&);

object*
gcd(context&, object*, object*);

object*
read_integer(context& ctx, std::string const& digits, unsigned base = 10);

object*
read_number(context&, port*);

void
write_number(context&, object* value, port* out);

void
export_numeric(context&, module&);

} // namespace insider

#endif
