#ifndef INSIDER_NUMERIC_HPP
#define INSIDER_NUMERIC_HPP

#include "free_store.hpp"
#include "integer.hpp"
#include "object_span.hpp"

#include <cassert>
#include <cstdint>
#include <limits>
#include <vector>

namespace insider {

class boolean;
class context;
class module_;
class reader_stream;
class textual_output_port;

namespace detail {
#if defined __GNUC__ || defined __clang__
  constexpr std::size_t limb_width = 64;
  using limb_type = std::uint64_t;
  using double_limb_type = __uint128_t;
#else
  constexpr std::size_t limb_width = 32;
  using limb_type = std::uint32_t;
  using double_limb_type = std::uint64_t;
#endif

  using integer_value_type = integer::value_type;
}

// An arbitray-length signed magnitude integer. It is made up of 64-bit unsigned
// limbs. The least-significant limb is stored first.
class big_integer : public dynamic_size_object<big_integer, detail::limb_type> {
public:
  static constexpr char const* scheme_name = "insider::big_integer";

  struct dont_initialize_t { } static constexpr dont_initialize{};
  using iterator = detail::limb_type*;
  using reverse_iterator = std::reverse_iterator<iterator>;
  using limb_type = detail::limb_type;
  static constexpr std::size_t limb_width = detail::limb_width;

  static std::size_t
  extra_elements(std::size_t length);

  static std::size_t
  extra_elements(std::size_t length, dont_initialize_t) { return big_integer::extra_elements(length); }

  static std::size_t
  extra_elements(std::vector<limb_type> const&, bool = true);

  static std::size_t
  extra_elements(integer);

  static std::size_t
  extra_elements(ptr<big_integer>);

  explicit
  big_integer(std::size_t length);

  big_integer(std::size_t length, dont_initialize_t);

  explicit
  big_integer(std::vector<limb_type> const&, bool positive = true);

  explicit
  big_integer(integer);

  explicit
  big_integer(ptr<big_integer>);

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
  length() const { return size(); }

  bool
  zero() const { return size() == 0; }

  bool
  positive() const { return positive_; }

  void
  set_positive(bool p) { positive_ = p; }

  void
  visit_members(member_visitor const&) { }

  std::size_t
  hash() const;

private:
  bool positive_ = true;
};

class fraction : public composite_object<fraction> {
public:
  static constexpr char const* scheme_name = "insider::fraction";

  fraction(ptr<> numerator, ptr<> denominator);

  ptr<>
  numerator() const { return numerator_; };

  ptr<>
  denominator() const { return denominator_; }

  void
  set_numerator(free_store& store, ptr<> n) { numerator_ = n; store.notify_arc(this, n); }

  void
  set_denominator(free_store& store, ptr<> d) { denominator_ = d; store.notify_arc(this, d); }

  void
  visit_members(member_visitor const& f) { f(numerator_); f(denominator_); }

  std::size_t
  hash() const;

private:
  ptr<> numerator_;
  ptr<> denominator_;
};

ptr<>
make_fraction(context& ctx, ptr<> num, ptr<> den);

ptr<>
normalize_fraction(context& ctx, ptr<fraction> q);

class floating_point : public leaf_object<floating_point> {
public:
  static constexpr char const* scheme_name = "insider::floating_point";

  using value_type = double;

  value_type value;

  static constexpr value_type positive_infinity = std::numeric_limits<value_type>::infinity();
  static constexpr value_type negative_infinity = -std::numeric_limits<value_type>::infinity();
  static constexpr value_type positive_nan      = std::numeric_limits<value_type>::quiet_NaN();
  static constexpr value_type negative_nan      = -std::numeric_limits<value_type>::quiet_NaN();

  explicit
  floating_point(double v) : value{v} { }

  std::size_t
  hash() const;
};

class complex : public composite_object<complex> {
public:
  static constexpr char const* scheme_name = "insider::complex";

  complex(ptr<> r, ptr<> i) : real_{r}, imaginary_{i} { }

  ptr<>
  real() const { return real_; }

  ptr<>
  imaginary() const { return imaginary_; }

  void
  set_real(free_store& fs, ptr<> r) { real_ = r; fs.notify_arc(this, r); }

  void
  set_imaginary(free_store& fs, ptr<> i) { imaginary_ = i; fs.notify_arc(this, i); }

  void
  visit_members(member_visitor const& f) { f(real_); f(imaginary_); }

  std::size_t
  hash() const;

private:
  ptr<> real_;
  ptr<> imaginary_;
};

ptr<>
make_rectangular(context&, ptr<> real, ptr<> imaginary);

ptr<>
make_polar(context&, ptr<> magnitude, ptr<> angle);

ptr<>
magnitude(context&, ptr<>);

ptr<>
angle(context&, ptr<>);

bool
is_exact_integer(ptr<>);

bool
is_integer(ptr<>);

bool
is_number(ptr<>);

bool
is_real(ptr<>);

bool
is_exact(ptr<>);

bool
is_inexact(ptr<>);

bool
is_nan(ptr<>);

bool
is_finite(ptr<>);

bool
is_infinite(ptr<>);

bool
is_positive(ptr<>);

bool
is_negative(ptr<>);

bool
is_zero(ptr<>);

bool
is_exactly_equal_to(ptr<>, integer::value_type);

bool
is_odd(ptr<>);

bool
is_even(ptr<>);

ptr<>
negate(context&, ptr<>);

ptr<>
add(context&, ptr<>, ptr<>);
ptr<>
add(context&, object_span);

inline bool
overflow(integer::value_type i) {
  return i > integer::max || i < integer::min;
}

inline ptr<>
add_fixnums(integer::value_type x, integer::value_type y) {
  integer::value_type result = x + y;
  if (overflow(result))
    return nullptr;
  else
    return integer_to_ptr(result);
}

ptr<>
subtract(context&, ptr<>, ptr<>);
ptr<>
subtract(context&, object_span);

inline ptr<>
subtract_fixnums(integer::value_type x, integer::value_type y) {
  integer::value_type result = x - y;
  if (overflow(result))
    return nullptr;
  else
    return integer_to_ptr(result);
}

ptr<>
multiply(context&, ptr<>, ptr<>);
ptr<>
multiply(context&, object_span);

inline bool
small_mul_overflow(integer::value_type x, integer::value_type y) {
  assert(y != 0);
  return std::abs(x) > integer::max / std::abs(y);
}

inline ptr<>
multiply_fixnums(integer::value_type lhs, integer::value_type rhs) {
  if (rhs == 0)
    return integer_to_ptr(0);

  integer::value_type x = lhs > 0 ? lhs : -lhs;
  integer::value_type y = rhs > 0 ? rhs : -rhs;
  bool result_positive = (lhs > 0) == (rhs > 0);

  if (small_mul_overflow(x, y))
    return nullptr;
  else {
    integer::value_type product = x * y;
    return integer_to_ptr(integer{result_positive ? product : -product});
  }
}

ptr<>
truncate_quotient(context&, ptr<>, ptr<>);
ptr<>
truncate_quotient(context&, object_span);

std::tuple<ptr<>, ptr<>>
quotient_remainder(context&, ptr<>, ptr<>);

ptr<>
divide(context&, ptr<>, ptr<>);
ptr<>
divide(context&, object_span);

ptr<>
conjugate(context&, ptr<>);

ptr<>
arithmetic_shift(context&, ptr<>, ptr<>);

ptr<>
bitwise_and(context&, ptr<>, ptr<>);

ptr<>
bitwise_or(context&, ptr<>, ptr<>);

ptr<>
bitwise_not(context&, ptr<>);

std::size_t
bit_length(context&, ptr<>);

ptr<boolean>
arith_equal(context&, ptr<>, ptr<>);
ptr<>
arith_equal(context&, object_span);

ptr<boolean>
less(context&, ptr<>, ptr<>);
ptr<>
less(context&, object_span);

ptr<boolean>
greater(context&, ptr<>, ptr<>);
ptr<>
greater(context&, object_span);

ptr<boolean>
less_or_equal(context&, ptr<>, ptr<>);
ptr<>
less_or_equal(context&, object_span);

ptr<boolean>
greater_or_equal(context&, ptr<>, ptr<>);
ptr<>
greater_or_equal(context&, object_span);

ptr<>
gcd(context&, ptr<>, ptr<>);

template <int Base>
ptr<>
integer_power(context& ctx, unsigned exponent) {
  ptr<> result = integer_to_ptr(1);
  while (exponent > 0) {
    result = multiply(ctx, result, integer_to_ptr(Base));
    exponent -= 1;
  }

  return result;
}

template <>
ptr<>
integer_power<2>(context&, unsigned);

ptr<>
inexact(context&, ptr<>);

ptr<>
exact(context&, ptr<>);

ptr<>
exp(context&, ptr<>);

ptr<>
log(context&, ptr<>);

ptr<>
sin(context&, ptr<>);

ptr<>
cos(context&, ptr<>);

ptr<>
tan(context&, ptr<>);

ptr<>
asin(context&, ptr<>);

ptr<>
acos(context&, ptr<>);

ptr<>
atan(context&, ptr<>);

ptr<>
atan2(context&, ptr<>, ptr<>);

ptr<>
square(context&, ptr<>);

ptr<>
sqrt(context&, ptr<>);

ptr<>
expt(context&, ptr<> base, ptr<> exponent);

ptr<>
read_integer(context& ctx, std::string const& digits, unsigned base = 10);

} // namespace insider

#endif
