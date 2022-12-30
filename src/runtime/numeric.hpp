#ifndef INSIDER_RUNTIME_NUMERIC_HPP
#define INSIDER_RUNTIME_NUMERIC_HPP

#include "memory/free_store.hpp"
#include "runtime/integer.hpp"
#include "type_indexes.hpp"
#include "util/object_span.hpp"

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
  static constexpr word_type static_type_index = type_indexes::big_integer;

  struct dont_initialize_t { } static constexpr dont_initialize{};
  using iterator = detail::limb_type*;
  using const_iterator = detail::limb_type const*;
  using reverse_iterator = std::reverse_iterator<iterator>;
  using limb_type = detail::limb_type;
  static constexpr std::size_t limb_width = detail::limb_width;

  static std::size_t
  extra_elements(std::size_t length);

  static std::size_t
  extra_elements(std::size_t length, dont_initialize_t) {
    return big_integer::extra_elements(length);
  }

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

  big_integer(big_integer&&) noexcept;

  iterator
  begin();

  iterator
  end();

  const_iterator
  begin() const;

  const_iterator
  end() const;

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

  limb_type const*
  data() const { return begin(); }

  limb_type&
  nth_limb(std::size_t i) { assert(i < size()); return data()[i]; }

  limb_type
  nth_limb(std::size_t i) const { assert(i < size()); return data()[i]; }

  limb_type
  nth_limb_or_zero(std::size_t i) const {
    if (i < size())
      return data()[i];
    else
      return {};
  }

  std::size_t
  length() const { return size(); }

  bool
  zero() const { return size() == 0; }

  bool
  positive() const { return positive_; }

  void
  set_positive(bool p) { positive_ = p; }

  void
  visit_members(member_visitor const&) const { }

  std::size_t
  hash() const;

private:
  bool positive_ = true;
};

class fraction : public composite_object<fraction> {
public:
  static constexpr char const* scheme_name = "insider::fraction";
  static constexpr word_type static_type_index = type_indexes::fraction;

  fraction(ptr<> numerator, ptr<> denominator);

  ptr<>
  numerator() const { return numerator_; };

  ptr<>
  denominator() const { return denominator_; }

  void
  visit_members(member_visitor const& f) const {
    f(numerator_);
    f(denominator_);
  }

  std::size_t
  hash() const;

private:
  ptr<> const numerator_;
  ptr<> const denominator_;
};

ptr<>
make_fraction(context& ctx, ptr<> num, ptr<> den);

ptr<>
normalize_fraction(context& ctx, ptr<fraction> q);

class floating_point : public leaf_object<floating_point> {
public:
  static constexpr char const* scheme_name = "insider::floating_point";
  static constexpr word_type static_type_index = type_indexes::floating_point;

  using value_type = double;

  value_type value;

  static constexpr value_type positive_infinity
    = std::numeric_limits<value_type>::infinity();
  static constexpr value_type negative_infinity
    = -std::numeric_limits<value_type>::infinity();
  static constexpr value_type positive_nan
    = std::numeric_limits<value_type>::quiet_NaN();
  static constexpr value_type negative_nan
    = -std::numeric_limits<value_type>::quiet_NaN();

  explicit
  floating_point(double v) : value{v} { }

  std::size_t
  hash() const;
};

class complex : public composite_object<complex> {
public:
  static constexpr char const* scheme_name = "insider::complex";
  static constexpr word_type static_type_index = type_indexes::complex;

  complex(ptr<> r, ptr<> i) : real_{r}, imaginary_{i} { }

  ptr<>
  real() const { return real_; }

  ptr<>
  imaginary() const { return imaginary_; }

  void
  visit_members(member_visitor const& f) const {
    f(real_);
    f(imaginary_);
  }

  std::size_t
  hash() const;

private:
  ptr<> const real_;
  ptr<> const imaginary_;
};

namespace detail {
  template <typename T>
  std::size_t
  number_of_limbs_for_small_integer(T i) {
    constexpr limb_type max_limb_value = std::numeric_limits<limb_type>::max();

    if (i == 0)
      return 0;

    if constexpr (sizeof(limb_type) >= sizeof(integer::value_type))
      return 1;
    else {
      if (i <= static_cast<integer::value_type>(max_limb_value)
          && -i <= static_cast<integer::value_type>(max_limb_value))
        return 1;
      else
        return 2;
    }
  }

  template <typename T>
  std::tuple<bool, T>
  integer_to_sign_magnitude(T i) {
    if constexpr (std::is_signed_v<T>) {
      if (i >= 0)
        return {true, i};
      else
        return {false, -i};
    } else
      return {true, i};
  }

  template <typename T>
  void
  make_bignum_limbs_from_integer(big_integer& b, T value) {
    auto [sign, magnitude] = integer_to_sign_magnitude(value);
    b.set_positive(sign);

    if constexpr (sizeof(T) <= sizeof(limb_type))
      b.front() = magnitude;
    else {
      constexpr double_limb_type limb_mask
        = (double_limb_type{1} << limb_width) - 1;

      auto it = b.begin();
      for (std::size_t k = 0; k < b.length(); ++k) {
        *it++ = magnitude & limb_mask;
        magnitude >>= limb_width;
      }
    }
  }

  template <typename T>
  ptr<big_integer>
  integer_to_bignum(context& ctx, T value) {
    auto b = make<big_integer>(ctx, number_of_limbs_for_small_integer(value));
    make_bignum_limbs_from_integer(*b, value);
    return b;
  }
} // namespace detail

template <typename T>
ptr<>
integer_to_scheme(context& ctx, T value) {
  if (in_fixnum_range(value))
    return integer_to_ptr(static_cast<integer::value_type>(value));
  else
    return detail::integer_to_bignum(ctx, value);
}

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
is_rational(ptr<>);

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

inline ptr<>
subtract_fixnums(integer::value_type x, integer::value_type y) {
  integer::value_type result = x - y;
  if (overflow(result))
    return nullptr;
  else
    return integer_to_ptr(result);
}

ptr<>
increment(context&, ptr<>);

ptr<>
decrement(context&, ptr<>);

ptr<>
multiply(context&, ptr<>, ptr<>);

inline bool
small_mul_overflow(integer::value_type x, integer::value_type y) {
  assert(x >= 0);
  assert(y > 0);
  return x > integer::max / y;
}

inline ptr<>
multiply_fixnums(integer::value_type lhs, integer::value_type rhs) {
  if (rhs == 0)
    return integer_to_ptr(0);

#if defined __GNUC__ || defined __clang__
  integer::value_type result;
  bool overflowed = __builtin_smull_overflow(lhs, rhs, &result);
  if (overflowed || overflow(result))
    return {};
  else
    return integer_to_ptr(result);
#else
  integer::value_type x = lhs > 0 ? lhs : -lhs;
  integer::value_type y = rhs > 0 ? rhs : -rhs;
  bool result_positive = (lhs > 0) == (rhs > 0);

  if (small_mul_overflow(x, y))
    return nullptr;
  else {
    integer::value_type product = x * y;
    return integer_to_ptr(integer{result_positive ? product : -product});
  }
#endif
}

std::tuple<ptr<>, ptr<>>
quotient_remainder(context&, ptr<>, ptr<>);

ptr<>
truncate_quotient(context& ctx, ptr<> lhs, ptr<> rhs);

ptr<>
truncate_remainder(context& ctx, ptr<> lhs, ptr<> rhs);

ptr<>
divide(context&, ptr<>, ptr<>);

ptr<>
conjugate(context&, ptr<>);

ptr<>
arithmetic_shift(context&, ptr<>, integer);

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

bool
numeric_eqv(context&, ptr<>, ptr<>);

ptr<boolean>
less(context&, ptr<>, ptr<>);

ptr<boolean>
greater(context&, ptr<>, ptr<>);

ptr<boolean>
less_or_equal(context&, ptr<>, ptr<>);

ptr<boolean>
greater_or_equal(context&, ptr<>, ptr<>);

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
abs(context&, ptr<>);

ptr<>
floor(context&, ptr<>);

ptr<>
read_integer(context& ctx, std::string const& digits, unsigned base = 10);

ptr<>
real_part(ptr<> x);

ptr<>
imag_part(ptr<> x);

} // namespace insider

#endif
