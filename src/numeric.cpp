#include "numeric.hpp"

#include "define_procedure.hpp"
#include "port.hpp"
#include "write.hpp"

#include <algorithm>
#include <bit>
#include <cmath>
#include <complex>
#include <cstdlib>
#include <ios>
#include <locale>
#include <numbers>
#include <numeric>
#include <sstream>

#undef small

namespace insider {

using limb_type = detail::limb_type;
using double_limb_type = detail::double_limb_type;

static constexpr limb_type max_limb_value = std::numeric_limits<limb_type>::max();
static constexpr unsigned  limb_width = std::numeric_limits<limb_type>::digits;
static constexpr double_limb_type limb_mask = (double_limb_type{1} << limb_width) - 1;

std::size_t
big_integer::extra_elements(std::size_t length) {
  return length;
}

std::size_t
big_integer::extra_elements(std::vector<limb_type> const& limbs, bool) {
  return limbs.size();
}

static std::size_t
number_of_limbs_for_small_integer(integer::value_type i) {
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

std::size_t
big_integer::extra_elements(integer i) {
  return number_of_limbs_for_small_integer(i.value());
}

std::size_t
big_integer::extra_elements(ptr<big_integer> i) {
  return i->length();
}

big_integer::big_integer(std::size_t length)
  : dynamic_size_object{length}
{
  std::fill(begin(), end(), limb_type{0});
}

big_integer::big_integer(std::size_t length, dont_initialize_t)
  : dynamic_size_object{length}
{ }

big_integer::big_integer(std::vector<limb_type> const& limbs, bool positive)
  : dynamic_size_object{limbs.size()}
  , positive_{positive}
{
  assert(static_cast<std::vector<limb_type>::size_type>(end() - begin()) == limbs.size());
  std::copy(limbs.begin(), limbs.end(), begin());
}

big_integer::big_integer(ptr<big_integer> i)
  : dynamic_size_object{i->length()}
  , positive_{i->positive()}
{
  std::copy(i->begin(), i->end(), begin());
}

static std::tuple<bool, integer::value_type>
short_integer_to_sign_magnitude(integer::value_type i) {
  if (i == 0)
    return {true, 0};
  else if (i > 0)
    return {true, i};
  else
    return {false, -i};
}

big_integer::big_integer(integer i)
  : dynamic_size_object{number_of_limbs_for_small_integer(i.value())}
{
  auto [sign, magnitude] = short_integer_to_sign_magnitude(i.value());
  positive_ = sign;

  if constexpr (sizeof(integer::value_type) <= sizeof(limb_type)) {
    front() = static_cast<big_integer::limb_type>(magnitude);
  } else {
    auto it = begin();
    for (std::size_t k = 0; k < number_of_limbs_for_small_integer(i.value()); ++k) {
      *it++ = magnitude & limb_mask;
      magnitude >>= limb_width;
    }
  }
}

big_integer::big_integer(big_integer&& other)
  : dynamic_size_object{other.size_}
  , positive_{other.positive_}
{
  std::copy(other.begin(), other.end(), begin());
}

auto
big_integer::begin() -> iterator {
  return &storage_element(0);
}

auto
big_integer::end() -> iterator{
  return &storage_element(0) + size_;
}

auto
big_integer::rbegin() -> reverse_iterator {
  return reverse_iterator{end()};
}

auto
big_integer::rend() -> reverse_iterator {
  return reverse_iterator{begin()};
}

std::size_t
big_integer::hash() const {
  std::size_t result = 0;

  for (std::size_t i = 0; i < size_; ++i)
    result = storage_element(i) + (result << 6) + (result << 16) - result;

  return result ^ static_cast<std::size_t>(positive_);
}

fraction::fraction(ptr<> num, ptr<> den)
  : numerator_{num}
  , denominator_{den}
{
  assert(is_exact_integer(num));
  assert(is_exact_integer(den));
}

std::size_t
fraction::hash() const {
  return insider::hash(numerator_) ^ insider::hash(denominator_);
}

std::size_t
floating_point::hash() const {
  return std::hash<value_type>{}(value);
}

std::size_t
complex::hash() const {
  return insider::hash(real_) ^ insider::hash(imaginary_);
}

static unsigned
digit_value(char32_t c) {
  if (c >= '0' && c <= '9')
    return c - '0';
  else if (c >= 'a' && c <= 'f')
    return c - 'a' + 10;
  else
    return c - 'A' + 10;
}

static ptr<big_integer>
extend_big(context& ctx, ptr<big_integer> i, limb_type new_limb) {
  auto result = make<big_integer>(ctx, i->length() + 1, big_integer::dont_initialize);
  std::copy(i->begin(), i->end(), result->begin());
  result->back() = new_limb;
  result->set_positive(i->positive());
  return result;
}

static std::size_t
normal_length(ptr<big_integer> i) {
  std::size_t result = i->length();
  while (result > 0 && i->data()[result - 1] == limb_type{0})
    --result;

  return result;
}

static ptr<>
normalize(context& ctx, ptr<big_integer> i) {
  std::size_t new_length = normal_length(i);

  if (new_length == 0)
    return integer_to_ptr(integer{0});

  if constexpr (max_limb_value >= integer::max) {
    if (new_length == 1) {
      limb_type l = i->front();
      if (i->positive() && l <= integer::max)
        return integer_to_ptr(static_cast<integer::value_type>(l));
      else if (!i->positive() && l <= -integer::min) {
        return integer_to_ptr(-static_cast<integer::value_type>(l));
      }
    }
  }
  else {
    if (new_length <= sizeof(integer::value_type) / sizeof(limb_type)) {
      integer::value_type small = 0;
      for (std::size_t k = i->length(); k > 0; --k)
        small = (small << limb_width) | i->data()[k - 1];

      if (i->positive() && small <= integer::max)
        return integer_to_ptr(integer{small});
      else if (!i->positive() && small <= -integer::min) {
        assert(small <= std::numeric_limits<integer::value_type>::max());
        return integer_to_ptr(integer{-static_cast<integer::value_type>(small)});
      }
    }
  }

  if (new_length == i->length())
    return i;

  auto result = make<big_integer>(ctx, new_length, big_integer::dont_initialize);
  std::copy(i->begin(), i->begin() + new_length, result->begin());
  result->set_positive(i->positive());
  return result;
}

ptr<>
make_fraction(context& ctx, ptr<> num, ptr<> den) {
  return normalize_fraction(ctx, make<fraction>(ctx, num, den));
}

ptr<>
normalize_fraction(context& ctx, ptr<fraction> q) {
  ptr<> num = q->numerator();
  ptr<> den = q->denominator();

  if (auto d = match<integer>(den)) {
    if (d->value() == 0)
      throw std::runtime_error{"0 in fraction denominator"};
  }

  if (auto n = match<integer>(num)) {
    if (n->value() == 0)
      return integer_to_ptr(*n);
  }

  if (is_negative(den))
    return normalize_fraction(ctx, make<fraction>(ctx, negate(ctx, num), negate(ctx, den)));

  ptr<> com_den = gcd(ctx, num, den);
  if (auto c = match<integer>(com_den)) {
    if (c->value() == 1)
      return q;
  }

  ptr<> reduced_num = truncate_quotient(ctx, num, com_den);
  ptr<> reduced_den = truncate_quotient(ctx, den, com_den);

  if (auto d = match<integer>(reduced_den))
    if (d->value() == 1)
      return reduced_num;

  return make<fraction>(ctx, num, den);
}

static ptr<>
normalize_complex(ptr<complex> z) {
  if (is_zero(z->imaginary()))
    return z->real();
  else
    return z;
}

static ptr<big_integer>
add_big_magnitude_to_limb_destructive(context& ctx, ptr<big_integer> result,
                                      ptr<big_integer> lhs, limb_type rhs) {
  assert(!lhs->zero());

  if (!result || result->length() < lhs->length())
    result = make<big_integer>(ctx, lhs->length());

  // result and lhs may point to the same object.

  limb_type* x = lhs->data();
  limb_type* out = result->data();
  limb_type x_0 = x[0];
  out[0] = x_0 + rhs;
  limb_type carry = x_0 > max_limb_value - rhs;

  for (std::size_t i = 1; i < lhs->length(); ++i) {
    limb_type x_i = x[i];
    out[i] = x_i + carry;
    carry = x_i > max_limb_value - carry;
  }

  if (carry)
    return extend_big(ctx, result, limb_type{1});
  else
    return result;
}

static ptr<>
add_magnitude_to_limb_destructive(context& ctx, ptr<> lhs, limb_type rhs) {
  if (auto b = match<big_integer>(lhs))
    return add_big_magnitude_to_limb_destructive(ctx, b, b, rhs);

  auto lhs_int = assume<integer>(lhs);
  double_limb_type sum = lhs_int.value() + rhs;
  if (sum <= integer::max) {
    lhs_int.set_value(static_cast<integer::value_type>(sum));
    return integer_to_ptr(lhs_int);
  }

  return add_big_magnitude_to_limb_destructive(ctx, {}, make<big_integer>(ctx, lhs_int), rhs);
}

static ptr<big_integer>
add_big_magnitude_destructive(context& ctx, ptr<big_integer> result, ptr<big_integer> lhs, ptr<big_integer> rhs) {
  // lhs is always going to be the bigger of the two to simplify things in the
  // implementation.

  if (lhs->length() < rhs->length())
    std::swap(lhs, rhs);

  if (!result || result->length() < lhs->length())
    result = make<big_integer>(ctx, lhs->length());
  limb_type* x = lhs->data();
  limb_type* y = rhs->data();
  limb_type* out = result->data();
  limb_type carry = 0;

  for (std::size_t i = 0; i < rhs->length(); ++i) {
    limb_type s = x[i] + y[i];
    out[i] = s + carry;
    carry = x[i] > max_limb_value - y[i] || s > max_limb_value - carry;
  }

  for (std::size_t i = rhs->length(); i < lhs->length(); ++i) {
    out[i] = x[i] + carry;
    carry = x[i] > max_limb_value - carry;
  }

  if (carry)
    return extend_big(ctx, result, limb_type{1});
  else
    return result;
}

static ptr<big_integer>
add_big_magnitude(context& ctx, ptr<big_integer> lhs, ptr<big_integer> rhs) {
  return add_big_magnitude_destructive(ctx, {}, lhs, rhs);
}

namespace {
  enum class exact_compare_result {
    less,
    equal,
    greater
  };
}

static exact_compare_result
compare_magnitude(ptr<big_integer> lhs, ptr<big_integer> rhs,
                  std::size_t lhs_length, std::size_t rhs_length) {
  if (lhs_length < rhs_length)
    return exact_compare_result::less;
  else if (lhs_length > rhs_length)
    return exact_compare_result::greater;

  for (std::size_t i = lhs_length; i > 0; --i) {
    limb_type x = lhs->data()[i - 1];
    limb_type y = rhs->data()[i - 1];

    if (x != y) {
      if (x < y)
        return exact_compare_result::less;
      else
        return exact_compare_result::greater;
    }
  }

  return exact_compare_result::equal;
}

static exact_compare_result
compare_magnitude(ptr<big_integer> lhs, ptr<big_integer> rhs) {
  return compare_magnitude(lhs, rhs, lhs->length(), rhs->length());
}

static ptr<big_integer>
sub_magnitude(context& ctx, ptr<big_integer> lhs, ptr<big_integer> rhs) {
  // We want the bigger number to be the LHS to simplify stuff below.

  bool positive = true;
  std::size_t lhs_length = normal_length(lhs);
  std::size_t rhs_length = normal_length(rhs);
  switch (compare_magnitude(lhs, rhs, lhs_length, rhs_length)) {
  case exact_compare_result::less:
    std::swap(lhs, rhs);
    std::swap(lhs_length, rhs_length);
    positive = false;
    break;
  case exact_compare_result::equal:
    return make<big_integer>(ctx, 0);
  case exact_compare_result::greater:
    break;
  }

  auto result = make<big_integer>(ctx, std::max(lhs_length, rhs_length));
  result->set_positive(positive);

  limb_type* x = lhs->data();
  limb_type* y = rhs->data();
  limb_type* out = result->data();
  limb_type borrow = 0;

  for (std::size_t i = 0; i < rhs_length; ++i) {
    limb_type d = x[i] - y[i];
    out[i] = d - borrow;
    borrow = x[i] < y[i] || d < borrow;
  }

  for (std::size_t i = rhs_length; i < lhs_length; ++i) {
    out[i] = x[i] - borrow;
    borrow = x[i] < borrow;
  }

  assert(!borrow);
  return result;
}

static ptr<big_integer>
flip_sign(ptr<big_integer> i) {
  i->set_positive(!i->positive());
  return i;
}

static ptr<big_integer>
set_sign_copy(context& ctx, ptr<big_integer> value, bool sign) {
  if (value->positive() == sign)
    return value;

  auto result = make<big_integer>(ctx, value);
  return flip_sign(result);
}

static ptr<big_integer>
add_big(context& ctx, ptr<big_integer> lhs, ptr<big_integer> rhs) {
  if (lhs->zero())
    return rhs;
  if (rhs->zero())
    return lhs;

  if (lhs->positive() != rhs->positive()) {
    if (lhs->positive())
      return sub_magnitude(ctx, lhs, rhs);
    else
      return sub_magnitude(ctx, rhs, lhs);
  }

  auto result = add_big_magnitude(ctx, lhs, rhs);
  if (lhs->positive())
    return result;
  else
    return flip_sign(result);
}

static ptr<>
add_small(context& ctx, integer lhs, integer rhs) {
  integer::value_type sum = lhs.value() + rhs.value();
  if (overflow(sum))
    return add_big(ctx, make<big_integer>(ctx, lhs), make<big_integer>(ctx, rhs));
  else
    return integer_to_ptr(integer{sum});
}

static ptr<fraction>
add_fraction(context& ctx, ptr<fraction> lhs, ptr<fraction> rhs) {
  //  a     c    ad + bc
  // --- + --- = -------
  //  b     d      bd

  return make<fraction>(ctx,
                        add(ctx,
                            multiply(ctx, lhs->numerator(), rhs->denominator()),
                            multiply(ctx, lhs->denominator(), rhs->numerator())),
                        multiply(ctx, lhs->denominator(), rhs->denominator()));
}

static ptr<floating_point>
add_float(context& ctx, ptr<floating_point> lhs, ptr<floating_point> rhs) {
  return make<floating_point>(ctx, lhs->value + rhs->value);
}

static ptr<complex>
add_complex(context& ctx, ptr<complex> lhs, ptr<complex> rhs) {
  return make<complex>(ctx,
                       add(ctx, lhs->real(), rhs->real()),
                       add(ctx, lhs->imaginary(), rhs->imaginary()));
}

static ptr<big_integer>
sub_big(context& ctx, ptr<big_integer> lhs, ptr<big_integer> rhs) {
  if (rhs->zero())
    return lhs;

  if (lhs->zero())
    return flip_sign(make<big_integer>(ctx, rhs));

  if (lhs->positive() != rhs->positive()) {
    if (lhs->positive())
      return add_big_magnitude(ctx, lhs, rhs);
    else
      return flip_sign(add_big_magnitude(ctx, lhs, rhs));
  }

  auto result = sub_magnitude(ctx, lhs, rhs);
  if (lhs->positive())
    return result;
  else
    return flip_sign(result);
}

static ptr<>
sub_small(context& ctx, integer lhs, integer rhs) {
  integer::value_type dif = lhs.value() - rhs.value();
  if (overflow(dif))
    return sub_big(ctx, make<big_integer>(ctx, lhs), make<big_integer>(ctx, rhs));
  else
    return integer_to_ptr(integer{dif});
}

static ptr<fraction>
sub_fraction(context& ctx, ptr<fraction> lhs, ptr<fraction> rhs) {
  return make<fraction>(ctx,
                        subtract(ctx,
                                 multiply(ctx, lhs->numerator(), rhs->denominator()),
                                 multiply(ctx, lhs->denominator(), rhs->numerator())),
                        multiply(ctx, lhs->denominator(), rhs->denominator()));
}

static ptr<floating_point>
sub_float(context& ctx, ptr<floating_point> lhs, ptr<floating_point> rhs) {
  return make<floating_point>(ctx, lhs->value - rhs->value);
}

static ptr<complex>
sub_complex(context& ctx, ptr<complex> lhs, ptr<complex> rhs) {
  return make<complex>(ctx,
                       subtract(ctx, lhs->real(), rhs->real()),
                       subtract(ctx, lhs->imaginary(), rhs->imaginary()));
}

static std::tuple<limb_type, limb_type>
mul_limb_by_limb(limb_type lhs, limb_type rhs) {
  double_limb_type result = double_limb_type{lhs} * double_limb_type{rhs};
  return {limb_type(result >> limb_width), limb_type(result & limb_mask)};
}

static ptr<big_integer>
mul_big_magnitude_by_limb_destructive(context& ctx, ptr<big_integer> result,
                                      ptr<big_integer> lhs, limb_type rhs) {
  if (rhs == 0)
    return make<big_integer>(ctx, 0);
  if (rhs == 1)
    return lhs;

  if (!result || result->length() < lhs->length())
    result = make<big_integer>(ctx, lhs->length(), big_integer::dont_initialize);

  limb_type carry = 0;
  limb_type* x = lhs->data();
  limb_type* out = result->data();

  for (std::size_t i = 0; i < result->length(); ++i) {
    auto [new_carry, term] = mul_limb_by_limb(x[i], rhs);
    out[i] = term + carry;

    if (term > max_limb_value - carry) {
      assert(new_carry <= max_limb_value - 1); // So the ++ won't overflow.
      ++new_carry;
    }

    carry = new_carry;
  }

  if (carry)
    return extend_big(ctx, result, carry);
  else
    return result;
}

static ptr<big_integer>
mul_big_magnitude_by_limb(context& ctx, ptr<big_integer> lhs, limb_type rhs) {
  return mul_big_magnitude_by_limb_destructive(ctx, {}, lhs, rhs);
}

static ptr<>
mul_magnitude_by_limb_destructive(context& ctx, ptr<> lhs, limb_type rhs) {
  if (auto b = match<big_integer>(lhs))
    return mul_big_magnitude_by_limb_destructive(ctx, b, b, rhs);

  auto lhs_int = assume<integer>(lhs);
  assert(lhs_int.value() >= 0);

  if (small_mul_overflow(lhs_int.value(), rhs))
    return mul_big_magnitude_by_limb_destructive(ctx, {}, make<big_integer>(ctx, lhs_int), rhs);

  lhs_int.set_value(lhs_int.value() * rhs);
  return integer_to_ptr(lhs_int);
}

static bool
magnitude_one(ptr<big_integer> i) {
  return i->length() == 1 && i->front() == 1;
}

static ptr<big_integer>
shift(context& ctx, ptr<big_integer> i, std::size_t k) {
  if (k == 0)
    return i;

  auto result = make<big_integer>(ctx, i->length() + k);
  std::copy(i->begin(), i->end(), result->begin() + k);
  result->set_positive(i->positive());
  return result;
}

static ptr<big_integer>
mul_big(context& ctx, ptr<big_integer> lhs, ptr<big_integer> rhs) {
  if (lhs->zero())
    return lhs;
  if (rhs->zero())
    return rhs;

  if (magnitude_one(lhs))
    return set_sign_copy(ctx, rhs, lhs->positive());
  if (magnitude_one(rhs))
    return set_sign_copy(ctx, lhs, rhs->positive());

  if (lhs->length() < rhs->length())
    std::swap(lhs, rhs);

  auto result = mul_big_magnitude_by_limb(ctx, lhs, rhs->front());
  for (unsigned i = 1; i < static_cast<unsigned>(rhs->length()); ++i) {
    auto term = mul_big_magnitude_by_limb(ctx, lhs, rhs->data()[i]);
    if (!term->zero())
      result = add_big_magnitude(ctx, result, shift(ctx, term, i));
  }

  if (lhs->positive() != rhs->positive())
    result->set_positive(false);

  return result;
}

static ptr<>
mul_small(context& ctx, integer lhs, integer rhs) {
  integer::value_type x = lhs.value() > 0 ? lhs.value() : -lhs.value();
  integer::value_type y = rhs.value() > 0 ? rhs.value() : -rhs.value();

  if (x == 0 || y == 0)
    return integer_to_ptr(0);

  bool result_positive = (lhs.value() > 0) == (rhs.value() > 0);

  if (small_mul_overflow(x, y))
    return mul_big(ctx, make<big_integer>(ctx, lhs), make<big_integer>(ctx, rhs));

  integer::value_type product = x * y;
  return integer_to_ptr(integer{result_positive ? product : -product});
}

static ptr<fraction>
mul_fraction(context& ctx, ptr<fraction> lhs, ptr<fraction> rhs) {
  return make<fraction>(ctx,
                        multiply(ctx, lhs->numerator(), rhs->numerator()),
                        multiply(ctx, lhs->denominator(), rhs->denominator()));
}

static ptr<floating_point>
mul_float(context& ctx, ptr<floating_point> lhs, ptr<floating_point> rhs) {
  return make<floating_point>(ctx, lhs->value * rhs->value);
}

static ptr<complex>
mul_complex(context& ctx, ptr<complex> lhs, ptr<complex> rhs) {
  // (a + bi)(c + di) = (ac - bd) + (bc + ad)i
  return make<complex>(
    ctx,
    subtract(ctx, multiply(ctx, lhs->real(), rhs->real()), multiply(ctx, lhs->imaginary(), rhs->imaginary())),
    add(ctx, multiply(ctx, lhs->imaginary(), rhs->real()), multiply(ctx, lhs->real(), rhs->imaginary()))
  );
}

static ptr<big_integer>
bitshift_left_destructive(context& ctx, ptr<big_integer> i, std::size_t shift) {
  if (shift == 0)
    return i;

  std::size_t k = shift % limb_width;
  std::size_t extra_limbs = shift / limb_width;

  limb_type const top_k_bits = ~limb_type{0} << (limb_width - k);
  if ((i->back() & top_k_bits) != 0)
    i = extend_big(ctx, i, 0);

  for (std::size_t n = i->length(); n > 0; --n) {
    limb_type& current = i->data()[n - 1];
    current <<= k;

    if (n - 1 > 0) {
      limb_type lower = i->data()[n - 2];
      current |= (lower & top_k_bits) >> (limb_width - k);
    }
  }

  if (extra_limbs > 0) {
    auto result = make<big_integer>(ctx, i->length() + extra_limbs, big_integer::dont_initialize);
    std::copy(i->begin(), i->end(), result->begin());
    std::fill_n(result->begin() + i->length(), result->length() - i->length(), 0);
    return result;
  }

  return i;
}

static ptr<big_integer>
bitshift_left(context& ctx, ptr<big_integer> i, unsigned k) {
  return bitshift_left_destructive(ctx, make<big_integer>(ctx, i), k);
}

static ptr<big_integer>
bitshift_right_destructive(ptr<big_integer> i, std::size_t k) {
  if (k == 0)
    return i;

  assert(k < limb_width);

  limb_type const bottom_k_bits_mask = ~limb_type{1} >> (limb_width - k);

  for (std::size_t n = 0; n < i->length(); ++n) {
    limb_type& current = i->data()[n];
    current >>= k;

    if (n + 1 < i->length()) {
      limb_type upper = i->data()[n + 1];
      current |= (upper & bottom_k_bits_mask) << (limb_width - k);
    }
  }

  return i;
}

static ptr<big_integer>
bitshift_right(context& ctx, ptr<big_integer> i, std::size_t k) {
  return bitshift_right_destructive(make<big_integer>(ctx, i), k);
}

static ptr<>
bitshift_right(context& ctx, ptr<> n, std::size_t k) {
  if (auto i = match<integer>(n))
    return integer_to_ptr(i->value() >> k);
  else
    return bitshift_right(ctx, assume<big_integer>(n), k);
}

static bool
least_significant_bit(ptr<> n) {
  if (auto i = match<integer>(n))
    return i->value() & 1;
  else
    return assume<big_integer>(n)->front() & 1;
}

static limb_type
guess_quotient(limb_type a_hi, limb_type a_lo, limb_type b) {
  double_limb_type a = (double_limb_type{a_hi} << limb_width) | a_lo;
  double_limb_type q = a / b;
  return static_cast<limb_type>(std::min(q, double_limb_type{max_limb_value}));
}

static std::tuple<ptr<big_integer>, limb_type>
div_rem_by_limb_magnitude(context& ctx, ptr<big_integer> dividend, limb_type divisor) {
  auto quotient = make<big_integer>(ctx, dividend->length());
  double_limb_type d{};

  for (std::size_t i = dividend->length(); i > 0; --i) {
    d = (d << limb_width) | dividend->data()[i - 1];
    limb_type q = static_cast<limb_type>(d / divisor);
    quotient->data()[i - 1] = q;
    d -= double_limb_type{q} * double_limb_type{divisor};
    assert(d <= max_limb_value);
  }

  return {quotient, limb_type(d)};
}

static std::tuple<ptr<big_integer>, ptr<big_integer>>
div_rem_magnitude(context& ctx, ptr<big_integer> dividend, ptr<big_integer> divisor) {
  assert(!divisor->zero());
  assert(divisor->back() != 0);

  if (divisor->length() == 1) {
    auto [quot, rem] = div_rem_by_limb_magnitude(ctx, dividend, divisor->front());
    return {quot, make<big_integer>(ctx, std::vector{rem})};
  }

  unsigned normalisation_shift = std::countl_zero(divisor->back());
  dividend = bitshift_left(ctx, dividend, normalisation_shift);
  divisor = bitshift_left(ctx, divisor, normalisation_shift);

  std::size_t dividend_len = dividend->length();
  std::size_t divisor_len = divisor->length();
  assert(dividend_len >= divisor_len);
  assert(divisor_len >= 2);
  assert(divisor->positive());
  assert(dividend->positive());

  auto quotient = make<big_integer>(ctx, dividend_len - divisor_len + 1);

  ptr<big_integer> first_shifted_divisor = shift(ctx, divisor, dividend_len - divisor_len);
  if (compare_magnitude(dividend, first_shifted_divisor) != exact_compare_result::less) {
    quotient->data()[dividend_len - divisor_len] = 1;
    dividend = sub_big(ctx, dividend, first_shifted_divisor);
  }

  for (std::size_t i = dividend_len - divisor_len; i > 0; --i) {
    std::size_t j = i - 1;
    assert(divisor_len + j < dividend->length());
    assert(divisor_len + j >= 1);
    assert(divisor->positive());

    limb_type q = guess_quotient(dividend->data()[divisor_len + j],
                                 dividend->data()[divisor_len + j - 1],
                                 divisor->back());
    ptr<big_integer> shifted_divisor = shift(ctx, divisor, j);
    dividend = sub_big(ctx, dividend, mul_big_magnitude_by_limb(ctx, shifted_divisor, q));

    while (!dividend->positive()) {
      --q;
      dividend = add_big(ctx, dividend, shifted_divisor);
    }

    quotient->data()[j] = q;
  }

  return {quotient, bitshift_right(ctx, dividend, normalisation_shift)};
}

static std::tuple<ptr<>, ptr<>>
div_rem_big(context& ctx, ptr<big_integer> dividend, ptr<big_integer> divisor) {
  if (divisor->zero())
    throw std::runtime_error{"Division by zero"};

  if (magnitude_one(divisor)) {
    if (divisor->positive())
      return {dividend, make<big_integer>(ctx, 0)};
    else
      return {set_sign_copy(ctx, dividend, !dividend->positive()),
              make<big_integer>(ctx, 0)};
  }

  auto [quot, rem] = div_rem_magnitude(ctx, dividend, divisor);
  if (divisor->positive())
    return {normalize(ctx, quot), normalize(ctx, rem)};
  else
    return {normalize(ctx, set_sign_copy(ctx, quot, !dividend->positive())),
            normalize(ctx, rem)};
}

static std::tuple<ptr<>, ptr<>>
div_rem_small(integer dividend, integer divisor) {
  auto [quot, rem] = std::div(dividend.value(), divisor.value());
  return {integer_to_ptr(integer{quot}), integer_to_ptr(integer{rem})};
}

static exact_compare_result
compare_big(ptr<big_integer> lhs, ptr<big_integer> rhs) {
  if (lhs->positive() != rhs->positive()) {
    if (!lhs->positive() && rhs->positive())
      return exact_compare_result::less;
    else
      return exact_compare_result::greater;
  }

  exact_compare_result result = compare_magnitude(lhs, rhs);

  if (result == exact_compare_result::equal)
    return result;

  if (!lhs->positive()) {
    if (result == exact_compare_result::less)
      return exact_compare_result::greater;
    else
      return exact_compare_result::less;
  }

  return result;
}

namespace {
  enum class common_type {
    small_integer,
    big_integer,
    fraction,
    floating_point,
    complex
  };
}

static common_type
find_common_type(ptr<> lhs, ptr<> rhs) {
  if (!is_number(lhs))
    throw std::runtime_error{fmt::format("Expected number, got {}", object_type_name(lhs))};
  if (!is_number(rhs))
    throw std::runtime_error{fmt::format("Expected number, got {}", object_type_name(rhs))};

  if (is<complex>(lhs) || is<complex>(rhs))
    return common_type::complex;
  else if (is<floating_point>(lhs) || is<floating_point>(rhs))
    return common_type::floating_point;
  else if (is<fraction>(lhs) || is<fraction>(rhs))
    return common_type::fraction;
  else if (is<big_integer>(lhs) || is<big_integer>(rhs))
    return common_type::big_integer;
  else {
    assert(is<integer>(lhs));
    assert(is<integer>(rhs));
    return common_type::small_integer;
  }
}

static ptr<big_integer>
make_big(context& ctx, ptr<> x) {
  if (auto b = match<big_integer>(x))
    return b;
  else if (auto s = match<integer>(x)) {
    return make<big_integer>(ctx, *s);
  }
  else {
    assert(!"Can't happen");
    return {};
  }
}

static ptr<big_integer>
make_big_copy(context& ctx, ptr<> x) {
  if (auto b = match<big_integer>(x))
    return make<big_integer>(ctx, b);
  else if (auto s = match<integer>(x)) {
    return make<big_integer>(ctx, *s);
  }
  else {
    assert(!"Can't happen");
    return {};
  }
}

static ptr<fraction>
make_fraction(context& ctx, ptr<> x) {
  if (is<integer>(x) || is<big_integer>(x))
    return make<fraction>(ctx, x, integer_to_ptr(integer{1}));

  assert(is<fraction>(x));
  return assume<fraction>(x);
}

static floating_point::value_type
big_to_float_value(ptr<big_integer> n) {
  floating_point::value_type result = 0;

  for (std::size_t i = n->length(); i > 0; --i)
    result = result * (static_cast<double>(max_limb_value) + 1.0) + n->data()[i - 1];

  if (n->positive())
    return result;
  else
    return -result;
}

static floating_point::value_type
integer_to_float_value(ptr<> n) {
  if (auto s = match<integer>(n))
    return static_cast<floating_point::value_type>(s->value());
  else
    return big_to_float_value(assume<big_integer>(n));
}

static floating_point::value_type
to_float_value(ptr<> x) {
  if (auto f = match<floating_point>(x))
    return f->value;
  else if (auto n = match<integer>(x))
    return static_cast<floating_point::value_type>(n->value());
  else if (auto n = match<big_integer>(x))
    return big_to_float_value(n);
  else if (auto q = match<fraction>(x))
    return integer_to_float_value(q->numerator()) / integer_to_float_value(q->denominator());
  else if (auto z = match<complex>(x)) {
    assert(is_real(z));
    return to_float_value(z->real());
  }

  assert(false);
  return {};
}

static ptr<floating_point>
make_float(context& ctx, ptr<> x) {
  return make<floating_point>(ctx, to_float_value(x));
}

template <auto F>
ptr<>
arithmetic(context& ctx, object_span xs, bool allow_empty, integer::value_type neutral) {
  if (xs.empty()) {
    if (allow_empty)
      return integer_to_ptr(integer{neutral});
    else
      throw std::runtime_error{"Not enough arguments"};
  }
  else if (xs.size() == 1)
    return F(ctx, integer_to_ptr(integer{neutral}), xs.front());
  else {
    ptr<> result = xs.front();
    for (auto rhs = xs.begin() + 1; rhs != xs.end(); ++rhs)
      result = F(ctx, result, *rhs);

    return result;
  }
}

static ptr<complex>
make_complex(context& ctx, ptr<> x) {
  if (auto z = match<complex>(x))
    return z;
  else
    return make<complex>(ctx, x, integer_to_ptr(0));
}

using primitive_arithmetic_type = ptr<>(context& ctx, ptr<>, ptr<>);

template <auto Small, auto Big, auto Fraction, auto Float, auto Complex>
ptr<>
arithmetic_two(context& ctx, ptr<> lhs, ptr<> rhs) {
  switch (find_common_type(lhs, rhs)) {
  case common_type::small_integer:
    return Small(ctx, assume<integer>(lhs), assume<integer>(rhs));
  case common_type::big_integer:
    return normalize(ctx, Big(ctx, make_big(ctx, lhs), make_big(ctx, rhs)));
  case common_type::fraction:
    return normalize_fraction(ctx, Fraction(ctx, make_fraction(ctx, lhs), make_fraction(ctx, rhs)));
  case common_type::floating_point:
    return Float(ctx, make_float(ctx, lhs), make_float(ctx, rhs));
  case common_type::complex:
    return normalize_complex(Complex(ctx, make_complex(ctx, lhs), make_complex(ctx, rhs)));
  }

  assert(false);
  return {};
}

ptr<>
make_rectangular(context& ctx, ptr<> real, ptr<> imaginary) {
  return normalize_complex(make<complex>(ctx, real, imaginary));
}

ptr<>
make_polar(context& ctx, ptr<> magnitude, ptr<> angle) {
  return normalize_complex(make<complex>(ctx,
                                         multiply(ctx, magnitude, cos(ctx, angle)),
                                         multiply(ctx, magnitude, sin(ctx, angle))));
}

static ptr<>
real_magnitude(context& ctx, ptr<> x) {
  if (is_negative(x))
    return multiply(ctx, integer_to_ptr(-1), x);
  else
    return x;
}

static ptr<>
complex_magnitude(context& ctx, ptr<complex> z) {
  double re = to_float_value(z->real());
  double im = to_float_value(z->imaginary());
  return make<floating_point>(ctx, std::sqrt(re * re + im * im));
}

ptr<>
magnitude(context& ctx, ptr<> x) {
  if (auto z = match<complex>(x))
    return complex_magnitude(ctx, z);
  else if (is_real(x))
    return real_magnitude(ctx, x);
  else
    throw std::runtime_error{"Expected a number"};
}

static ptr<>
real_angle(context& ctx, ptr<> x) {
  if (is_negative(x))
    return make<floating_point>(ctx, std::numbers::pi);
  else
    return integer_to_ptr(0);
}

static ptr<>
complex_angle(context& ctx, ptr<complex> z) {
  double re = to_float_value(z->real());
  double im = to_float_value(z->imaginary());
  return make<floating_point>(ctx, std::atan2(im, re));
}

ptr<>
angle(context& ctx, ptr<> x) {
  if (auto z = match<complex>(x))
    return complex_angle(ctx, z);
  else if (is_real(x))
    return real_angle(ctx, x);
  else
    throw std::runtime_error{"Expected a number"};
}

bool
is_exact_integer(ptr<> x) {
  return is<integer>(x) || is<big_integer>(x);
}

static bool
is_floating_integer(ptr<> x) {
  if (auto fp = match<floating_point>(x)) {
    double integer_part;
    return std::modf(fp->value, &integer_part) == 0.0;
  } else
    return false;
}

bool
is_integer(ptr<> x) {
  return is_exact_integer(x) || is_floating_integer(x);
}

bool
is_number(ptr<> x) {
  return is_exact_integer(x) || is<fraction>(x) || is<floating_point>(x) || is<complex>(x);
}

bool
is_real(ptr<> x) {
  if (auto z = match<complex>(x))
    return is_zero(z->imaginary());
  else
    return is_number(x);
}

static bool
is_exact_complex(ptr<> x) {
  if (auto z = match<complex>(x))
    return is_exact(z->real()) && is_exact(z->imaginary());
  else
    return false;
}

bool
is_exact(ptr<> x) {
  return is_exact_integer(x) || is<fraction>(x) || is_exact_complex(x);
}

bool
is_inexact(ptr<> x) {
  return is_number(x) && !is_exact(x);
}

bool
is_nan(ptr<> x) {
  if (auto z = match<complex>(x))
    return is_nan(z->real()) || is_nan(z->imaginary());
  else if (auto fp = match<floating_point>(x))
    return std::isnan(fp->value);
  else
    return false;
}

bool
is_finite(ptr<> x) {
  return is_number(x) && !is_infinite(x) && !is_nan(x);
}

bool
is_infinite(ptr<> x) {
  if (auto z = match<complex>(x))
    return is_infinite(z->real()) || is_infinite(z->imaginary());
  else if (auto fp = match<floating_point>(x))
    return std::isinf(fp->value);
  else
    return false;
}

bool
is_positive(ptr<> x) {
  if (auto i = match<integer>(x))
    return i->value() > 0;
  else if (auto b = match<big_integer>(x))
    return b->positive();
  else if (auto f = match<fraction>(x))
    return is_positive(f->numerator()) == is_positive(f->denominator());
  else if (auto fp = match<floating_point>(x))
    return fp->value > 0.0;
  else
    throw std::runtime_error{"Expected a real number"};
}

bool
is_negative(ptr<> x) {
  if (auto i = match<integer>(x))
    return i->value() < 0;
  else if (auto b = match<big_integer>(x))
    return !b->positive() && !b->zero();
  else if (auto f = match<fraction>(x))
    return is_negative(f->numerator()) == is_negative(f->denominator());
  else if (auto fp = match<floating_point>(x))
    return fp->value < 0.0;
  else
    throw std::runtime_error{"Expected a real number"};
}

bool
is_zero(ptr<> x) {
  if (auto i = match<integer>(x))
    return i->value() == 0;
  else if (auto b = match<big_integer>(x))
    return b->zero();
  else if (auto f = match<fraction>(x))
    return is_zero(f->numerator());
  else if (auto fp = match<floating_point>(x))
    return fp->value == 0.0;
  else if (auto z = match<complex>(x))
    return is_zero(z->real()) && is_zero(z->imaginary());
  else
    throw std::runtime_error{"Expected a number"};
}

bool
is_exactly_equal_to(ptr<> x, integer::value_type y) {
  if (auto i = match<integer>(x))
    return i->value() == y;
  else
    return false;
}

bool
is_odd(ptr<> n) {
  if (auto i = match<integer>(n))
    return (i->value() & 1) == 1;
  else if (auto b = match<big_integer>(n))
    return (b->front() & 1) == 1;
  else
    throw std::runtime_error{"Expected an integer"};
}

bool
is_even(ptr<> n) {
  if (auto i = match<integer>(n))
    return (i->value() & 1) == 0;
  else if (auto b = match<big_integer>(n))
    return (b->front() & 1) == 0;
  else
    throw std::runtime_error{"Expected an integer"};
}

ptr<>
negate(context& ctx, ptr<> x) {
  return multiply(ctx, x, integer_to_ptr(-1));
}

ptr<>
add(context& ctx, ptr<> lhs, ptr<> rhs) {
  return arithmetic_two<add_small, add_big, add_fraction, add_float, add_complex>(ctx, lhs, rhs);
}

ptr<>
add(context& ctx, object_span xs) {
  return arithmetic<static_cast<primitive_arithmetic_type*>(&add)>(ctx, xs, true, 0);
}

ptr<>
subtract(context& ctx, ptr<> lhs, ptr<> rhs) {
  return arithmetic_two<sub_small, sub_big, sub_fraction, sub_float, sub_complex>(ctx, lhs, rhs);
}

ptr<>
subtract(context& ctx, object_span xs) {
  return arithmetic<static_cast<primitive_arithmetic_type*>(&subtract)>(ctx, xs, false, 0);
}

ptr<>
multiply(context& ctx, ptr<> lhs, ptr<> rhs) {
  return arithmetic_two<mul_small, mul_big, mul_fraction, mul_float, mul_complex>(ctx, lhs, rhs);
}

ptr<>
multiply(context& ctx, object_span xs) {
  return arithmetic<static_cast<primitive_arithmetic_type*>(&multiply)>(ctx, xs, true, 1);
}

ptr<>
truncate_quotient(context& ctx, ptr<> lhs, ptr<> rhs) {
  return std::get<0>(quotient_remainder(ctx, lhs, rhs));
}

ptr<>
truncate_quotient(context& ctx, object_span xs) {
  return arithmetic<static_cast<primitive_arithmetic_type*>(&truncate_quotient)>(ctx, xs, false, 1);
}

std::tuple<ptr<>, ptr<>>
quotient_remainder(context& ctx, ptr<> lhs, ptr<> rhs) {
  switch (find_common_type(lhs, rhs)) {
  case common_type::small_integer:
    return div_rem_small(assume<integer>(lhs), assume<integer>(rhs));
  case common_type::big_integer:
    return div_rem_big(ctx, make_big(ctx, lhs), make_big(ctx, rhs));
  default:
    throw std::runtime_error{"Expected integer"};
  }

  assert(false);
  return {};
}

static ptr<>
div_fraction(context& ctx, ptr<fraction> x, ptr<fraction> y) {
  return normalize_fraction(
    ctx,
    make<fraction>(ctx,
                   multiply(ctx, x->numerator(), y->denominator()),
                   multiply(ctx, x->denominator(), y->numerator()))
  );
}

static ptr<floating_point>
div_float(context& ctx, ptr<floating_point> x, ptr<floating_point> y) {
  return make<floating_point>(ctx, x->value / y->value);
}

static ptr<>
div_complex_by_real(context& ctx, ptr<complex> lhs, ptr<> rhs) {
  assert(is_real(rhs));
  return make_rectangular(ctx, divide(ctx, lhs->real(), rhs), divide(ctx, lhs->imaginary(), rhs));
}

static ptr<>
div_complex(context& ctx, ptr<complex> lhs, ptr<complex> rhs) {
  // a + bi   (a + bi) (c - di)
  // ------ = -----------------
  // c + di       c^2 + d^2

  return div_complex_by_real(ctx,
                             make_complex(ctx, multiply(ctx, lhs, conjugate(ctx, rhs))),
                             add(ctx,
                                 multiply(ctx, rhs->real(), rhs->real()),
                                 multiply(ctx, rhs->imaginary(), rhs->imaginary())));
}

ptr<>
divide(context& ctx, ptr<> lhs, ptr<> rhs) {
  switch (find_common_type(lhs, rhs)) {
  case common_type::small_integer:
  case common_type::big_integer:
  case common_type::fraction:
    return div_fraction(ctx, make_fraction(ctx, lhs), make_fraction(ctx, rhs));

  case common_type::floating_point:
    return div_float(ctx, make_float(ctx, lhs), make_float(ctx, rhs));

  case common_type::complex:
    if (is<complex>(rhs))
      return div_complex(ctx, make_complex(ctx, lhs), make_complex(ctx, rhs));
    else
      return div_complex_by_real(ctx, assume<complex>(lhs), rhs);
  }

  assert(false);
  return {};
}

ptr<>
divide(context& ctx, object_span xs) {
  return arithmetic<static_cast<primitive_arithmetic_type*>(&divide)>(ctx, xs, false, 1);
}

ptr<>
conjugate(context& ctx, ptr<> x) {
  if (auto z = match<complex>(x))
    return make<complex>(ctx, z->real(), subtract(ctx, integer_to_ptr(0), z->imaginary()));
  else
    return x;
}

template <auto Small>
ptr<>
bitwise_two(ptr<> lhs, ptr<> rhs) {
  switch (find_common_type(lhs, rhs)) {
  case common_type::small_integer: {
    auto x = assume<integer>(lhs);
    auto y = assume<integer>(rhs);
    integer::value_type value = Small(x, y);

    if (overflow(value))
      throw std::runtime_error("Overflow in arithmetic-shift");

    return integer_to_ptr(integer{value});
  }

  default:
    throw std::runtime_error("Only fixnums are supported");
  }

  assert(false);
  return {};
}

static integer::value_type
arithmetic_shift_small(integer x, integer y) {
  if (y.value() > 0)
    return x.value() << y.value();
  else
    return x.value() >> -y.value();
}

static integer::value_type
bitwise_and_small(integer x, integer y) {
  return x.value() & y.value();
}

static integer::value_type
bitwise_or_small(integer x, integer y) {
  return x.value() | y.value();
}

ptr<>
arithmetic_shift(context&, ptr<> lhs, ptr<> rhs) {
  return bitwise_two<arithmetic_shift_small>(lhs, rhs);
}

ptr<>
bitwise_and(context&, ptr<> lhs, ptr<> rhs) {
  return bitwise_two<bitwise_and_small>(lhs, rhs);
}

ptr<>
bitwise_or(context&, ptr<> lhs, ptr<> rhs) {
  return bitwise_two<bitwise_or_small>(lhs, rhs);
}

ptr<>
bitwise_not(context&, ptr<> x) {
  if (auto value = match<integer>(x))
    return integer_to_ptr(integer{~value->value()});
  else
    throw std::runtime_error{"Only fixnums are supported"};
}

static std::size_t
integer_bit_length(integer::value_type i) {
  // For a positive integer, the bit length is the type width minus the number
  // of leading zeroes. This is because we imagine all integers to be preceded
  // by an infinite sequence of zeroes.
  //
  // Analogously, we imagine negative integers to be preceded by an infinite
  // sequence of ones, so the bit length of a negative value is the type width
  // minus the number of leading ones.

  using unsigned_type = std::make_unsigned_t<integer::value_type>;

  if (i >= 0)
    return integer::storage_width - std::countl_zero(static_cast<unsigned_type>(i));
  else
    return integer::storage_width - std::countl_one(static_cast<unsigned_type>(i));
}

static ptr<big_integer>
big_complement(context& ctx, ptr<big_integer> b) {
  auto minus_one = make<big_integer>(ctx, std::vector{big_integer::limb_type{1}}, false);
  return sub_big(ctx, minus_one, b);
}

static std::size_t
big_integer_bit_length(context& ctx, ptr<big_integer> b) {
  if (!b->positive())
    b = big_complement(ctx, b);

  return b->length() * big_integer::limb_width - std::countl_zero(b->back());
}

std::size_t
bit_length(context& ctx, ptr<> x) {
  if (auto i = match<integer>(x))
    return integer_bit_length(i->value());
  else if (auto b = match<big_integer>(x))
    return big_integer_bit_length(ctx, b);
  else
    throw std::runtime_error{"Expected integer"};
}

using primitive_relational_type = ptr<boolean>(context&, ptr<>, ptr<>);

template <primitive_relational_type* F>
ptr<>
relational(context& ctx, object_span xs, std::string const& name) {
  if (xs.size() < 2)
    throw std::runtime_error{fmt::format("Not enough arguments to {}", name)};

  ptr<> lhs = xs[0];
  for (std::size_t i = 1; i < xs.size(); ++i) {
    ptr<> rhs = xs[i];
    if (F(ctx, lhs, rhs) == ctx.constants->f.get())
      return ctx.constants->f.get();

    lhs = rhs;
  }

  return ctx.constants->t.get();
}

static bool
positive(ptr<> x) {
  if (auto small = match<integer>(x))
    return small->value() > 0;
  else {
    assert(is<big_integer>(x));
    return assume<big_integer>(x)->positive();
  }
}

namespace {
  enum class general_compare_result {
    less,
    greater,
    equal,
    incomparable
  };
}

static general_compare_result
opposite(general_compare_result r) {
  if (r == general_compare_result::less)
    return general_compare_result::greater;
  else if (r == general_compare_result::greater)
    return general_compare_result::less;
  else
    return r;
}

static general_compare_result
exact_to_general(exact_compare_result r) {
  switch (r) {
  case exact_compare_result::less: return general_compare_result::less;
  case exact_compare_result::greater: return general_compare_result::greater;
  case exact_compare_result::equal: return general_compare_result::equal;
  }
  return {};
}

static general_compare_result
compare(context& ctx, ptr<> lhs, ptr<> rhs) {
  switch (find_common_type(lhs, rhs)) {
  case common_type::small_integer: {
    integer::value_type x = assume<integer>(lhs).value();
    integer::value_type y = assume<integer>(rhs).value();

    if (x < y)
      return general_compare_result::less;
    else if (x > y)
      return general_compare_result::greater;
    else
      return general_compare_result::equal;
  }

  case common_type::big_integer:
    return exact_to_general(compare_big(make_big(ctx, lhs), make_big(ctx, rhs)));

  case common_type::fraction: {
    auto x = make_fraction(ctx, lhs);
    auto y = make_fraction(ctx, rhs);

    ptr<> lhs_num = multiply(ctx, x->numerator(), y->denominator());
    ptr<> rhs_num = multiply(ctx, x->denominator(), y->numerator());
    general_compare_result num_compare = compare(ctx, lhs_num, rhs_num);
    if (num_compare == general_compare_result::equal)
      return general_compare_result::equal;

    ptr<> common_den = multiply(ctx, x->denominator(), y->denominator());
    if (positive(common_den))
      return num_compare;
    else
      return opposite(num_compare);
  }

  case common_type::floating_point: {
    auto x = make_float(ctx, lhs);
    auto y = make_float(ctx, rhs);

    if (is_nan(x) || is_nan(y))
      return general_compare_result::incomparable;
    else if (x->value < y->value)
      return general_compare_result::less;
    else if (x->value > y->value)
      return general_compare_result::greater;
    else
      return general_compare_result::equal;
  }

  case common_type::complex: {
    auto x = make_complex(ctx, lhs);
    auto y = make_complex(ctx, rhs);

    auto re = compare(ctx, x->real(), y->real());
    auto im = compare(ctx, x->imaginary(), y->imaginary());

    if (re == general_compare_result::equal && im == general_compare_result::equal)
      return general_compare_result::equal;
    else
      return general_compare_result::incomparable;
  }
  }

  return {};
}

static void
throw_if_not_real(ptr<> x) {
  if (!is_real(x))
    throw std::runtime_error{"Expected a real number"};
}

ptr<boolean>
arith_equal(context& ctx, ptr<> lhs, ptr<> rhs) {
  return compare(ctx, lhs, rhs) == general_compare_result::equal ? ctx.constants->t.get() : ctx.constants->f.get();
}

ptr<>
arith_equal(context& ctx, object_span xs) {
  return relational<arith_equal>(ctx, xs, "=");
}

ptr<boolean>
less(context& ctx, ptr<> lhs, ptr<> rhs) {
  throw_if_not_real(lhs);
  throw_if_not_real(rhs);
  return compare(ctx, lhs, rhs) == general_compare_result::less ? ctx.constants->t.get() : ctx.constants->f.get();
}

ptr<>
less(context& ctx, object_span xs) {
  return relational<less>(ctx, xs, "<");
}

ptr<boolean>
greater(context& ctx, ptr<> lhs, ptr<> rhs) {
  throw_if_not_real(lhs);
  throw_if_not_real(rhs);
  return compare(ctx, lhs, rhs) == general_compare_result::greater ? ctx.constants->t.get() : ctx.constants->f.get();
}

ptr<>
greater(context& ctx, object_span xs) {
  return relational<greater>(ctx, xs, ">");
}

ptr<boolean>
less_or_equal(context& ctx, ptr<> lhs, ptr<> rhs) {
  throw_if_not_real(lhs);
  throw_if_not_real(rhs);

  general_compare_result cmp = compare(ctx, lhs, rhs);
  return (cmp == general_compare_result::less || cmp == general_compare_result::equal)
         ? ctx.constants->t.get() : ctx.constants->f.get();
}

ptr<>
less_or_equal(context& ctx, object_span xs) {
  return relational<less_or_equal>(ctx, xs, "<=");
}

ptr<boolean>
greater_or_equal(context& ctx, ptr<> lhs, ptr<> rhs) {
  throw_if_not_real(lhs);
  throw_if_not_real(rhs);

  general_compare_result cmp = compare(ctx, lhs, rhs);
  return (cmp == general_compare_result::greater || cmp == general_compare_result::equal)
         ? ctx.constants->t.get() : ctx.constants->f.get();
}

ptr<>
greater_or_equal(context& ctx, object_span xs) {
  return relational<greater_or_equal>(ctx, xs, ">=");
}

static bool
odd(ptr<big_integer> i) {
  if (i->zero())
    return false;
  return i->front() & 1;
}

static bool
even(ptr<big_integer> i) {
  return !odd(i);
}

static ptr<>
gcd_big(context& ctx, ptr<big_integer> x, ptr<big_integer> y) {
  if (x->zero())
    return y;
  if (y->zero())
    return x;

  x->set_positive(true);
  y->set_positive(true);

  std::size_t shift = 0;
  while (even(x) && even(y)) {
    assert(shift < std::numeric_limits<std::size_t>::max());
    ++shift;
    x = bitshift_right_destructive(x, 1);
    y = bitshift_right_destructive(y, 1);
  }

  while (even(x))
    x = bitshift_right_destructive(x, 1);

  while (!y->zero()) {
    while (even(y))
      y = bitshift_right_destructive(y, 1);

    if (compare_magnitude(x, y, normal_length(x), normal_length(y)) == exact_compare_result::greater)
      std::swap(x, y);

    y = sub_magnitude(ctx, y, x);
  }

  return normalize(ctx, bitshift_left_destructive(ctx, x, shift));
}

ptr<>
gcd(context& ctx, ptr<> x, ptr<> y) {
  switch (find_common_type(x, y)) {
  case common_type::small_integer:
    return integer_to_ptr(integer{std::gcd(assume<integer>(x).value(), assume<integer>(y).value())});
  case common_type::big_integer:
    return gcd_big(ctx, make_big_copy(ctx, x), make_big_copy(ctx, y));
  default:
    throw std::runtime_error{"gcd: Invalid type, expected integer"};
  }

  assert(false);
  return {};
}

static void
export_native(context& ctx, module_& m, std::string const& name,
              ptr<> (*f)(context&, object_span), special_top_level_tag tag) {
  auto index = ctx.add_top_level(ctx.store.make<native_procedure>(f, name.c_str()), name);
  ctx.tag_top_level(index, tag);

  auto name_sym = ctx.intern(name);
  auto id = make<syntax>(ctx, name_sym, scope_set{m.scope()});
  m.scope()->add(ctx.store, id, std::make_shared<variable>(name, index));
  m.export_(name_sym);
}

static ptr<floating_point>
small_integer_to_floating_point(context& ctx, integer::value_type i) {
  return make<floating_point>(ctx, static_cast<floating_point::value_type>(i));
}

static void
check_fit_in_floating_point(context& ctx, ptr<big_integer> b) {
  if (bit_length(ctx, b) > std::numeric_limits<floating_point::value_type>::max_exponent)
    throw std::runtime_error{"Value too large to fit into inexact number"};
}

static double
big_integer_to_double(context& ctx, ptr<big_integer> b) {
  check_fit_in_floating_point(ctx, b);

  double result = 0.0;
  auto* limbs = b->data();
  for (std::size_t i = 0; i < b->length(); ++i)
    result += std::ldexp(static_cast<floating_point::value_type>(limbs[i]),
                         static_cast<int>(i * big_integer::limb_width));

  return b->positive() ? result : -result;
}

static ptr<floating_point>
big_integer_to_floating_point(context& ctx, ptr<big_integer> b) {
  return make<floating_point>(ctx, big_integer_to_double(ctx, b));
}

static double
integer_to_double(context& ctx, ptr<> x) {
  if (auto i = match<integer>(x))
    return static_cast<double>(i->value());
  else {
    auto f = assume<big_integer>(x);
    return big_integer_to_double(ctx, f);
  }
}

static ptr<floating_point>
fraction_to_floating_point(context& ctx, ptr<fraction> f) {
  auto [quot, rem] = quotient_remainder(ctx, f->numerator(), f->denominator());
  double quot_double = integer_to_double(ctx, quot);
  double rem_double = integer_to_double(ctx, rem);
  double den_double = integer_to_double(ctx, f->denominator());
  return make<floating_point>(ctx, quot_double + rem_double / den_double);
}

ptr<>
inexact(context& ctx, ptr<> x) {
  if (auto i = match<integer>(x))
    return small_integer_to_floating_point(ctx, i->value());
  else if (auto b = match<big_integer>(x))
    return big_integer_to_floating_point(ctx, b);
  else if (auto f = match<fraction>(x))
    return fraction_to_floating_point(ctx, f);
  else if (auto fp = match<floating_point>(x))
    return fp;
  else if (auto z = match<complex>(x))
    return make<complex>(ctx, inexact(ctx, z->real()), inexact(ctx, z->imaginary()));
  else
    throw std::runtime_error{"Expected a number"};
}

static ptr<>
big_integer_power_of_2(context& ctx, unsigned exponent) {
  std::size_t num_limbs = exponent / big_integer::limb_width + 1;
  unsigned limb_exponent = exponent % big_integer::limb_width;

  auto result = make<big_integer>(ctx, num_limbs);
  result->back() = big_integer::limb_type{1} << limb_exponent;
  return result;
}

template <>
ptr<>
integer_power<2>(context& ctx, unsigned exponent) {
  assert(exponent > 0);

  if (exponent < integer::value_width - 1)
    return integer_to_ptr(integer::value_type{1} << exponent);
  else
    return big_integer_power_of_2(ctx, exponent);
}

static void
throw_if_not_representable_as_exact(ptr<floating_point> fp) {
  if (is_infinite(fp))
    throw std::runtime_error{"Infinity cannot be represented as exact number"};
  if (is_nan(fp))
    throw std::runtime_error{"NaN cannot be represented as exact number"};
}

ptr<>
floating_point_to_exact(context& ctx, ptr<floating_point> value) {
  throw_if_not_representable_as_exact(value);

  constexpr int radix = std::numeric_limits<floating_point::value_type>::radix;
  bool negative = value->value < 0.0;
  int exponent;
  floating_point::value_type f = std::frexp(std::fabs(value->value), &exponent);

  // f = 0 . d1 d2 ... dn * r^e, where di are digits in base-r, r is the radix, e the exponent.

  ptr<> numerator = integer_to_ptr(0);

  while (f != 0.0) {
    f *= radix;               // f = d1 . d2 d3 ... dn * r^e
    double d = std::trunc(f); // d1
    f -= d;                   // f = 0 . d2 d3 ... dn * r^e

    numerator = add(ctx,
                    integer_to_ptr(static_cast<integer::value_type>(d)),
                    multiply(ctx, numerator, integer_to_ptr(radix)));
    exponent -= 1;
  }

  if (negative)
    numerator = negate(ctx, numerator);

  if (exponent == 0)
    return numerator;
  else if (exponent > 0)
    return multiply(ctx, numerator, integer_power<radix>(ctx, exponent));
  else {
    ptr<> denominator = integer_power<radix>(ctx, -exponent);
    return normalize_fraction(ctx, make<fraction>(ctx, numerator, denominator));
  }
}

ptr<>
exact(context& ctx, ptr<> x) {
  if (auto f = match<floating_point>(x))
    return floating_point_to_exact(ctx, f);
  else if (is_exact(x))
    return x;
  else if (auto z = match<complex>(x))
    return make<complex>(ctx, exact(ctx, z->real()), exact(ctx, z->imaginary()));
  else
    throw std::runtime_error{"Expected a number"};
}

static ptr<>
real_exp(context& ctx, ptr<floating_point> x) {
  return make<floating_point>(ctx, std::exp(x->value));
}

using std_complex = std::complex<floating_point::value_type>;

static std_complex
to_std_complex(ptr<complex> z) {
  return {assume<floating_point>(z->real())->value,
          assume<floating_point>(z->imaginary())->value};
}

static ptr<>
from_std_complex(context& ctx, std_complex z) {
  if (z.imag() == 0.0)
    return make<floating_point>(ctx, z.real());
  else
    return make<complex>(ctx, make<floating_point>(ctx, z.real()), make<floating_point>(ctx, z.imag()));
}

ptr<>
complex_exp(context& ctx, ptr<complex> z) {
  return from_std_complex(ctx, std::exp(to_std_complex(z)));
}

template <auto Real, auto Complex>
static ptr<>
transcendental(context& ctx, ptr<> z) {
  if (is<complex>(z))
    return Complex(ctx, assume<complex>(inexact(ctx, z)));
  else if (is_real(z))
    return Real(ctx, assume<floating_point>(inexact(ctx, z)));
  else
    throw std::runtime_error{"Expected a number"};
}

ptr<>
exp(context& ctx, ptr<> z) {
  return transcendental<real_exp, complex_exp>(ctx, z);
}

static ptr<>
real_log(context& ctx, ptr<floating_point> x) {
  if (x->value > 0.0)
    return make<floating_point>(ctx, std::log(x->value));
  else
    return from_std_complex(ctx, std::log(std_complex{x->value, 0.0}));
}

static ptr<>
complex_log(context& ctx, ptr<complex> z) {
  return from_std_complex(ctx, std::log(to_std_complex(z)));
}

ptr<>
log(context& ctx, ptr<> z) {
  return transcendental<real_log, complex_log>(ctx, z);
}

static ptr<>
real_sin(context& ctx, ptr<floating_point> x) {
  return make<floating_point>(ctx, std::sin(x->value));
}

static ptr<>
complex_sin(context& ctx, ptr<complex> z) {
  return from_std_complex(ctx, std::sin(to_std_complex(z)));
}

static ptr<>
real_cos(context& ctx, ptr<floating_point> x) {
  return make<floating_point>(ctx, std::cos(x->value));
}

static ptr<>
complex_cos(context& ctx, ptr<complex> z) {
  return from_std_complex(ctx, std::cos(to_std_complex(z)));
}

ptr<>
sin(context& ctx, ptr<> z) {
  return transcendental<real_sin, complex_sin>(ctx, z);
}

ptr<>
cos(context& ctx, ptr<> z) {
  return transcendental<real_cos, complex_cos>(ctx, z);
}

static ptr<>
real_tan(context& ctx, ptr<floating_point> x) {
  return make<floating_point>(ctx, std::tan(x->value));
}

static ptr<>
complex_tan(context& ctx, ptr<complex> z) {
  return from_std_complex(ctx, std::tan(to_std_complex(z)));
}

ptr<>
tan(context& ctx, ptr<> z) {
  return transcendental<real_tan, complex_tan>(ctx, z);
}

static ptr<>
real_asin(context& ctx, ptr<floating_point> x) {
  return make<floating_point>(ctx, std::asin(x->value));
}

static ptr<>
complex_asin(context& ctx, ptr<complex> z) {
  return from_std_complex(ctx, std::asin(to_std_complex(z)));
}

ptr<>
asin(context& ctx, ptr<> z) {
  return transcendental<real_asin, complex_asin>(ctx, z);
}

static ptr<>
real_acos(context& ctx, ptr<floating_point> x) {
  return make<floating_point>(ctx, std::acos(x->value));
}

static ptr<>
complex_acos(context& ctx, ptr<complex> z) {
  return from_std_complex(ctx, std::acos(to_std_complex(z)));
}

ptr<>
acos(context& ctx, ptr<> z) {
  return transcendental<real_acos, complex_acos>(ctx, z);
}

static ptr<>
real_atan(context& ctx, ptr<floating_point> x) {
  return make<floating_point>(ctx, std::atan(x->value));
}

static ptr<>
complex_atan(context& ctx, ptr<complex> z) {
  return from_std_complex(ctx, std::atan(to_std_complex(z)));
}

ptr<>
atan(context& ctx, ptr<> z) {
  return transcendental<real_atan, complex_atan>(ctx, z);
}

ptr<>
atan2(context& ctx, ptr<> y, ptr<> x) {
  if (!is_real(x) || !is_real(y))
    throw std::runtime_error{"Expected real number"};

  double y_fl = to_float_value(y);
  double x_fl = to_float_value(x);
  return make<floating_point>(ctx, std::atan2(y_fl, x_fl));
}

static std_complex
std_sqrt(std_complex z) {
  auto result = std::sqrt(z);
  if (result.real() == 0.0 && result.imag() < 0.0)
    return -result;
  else
    return result;
}

ptr<>
square(context& ctx, ptr<> z) {
  return multiply(ctx, z, z);
}

static ptr<>
real_sqrt(context& ctx, ptr<floating_point> x) {
  if (x->value >= 0)
    return make<floating_point>(ctx, std::sqrt(x->value));
  else
    return from_std_complex(ctx, std_sqrt(std_complex{x->value, 0}));
}

static ptr<>
complex_sqrt(context& ctx, ptr<complex> z) {
  return from_std_complex(ctx, std_sqrt(to_std_complex(z)));
}

ptr<>
sqrt(context& ctx, ptr<> z) {
  return transcendental<real_sqrt, complex_sqrt>(ctx, z);
}

static std_complex
to_inexact_complex(ptr<> x) {
  if (auto z = match<complex>(x))
    return {to_float_value(z->real()), to_float_value(z->imaginary())};
  else if (is_real(x))
    return {to_float_value(x), 0.0};
  else
    throw std::runtime_error{"Expected a number"};
}

static ptr<>
real_expt(context& ctx, ptr<> base, ptr<> exponent) {
  return make<floating_point>(ctx, std::pow(to_float_value(base), to_float_value(exponent)));
}

static ptr<>
complex_expt(context& ctx, ptr<> base, ptr<> exponent) {
  return from_std_complex(ctx, std::pow(to_inexact_complex(base), to_inexact_complex(exponent)));
}

static ptr<>
inexact_expt(context& ctx, ptr<> base, ptr<> exponent) {
  if (is_real(base) && is_real(exponent))
    return real_expt(ctx, base, exponent);
  else
    return complex_expt(ctx, base, exponent);
}

static ptr<>
exact_integral_expt_of_generic_base(context& ctx, ptr<> base, ptr<> exponent) {
  // b^(a_n 2^n + a_(n - 1) 2^(n - 1) + ... + a_0)
  // = b^(a_n 2^n) * b^(a_(n - 1) 2^(n - 1)) * ... * 2^(a_0)
  // = b^(2^(k_1)) * b^(2^(k_2)) * ... b^(2^(k_m)),
  // where k_i is the maximal subsequence of (1, ..., n) such that a_(k_i) is 1 for all i.

  ptr<> result = integer_to_ptr(1);
  ptr<> abs_exponent = is_negative(exponent) ? multiply(ctx, exponent, integer_to_ptr(-1)) : exponent;
  ptr<> base_to_2i = base; // b^(2^i) where i is the number of iterations of the while loop below

  while (!is_zero(abs_exponent)) {
    if (least_significant_bit(abs_exponent))
      result = multiply(ctx, result, base_to_2i);

    base_to_2i = multiply(ctx, base_to_2i, base_to_2i);
    abs_exponent = bitshift_right(ctx, abs_exponent, 1);
  }

  if (is_negative(exponent))
    return normalize_fraction(ctx, make<fraction>(ctx, integer_to_ptr(1), result));
  else
    return result;
}

static ptr<>
integer_power_of_negative_1(ptr<> exponent) {
  if (is_even(exponent))
    return integer_to_ptr(1);
  else
    return integer_to_ptr(-1);
}

static ptr<>
exact_integral_expt(context& ctx, ptr<> base, ptr<> exponent) {
  assert(is_exact_integer(exponent));

  if (base == integer_to_ptr(2) && is<integer>(exponent) && !is_negative(exponent))
    return integer_power<2>(ctx, static_cast<unsigned>(assume<integer>(exponent).value()));
  else if (base == integer_to_ptr(-1))
    return integer_power_of_negative_1(exponent);
  else
    return exact_integral_expt_of_generic_base(ctx, base, exponent);
}

ptr<>
expt(context& ctx, ptr<> base, ptr<> exponent) {
  if (is_inexact(base) || is_inexact(exponent) || !is_exact_integer(exponent))
    return inexact_expt(ctx, base, exponent);
  else
    return exact_integral_expt(ctx, base, exponent);
}

ptr<>
read_integer(context& ctx, std::string const& digits, unsigned base) {
  ptr<> result = integer_to_ptr(integer{0});

  for (char32_t c : digits) {
    result = mul_magnitude_by_limb_destructive(ctx, result, base);
    result = add_magnitude_to_limb_destructive(ctx, result, digit_value(c));
  }

  if (auto b = match<big_integer>(result))
    result = normalize(ctx, b);

  return result;
}

void
export_numeric(context& ctx, module_& result) {
  export_native(ctx, result, "+", add, special_top_level_tag::plus);
  export_native(ctx, result, "-", subtract, special_top_level_tag::minus);
  export_native(ctx, result, "*", multiply, special_top_level_tag::times);
  export_native(ctx, result, "/", divide, special_top_level_tag::divide);
  export_native(ctx, result, "=", arith_equal, special_top_level_tag::arith_equal);
  export_native(ctx, result, "<", less, special_top_level_tag::less_than);
  export_native(ctx, result, "<=", less_or_equal, special_top_level_tag::less_or_equal);
  export_native(ctx, result, ">", greater, special_top_level_tag::greater_than);
  export_native(ctx, result, ">=", greater_or_equal, special_top_level_tag::greater_or_equal);
  define_procedure(ctx, "gcd", result, true, gcd);
  define_procedure(ctx, "arithmetic-shift", result, true, arithmetic_shift);
  define_procedure(ctx, "bitwise-and", result, true, bitwise_and);
  define_procedure(ctx, "bitwise-or", result, true, bitwise_or);
  define_procedure(ctx, "bitwise-not", result, true, bitwise_not);
  define_procedure(ctx, "integer?", result, true, is_integer);
  define_procedure(ctx, "exact-integer?", result, true, is_exact_integer);
  define_procedure(ctx, "odd?", result, true, is_odd);
  define_procedure(ctx, "even?", result, true, is_even);
  define_procedure(ctx, "zero?", result, true, is_zero);
  define_procedure(ctx, "number?", result, true, is_number);
  define_procedure(ctx, "exp", result, true, exp);
  define_procedure(ctx, "log", result, true, log);
  define_procedure(ctx, "expt", result, true, expt);
}

} // namespace insider
