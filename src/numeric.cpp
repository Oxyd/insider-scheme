#include "numeric.hpp"

#include "converters.hpp"
#include "io.hpp"
#include "scheme.hpp"

#include <algorithm>
#include <cmath>
#include <cstdlib>
#include <ios>
#include <locale>
#include <numeric>
#include <sstream>

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
big_integer::extra_elements(ptr<integer> const& i) {
  return number_of_limbs_for_small_integer(i->value());
}

std::size_t
big_integer::extra_elements(ptr<big_integer> const& i) {
  return i->length();
}

big_integer::big_integer(std::size_t length)
  : length_{length}
{
  std::fill(begin(), end(), limb_type{0});
}

big_integer::big_integer(std::size_t length, dont_initialize_t)
  : length_{length}
{ }

big_integer::big_integer(std::vector<limb_type> const& limbs, bool positive)
  : length_{limbs.size()}
  , positive_{positive}
{
  assert(static_cast<std::vector<limb_type>::size_type>(end() - begin()) == limbs.size());
  std::copy(limbs.begin(), limbs.end(), begin());
}

big_integer::big_integer(ptr<big_integer> const& i)
  : length_{i->length()}
  , positive_{i->positive()}
{
  std::copy(i->begin(), i->end(), begin());
}

static std::tuple<bool, integer::storage_type>
short_integer_to_sign_magnitude(integer::value_type i) {
  if (i == 0)
    return {true, 0};
  else if (i > 0)
    return {true, i};
  else
    return {false, ~*reinterpret_cast<integer::storage_type*>(&i) + 1};
}

big_integer::big_integer(ptr<integer> const& i)
  : length_{number_of_limbs_for_small_integer(i->value())}
{
  auto [sign, magnitude] = short_integer_to_sign_magnitude(i->value());
  positive_ = sign;

  if constexpr (sizeof(integer::storage_type) <= sizeof(limb_type)) {
    front() = magnitude;
  } else {
    auto it = begin();
    for (std::size_t k = 0; k < number_of_limbs_for_small_integer(i->value()); ++k) {
      *it++ = magnitude & limb_mask;
      magnitude >>= limb_width;
    }
  }
}

big_integer::big_integer(big_integer&& other)
  : length_{other.length_}
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
  return &storage_element(0) + length_;
}

auto
big_integer::rbegin() -> reverse_iterator {
  return reverse_iterator{end()};
}

auto
big_integer::rend() -> reverse_iterator {
  return reverse_iterator{begin()};
}

fraction::fraction(generic_ptr const& num, generic_ptr const& den)
  : numerator_{num.get()}
  , denominator_{den.get()}
{
  assert(is_integer(num));
  assert(is_integer(den));
}

static bool
digit(char c) {
  return c >= '0' && c <= '9';
}

static unsigned
digit_value(char c) {
  if (c >= '0' && c <= '9')
    return c - '0';
  else if (c >= 'a' && c <= 'f')
    return c - 'a' + 10;
  else
    return c - 'A' + 10;
}

static ptr<big_integer>
extend_big(context& ctx, ptr<big_integer> const& i, limb_type new_limb) {
  auto result = make<big_integer>(ctx, i->length() + 1, big_integer::dont_initialize);
  std::copy(i->begin(), i->end(), result->begin());
  result->back() = new_limb;
  result->set_positive(i->positive());
  return result;
}

static std::size_t
normal_length(ptr<big_integer> const& i) {
  std::size_t result = i->length();
  while (result > 0 && i->data()[result - 1] == limb_type{0})
    --result;

  return result;
}

static generic_ptr
normalize(context& ctx, ptr<big_integer> const& i) {
  std::size_t new_length = normal_length(i);

  if (new_length == 0)
    return make<integer>(ctx, 0);

  if constexpr (max_limb_value >= integer::max) {
    if (new_length == 1) {
      limb_type l = i->front();
      if (i->positive() && l <= integer::max)
        return make<integer>(ctx, l);
      else if (!i->positive() && l <= -integer::min) {
        return make<integer>(ctx, ~l + 1);
      }
    }
  }
  else {
    if (new_length <= sizeof(integer::storage_type) / sizeof(limb_type)) {
      integer::storage_type small = 0;
      for (std::size_t k = i->length(); k > 0; --k)
        small = (small << limb_width) | i->data()[k - 1];

      if (i->positive() && small <= integer::max)
        return make<integer>(ctx, small);
      else if (!i->positive() && small <= integer::storage_type(-integer::min)) {
        assert(small <= std::numeric_limits<integer::value_type>::max());
        return make<integer>(ctx, -static_cast<integer::value_type>(small));
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

static generic_ptr
normalize_fraction(context& ctx, ptr<fraction> const& q) {
  generic_ptr num = fraction_numerator(q);
  generic_ptr den = fraction_denominator(q);

  if (auto d = match<integer>(den)) {
    if (d->value() == 0)
      throw std::runtime_error{"0 in fraction denominator"};
  }

  if (auto n = match<integer>(num)) {
    if (n->value() == 0)
      return n;
  }

  generic_ptr com_den = gcd(ctx, num, den);
  if (auto c = match<integer>(com_den)) {
    if (c->value() == 1)
      return q;
  }

  return make<fraction>(ctx, truncate_quotient(ctx, num, com_den), truncate_quotient(ctx, den, com_den));
}

static ptr<big_integer>
add_big_magnitude_to_limb_destructive(context& ctx, ptr<big_integer> result,
                                      ptr<big_integer> const& lhs, limb_type rhs) {
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

static generic_ptr
add_magnitude_to_limb_destructive(context& ctx, generic_ptr lhs, limb_type rhs) {
  if (auto b = match<big_integer>(lhs))
    return add_big_magnitude_to_limb_destructive(ctx, b, b, rhs);

  auto lhs_int = assume<integer>(lhs);
  double_limb_type sum = lhs_int->value() + rhs;
  if (sum <= integer::max) {
    lhs_int->set_value(static_cast<integer::value_type>(sum));
    return lhs_int;
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
  enum class compare {
    less,
    equal,
    greater
  };
}

static compare
compare_magnitude(ptr<big_integer> const& lhs, ptr<big_integer> const& rhs,
                  std::size_t lhs_length, std::size_t rhs_length) {
  if (lhs_length < rhs_length)
    return compare::less;
  else if (lhs_length > rhs_length)
    return compare::greater;

  for (std::size_t i = lhs_length; i > 0; --i) {
    limb_type x = lhs->data()[i - 1];
    limb_type y = rhs->data()[i - 1];

    if (x != y) {
      if (x < y)
        return compare::less;
      else
        return compare::greater;
    }
  }

  return compare::equal;
}

static compare
compare_magnitude(ptr<big_integer> const& lhs, ptr<big_integer> const& rhs) {
  return compare_magnitude(lhs, rhs, lhs->length(), rhs->length());
}

static ptr<big_integer>
sub_magnitude(context& ctx, ptr<big_integer> lhs, ptr<big_integer> rhs) {
  // We want the bigger number to be the LHS to simplify stuff below.

  bool positive = true;
  std::size_t lhs_length = normal_length(lhs);
  std::size_t rhs_length = normal_length(rhs);
  switch (compare_magnitude(lhs, rhs, lhs_length, rhs_length)) {
  case compare::less:
    std::swap(lhs, rhs);
    std::swap(lhs_length, rhs_length);
    positive = false;
    break;
  case compare::equal:
    return make<big_integer>(ctx, 0);
  case compare::greater:
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
flip_sign(ptr<big_integer> const& i) {
  i->set_positive(!i->positive());
  return i;
}

static generic_ptr
flip_sign(generic_ptr const& i) {
  if (auto b = match<big_integer>(i))
    return flip_sign(b);

  auto small = assume<integer>(i);
  small->set_value(-small->value());
  return small;
}

static ptr<big_integer>
set_sign_copy(context& ctx, ptr<big_integer> const& value, bool sign) {
  if (value->positive() == sign)
    return value;

  auto result = make<big_integer>(ctx, value);
  return flip_sign(result);
}

static bool
overflow(integer::storage_type i) {
  return detail::highest_storage_bit(i) != detail::highest_value_bit(i);
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

static generic_ptr
add_small(context& ctx, ptr<integer> const& lhs, ptr<integer> const& rhs) {
  integer::storage_type sum = lhs->data() + rhs->data();
  if (overflow(sum))
    return add_big(ctx, make<big_integer>(ctx, lhs), make<big_integer>(ctx, rhs));
  else
    return make<integer>(ctx, sum);
}

static ptr<fraction>
add_fraction(context& ctx, ptr<fraction> const& lhs, ptr<fraction> const& rhs) {
  //  a     c    ad + bc
  // --- + --- = -------
  //  b     d      bd

  return make<fraction>(ctx,
                        add(ctx,
                            multiply(ctx, fraction_numerator(lhs), fraction_denominator(rhs)),
                            multiply(ctx, fraction_denominator(lhs), fraction_numerator(rhs))),
                        multiply(ctx, fraction_denominator(lhs), fraction_denominator(rhs)));
}

static ptr<floating_point>
add_float(context& ctx, ptr<floating_point> const& lhs, ptr<floating_point> const& rhs) {
  return make<floating_point>(ctx, lhs->value + rhs->value);
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

static generic_ptr
sub_small(context& ctx, ptr<integer> const& lhs, ptr<integer> const& rhs) {
  integer::storage_type dif = lhs->data() + ~rhs->data() + 1;
  if (overflow(dif))
    return sub_big(ctx, make<big_integer>(ctx, lhs), make<big_integer>(ctx, rhs));
  else
    return make<integer>(ctx, dif);
}

static ptr<fraction>
sub_fraction(context& ctx, ptr<fraction> const& lhs, ptr<fraction> const& rhs) {
  return make<fraction>(ctx,
                        subtract(ctx,
                                 multiply(ctx, fraction_numerator(lhs), fraction_denominator(rhs)),
                                 multiply(ctx, fraction_denominator(lhs), fraction_numerator(rhs))),
                        multiply(ctx, fraction_denominator(lhs), fraction_denominator(rhs)));
}

static ptr<floating_point>
sub_float(context& ctx, ptr<floating_point> const& lhs, ptr<floating_point> const& rhs) {
  return make<floating_point>(ctx, lhs->value - rhs->value);
}

static std::tuple<limb_type, limb_type>
mul_limb_by_limb(limb_type lhs, limb_type rhs) {
  double_limb_type result = double_limb_type{lhs} * double_limb_type{rhs};
  return {limb_type(result >> limb_width), limb_type(result & limb_mask)};
}

static ptr<big_integer>
mul_big_magnitude_by_limb_destructive(context& ctx, ptr<big_integer> result,
                                      ptr<big_integer> const& lhs, limb_type rhs) {
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
mul_big_magnitude_by_limb(context& ctx, ptr<big_integer> const& lhs, limb_type rhs) {
  return mul_big_magnitude_by_limb_destructive(ctx, {}, lhs, rhs);
}

static bool
small_mul_overflow(integer::storage_type x, integer::storage_type y) {
  return x > static_cast<integer::storage_type>(integer::max) / y;
}

static generic_ptr
mul_magnitude_by_limb_destructive(context& ctx, generic_ptr lhs, limb_type rhs) {
  if (auto b = match<big_integer>(lhs))
    return mul_big_magnitude_by_limb_destructive(ctx, b, b, rhs);

  auto lhs_int = assume<integer>(lhs);
  assert(lhs_int->value() >= 0);

  if (small_mul_overflow(lhs_int->value(), rhs))
    return mul_big_magnitude_by_limb_destructive(ctx, {}, make<big_integer>(ctx, lhs_int), rhs);

  lhs_int->set_value(lhs_int->value() * rhs);
  return lhs_int;
}

static bool
magnitude_one(ptr<big_integer> const& i) {
  return i->length() == 1 && i->front() == 1;
}

static ptr<big_integer>
shift(context& ctx, ptr<big_integer> const& i, unsigned k) {
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
  for (std::size_t i = 1; i < rhs->length(); ++i) {
    auto term = mul_big_magnitude_by_limb(ctx, lhs, rhs->data()[i]);
    if (!term->zero())
      result = add_big_magnitude(ctx, result, shift(ctx, term, i));
  }

  if (lhs->positive() != rhs->positive())
    result->set_positive(false);

  return result;
}

static generic_ptr
mul_small(context& ctx, ptr<integer> const& lhs, ptr<integer> const& rhs) {
  integer::storage_type x = lhs->value() > 0 ? lhs->value() : -lhs->value();
  integer::storage_type y = rhs->value() > 0 ? rhs->value() : -rhs->value();
  bool result_positive = (lhs->value() > 0) == (rhs->value() > 0);

  if (small_mul_overflow(x, y))
    return mul_big(ctx, make<big_integer>(ctx, lhs), make<big_integer>(ctx, rhs));

  integer::value_type product = x * y;
  return make<integer>(ctx, result_positive ? product : -product);
}

static ptr<fraction>
mul_fraction(context& ctx, ptr<fraction> const& lhs, ptr<fraction> const& rhs) {
  return make<fraction>(ctx,
                        multiply(ctx, fraction_numerator(lhs), fraction_numerator(rhs)),
                        multiply(ctx, fraction_denominator(lhs), fraction_denominator(rhs)));
}

static ptr<floating_point>
mul_float(context& ctx, ptr<floating_point> const& lhs, ptr<floating_point> const& rhs) {
  return make<floating_point>(ctx, lhs->value * rhs->value);
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

  limb_type const bottom_k_bits = ~limb_type{1} >> (limb_width - k);
  assert((i->front() & bottom_k_bits) == 0);

  for (std::size_t n = 0; n < i->length(); ++n) {
    limb_type& current = i->data()[n];
    current >>= k;

    if (n + 1 < i->length()) {
      limb_type upper = i->data()[n + 1];
      current |= (upper & bottom_k_bits) << (limb_width - k);
    }
  }

  return i;
}

static ptr<big_integer>
bitshift_right(context& ctx, ptr<big_integer> i, std::size_t k) {
  return bitshift_right_destructive(make<big_integer>(ctx, i), k);
}

static unsigned
count_leading_zeroes(limb_type i) {
  limb_type const top_bit = limb_type{1} << (limb_width - 1);

  unsigned count = 0;
  while ((i & top_bit) == 0) {
    i <<= 1;
    ++count;
  }

  return count;
}

static limb_type
guess_quotient(limb_type a_hi, limb_type a_lo, limb_type b) {
  double_limb_type a = (double_limb_type{a_hi} << limb_width) | a_lo;
  double_limb_type q = a / b;
  return std::min(q, double_limb_type{max_limb_value});
}

static std::tuple<ptr<big_integer>, limb_type>
div_rem_by_limb_magnitude(context& ctx, ptr<big_integer> dividend, limb_type divisor) {
  auto quotient = make<big_integer>(ctx, dividend->length());
  double_limb_type d{};

  for (std::size_t i = dividend->length(); i > 0; --i) {
    d = (d << limb_width) | dividend->data()[i - 1];
    limb_type q = d / divisor;
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

  unsigned normalisation_shift = count_leading_zeroes(divisor->back());
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
  if (compare_magnitude(dividend, first_shifted_divisor) != compare::less) {
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

static std::tuple<generic_ptr, generic_ptr>
div_rem_big(context& ctx, ptr<big_integer> const& dividend, ptr<big_integer> const& divisor) {
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

static std::tuple<ptr<integer>, ptr<integer>>
div_rem_small(context& ctx, ptr<integer> const& dividend, ptr<integer> const& divisor) {
  auto [quot, rem] = std::div(dividend->value(), divisor->value());
  return {make<integer>(ctx, quot), make<integer>(ctx, rem)};
}

static compare
compare_big(ptr<big_integer> const& lhs, ptr<big_integer> const& rhs) {
  if (lhs->positive() != rhs->positive()) {
    if (!lhs->positive() && rhs->positive())
      return compare::less;
    else
      return compare::greater;
  }

  compare result = compare_magnitude(lhs, rhs);

  if (result == compare::equal)
    return result;

  if (!lhs->positive()) {
    if (result == compare::less)
      return compare::greater;
    else
      return compare::less;
  }

  return result;
}

namespace {
  enum class common_type {
    small_integer,
    big_integer,
    fraction,
    floating_point
  };
}

static common_type
find_common_type(generic_ptr const& lhs, generic_ptr const& rhs) {
  if (!is_number(lhs) || !is_number(rhs))
    throw std::runtime_error{"Expected number"};

  if (is<floating_point>(lhs) || is<floating_point>(rhs))
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
make_big(context& ctx, generic_ptr const& x) {
  if (auto b = match<big_integer>(x))
    return b;
  else if (auto s = match<integer>(x)) {
    return make<big_integer>(ctx, s);
  }
  else {
    assert(!"Can't happen");
    return {};
  }
}

static ptr<big_integer>
make_big_copy(context& ctx, generic_ptr const& x) {
  if (auto b = match<big_integer>(x))
    return make<big_integer>(ctx, b);
  else if (auto s = match<integer>(x)) {
    return make<big_integer>(ctx, s);
  }
  else {
    assert(!"Can't happen");
    return {};
  }
}

static ptr<fraction>
make_fraction(context& ctx, generic_ptr const& x) {
  if (is<integer>(x) || is<big_integer>(x))
    return make<fraction>(ctx, x, make<integer>(ctx, 1));

  assert(is<fraction>(x));
  return assume<fraction>(x);
}

static floating_point::value_type
big_to_float_value(ptr<big_integer> const& n) {
  floating_point::value_type result = 0;

  for (std::size_t i = n->length(); i > 0; --i)
    result = result * (static_cast<double>(max_limb_value) + 1.0) + n->data()[i - 1];

  if (n->positive())
    return result;
  else
    return -result;
}

static floating_point::value_type
integer_to_float_value(generic_ptr const& n) {
  if (auto s = match<integer>(n))
    return s->value();
  else
    return big_to_float_value(assume<big_integer>(n));
}

static ptr<floating_point>
make_float(context& ctx, generic_ptr const& x) {
  if (auto f = match<floating_point>(x))
    return f;
  else if (auto n = match<integer>(x))
    return make<floating_point>(ctx, n->value());
  else if (auto n = match<big_integer>(x))
    return make<floating_point>(ctx, big_to_float_value(n));
  else if (auto q = match<fraction>(x))
    return make<floating_point>(
      ctx,
      integer_to_float_value(fraction_numerator(q))
      / integer_to_float_value(fraction_denominator(q))
    );

  assert(false);
  return {};
}

template <auto F>
generic_ptr
arithmetic(context& ctx, std::vector<generic_ptr> const& xs, bool allow_empty, integer::value_type neutral) {
  if (xs.empty()) {
    if (allow_empty)
      return make<integer>(ctx, neutral);
    else
      throw std::runtime_error{"Not enough arguments"};
  }
  else if (xs.size() == 1)
    return F(ctx, make<integer>(ctx, neutral), expect<integer>(xs.front()));
  else {
    generic_ptr result = xs.front();
    for (auto rhs = xs.begin() + 1; rhs != xs.end(); ++rhs)
      result = F(ctx, result, expect<integer>(*rhs));

    return result;
  }
}

using primitive_arithmetic_type = generic_ptr(context& ctx, generic_ptr const&, generic_ptr const&);

template <auto Small, auto Big, auto Fraction, auto Float>
generic_ptr
arithmetic_two(context& ctx, generic_ptr const& lhs, generic_ptr const& rhs) {
  switch (find_common_type(lhs, rhs)) {
  case common_type::small_integer:
    return Small(ctx, assume<integer>(lhs), assume<integer>(rhs));
  case common_type::big_integer:
    return normalize(ctx, Big(ctx, make_big(ctx, lhs), make_big(ctx, rhs)));
  case common_type::fraction:
    return normalize_fraction(ctx, Fraction(ctx, make_fraction(ctx, lhs), make_fraction(ctx, rhs)));
  case common_type::floating_point:
    return Float(ctx, make_float(ctx, lhs), make_float(ctx, rhs));
  }

  assert(false);
  return {};
}

bool
is_integer(generic_ptr const& x) {
  return is<integer>(x) || is<big_integer>(x);
}

bool
is_number(generic_ptr const& x) {
  return is_integer(x) || is<fraction>(x) || is<floating_point>(x);
}

generic_ptr
add(context& ctx, generic_ptr const& lhs, generic_ptr const& rhs) {
  return arithmetic_two<add_small, add_big, add_fraction, add_float>(ctx, lhs, rhs);
}

generic_ptr
add(context& ctx, std::vector<generic_ptr> const& xs) {
  return arithmetic<static_cast<primitive_arithmetic_type*>(&add)>(ctx, xs, true, 0);
}

generic_ptr
subtract(context& ctx, generic_ptr const& lhs, generic_ptr const& rhs) {
  return arithmetic_two<sub_small, sub_big, sub_fraction, sub_float>(ctx, lhs, rhs);
}

generic_ptr
subtract(context& ctx, std::vector<generic_ptr> const& xs) {
  return arithmetic<static_cast<primitive_arithmetic_type*>(&subtract)>(ctx, xs, false, 0);
}

generic_ptr
multiply(context& ctx, generic_ptr const& lhs, generic_ptr const& rhs) {
  return arithmetic_two<mul_small, mul_big, mul_fraction, mul_float>(ctx, lhs, rhs);
}

generic_ptr
multiply(context& ctx, std::vector<generic_ptr> const& xs) {
  return arithmetic<static_cast<primitive_arithmetic_type*>(&multiply)>(ctx, xs, true, 1);
}

generic_ptr
truncate_quotient(context& ctx, generic_ptr const& lhs, generic_ptr const& rhs) {
  return std::get<0>(quotient_remainder(ctx, lhs, rhs));
}

generic_ptr
truncate_quotient(context& ctx, std::vector<generic_ptr> const& xs) {
  return arithmetic<static_cast<primitive_arithmetic_type*>(&truncate_quotient)>(ctx, xs, false, 1);
}

std::tuple<generic_ptr, generic_ptr>
quotient_remainder(context& ctx, generic_ptr const& lhs, generic_ptr const& rhs) {
  switch (find_common_type(lhs, rhs)) {
  case common_type::small_integer:
    return div_rem_small(ctx, assume<integer>(lhs), assume<integer>(rhs));
  case common_type::big_integer:
    return div_rem_big(ctx, make_big(ctx, lhs), make_big(ctx, rhs));
  default:
    throw std::runtime_error{"Expected integer"};
  }

  assert(false);
  return {};
}

static generic_ptr
div_fraction(context& ctx, ptr<fraction> const& x, ptr<fraction> const& y) {
  return normalize_fraction(
    ctx,
    make<fraction>(ctx,
                   multiply(ctx, fraction_numerator(x), fraction_denominator(y)),
                   multiply(ctx, fraction_denominator(x), fraction_numerator(y)))
  );
}

static ptr<floating_point>
div_float(context& ctx, ptr<floating_point> const& x, ptr<floating_point> const& y) {
  return make<floating_point>(ctx, x->value / y->value);
}

generic_ptr
divide(context& ctx, generic_ptr const& lhs, generic_ptr const& rhs) {
  switch (find_common_type(lhs, rhs)) {
  case common_type::small_integer:
  case common_type::big_integer:
  case common_type::fraction:
    return div_fraction(ctx, make_fraction(ctx, lhs), make_fraction(ctx, rhs));

  case common_type::floating_point:
    return div_float(ctx, make_float(ctx, lhs), make_float(ctx, rhs));
  }

  assert(false);
  return {};
}

using primitive_relational_type = ptr<boolean>(context&, generic_ptr const&, generic_ptr const&);

template <primitive_relational_type* F>
generic_ptr
relational(context& ctx, std::vector<generic_ptr> const& xs, std::string const& name) {
  if (xs.size() < 2)
    throw std::runtime_error{fmt::format("Not enough arguments to {}", name)};

  ptr<integer> lhs = expect<integer>(xs[0]);
  for (std::size_t i = 1; i < xs.size(); ++i) {
    ptr<integer> rhs = expect<integer>(xs[i]);
    if (F(ctx, lhs, rhs) == ctx.constants->f)
      return ctx.constants->f;

    lhs = rhs;
  }

  return ctx.constants->t;
}

ptr<boolean>
arith_equal(context& ctx, generic_ptr const& lhs, generic_ptr const& rhs) {
  switch (find_common_type(lhs, rhs)) {
  case common_type::small_integer:
    return assume<integer>(lhs)->value() == assume<integer>(rhs)->value() ? ctx.constants->t : ctx.constants->f;
  case common_type::big_integer:
    return compare_big(make_big(ctx, lhs), make_big(ctx, rhs)) == compare::equal
           ? ctx.constants->t : ctx.constants->f;
  case common_type::fraction: {
    auto l = make_fraction(ctx, lhs);
    auto r = make_fraction(ctx, rhs);
    if (arith_equal(ctx, fraction_numerator(l), fraction_numerator(r)) == ctx.constants->f)
      return ctx.constants->f;
    if (arith_equal(ctx, fraction_denominator(l), fraction_denominator(r)) == ctx.constants->f)
      return ctx.constants->f;
    return ctx.constants->t;
  }

  case common_type::floating_point:
    if (make_float(ctx, lhs)->value == make_float(ctx, rhs)->value)
      return ctx.constants->t;
    else
      return ctx.constants->f;
  }

  assert(false);
  return {};
}

generic_ptr
arith_equal(context& ctx, std::vector<generic_ptr> const& xs) {
  return relational<arith_equal>(ctx, xs, "=");
}

ptr<boolean>
less(context& ctx, generic_ptr const& lhs, generic_ptr const& rhs) {
  return expect<integer>(lhs)->value() < expect<integer>(rhs)->value() ? ctx.constants->t : ctx.constants->f;
}

generic_ptr
less(context& ctx, std::vector<generic_ptr> const& xs) {
  return relational<less>(ctx, xs, "<");
}

ptr<boolean>
greater(context& ctx, generic_ptr const& lhs, generic_ptr const& rhs) {
  return expect<integer>(lhs)->value() > expect<integer>(rhs)->value() ? ctx.constants->t : ctx.constants->f;
}

generic_ptr
greater(context& ctx, std::vector<generic_ptr> const& xs) {
  return relational<greater>(ctx, xs, ">");
}

static bool
odd(ptr<big_integer> const& i) {
  if (i->zero())
    return false;
  return i->front() & 1;
}

static bool
even(ptr<big_integer> const& i) {
  return !odd(i);
}

static generic_ptr
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

    if (compare_magnitude(x, y, normal_length(x), normal_length(y)) == compare::greater)
      std::swap(x, y);

    y = sub_magnitude(ctx, y, x);
  }

  return normalize(ctx, bitshift_left_destructive(ctx, x, shift));
}

generic_ptr
gcd(context& ctx, generic_ptr const& x, generic_ptr const& y) {
  switch (find_common_type(x, y)) {
  case common_type::small_integer:
    return make<integer>(ctx, std::gcd(assume<integer>(x)->value(), assume<integer>(y)->value()));
  case common_type::big_integer:
    return gcd_big(ctx, make_big_copy(ctx, x), make_big_copy(ctx, y));
  default:
    throw std::runtime_error{"gcd: Invalid type, expected integer"};
  }

  assert(false);
  return {};
}

static void
export_native(context& ctx, module& m, std::string const& name,
              generic_ptr (*f)(context&, std::vector<generic_ptr> const&), special_top_level_tag tag) {
  auto index = ctx.add_top_level(ctx.store.make<native_procedure>(f), name);
  ctx.tag_top_level(index, tag);
  m.add(name, index);
  m.export_(name);
}

generic_ptr
read_integer(context& ctx, std::string const& digits, unsigned base) {
  generic_ptr result = make<integer>(ctx, 0);

  for (char c : digits) {
    result = mul_magnitude_by_limb_destructive(ctx, result, base);
    result = add_magnitude_to_limb_destructive(ctx, result, digit_value(c));
  }

  if (auto b = match<big_integer>(result))
    result = normalize(ctx, b);

  return result;
}

static std::optional<char>
peek_next(ptr<port> const& stream) {
  stream->read_char();
  return stream->peek_char();
}

static std::string
read_digits(ptr<port> const& stream) {
  std::optional<char> c = stream->peek_char();

  std::string result;
  while (c && digit(*c)) {
    result += *c;
    c = peek_next(stream);
  }

  return result;
}

static double
string_to_double(std::string const& s) {
  // XXX: This would ideally use std::from_chars. But, as of 2020, GCC does not implement that, violating C++17.
  // Alternatives include std::stod and std::atof, but those parse the string according to the currently installed
  // global locale. Many things could be said about std::istringstream, but at least it can be imbued with a locale
  // without messing with the global locale. Thus, that's what we're going with.

  std::istringstream is{s};
  is.imbue(std::locale{"C"});

  double result;
  is >> result;

  if (!is || is.peek() != std::istringstream::traits_type::eof())
    throw std::runtime_error{fmt::format("Invalid floating point literal: {}", s)};

  return result;
}

generic_ptr
read_number(context& ctx, ptr<port> const& stream) {
  std::optional<char> c = stream->peek_char();
  bool negative = false;
  assert(c);
  if (c == '-' || c == '+') {
    negative = c == '-';
    stream->read_char();
  }

  std::string literal = read_digits(stream);
  c = stream->peek_char();
  if (c == '/') {
    stream->read_char();

    generic_ptr num = read_integer(ctx, literal);
    if (negative)
      num = flip_sign(num);

    return normalize_fraction(ctx, make<fraction>(ctx,
                                                  num,
                                                  read_integer(ctx, read_digits(stream))));
  }
  else if (c == '.' || c == 'e' || c == 'E') {
    literal += *c;
    stream->read_char();
    literal += read_digits(stream);

    c = stream->peek_char();
    if (c == 'e' || c == 'E') {
      literal += *c;
      stream->read_char();
      literal += read_digits(stream);
    }

    double value = string_to_double(literal);
    if (negative)
      value = -value;

    return make<floating_point>(ctx, value);
  }
  else {
    generic_ptr value = read_integer(ctx, literal);
    return negative ? flip_sign(value) : value;
  }
}

template <typename T>
static void
write_small_magnitude(std::string& buffer, T n) {
  while (n > 0) {
    T quot = n / 10;
    T rem = n % 10;
    buffer.push_back('0' + rem);
    n = quot;
  }
}

static void
write_small(ptr<integer> const& value, ptr<port> const& out) {
  if (value->value() == 0) {
    out->write_char('0');
    return;
  }

  if (value->value() < 0)
    out->write_char('-');

  std::string buffer;
  integer::storage_type n = value->value() >= 0 ? value->value() : -value->value();
  write_small_magnitude(buffer, n);

  std::reverse(buffer.begin(), buffer.end());
  out->write_string(buffer);
}

static void
write_big(context& ctx, ptr<big_integer> value, ptr<port> const& out) {
  if (value->zero()) {
    out->write_char('0');
    return;
  }

  if (!value->positive())
    out->write_char('-');

  std::string buffer;
  limb_type remainder;
  while (normal_length(value) > 1) {
    std::tie(value, remainder) = div_rem_by_limb_magnitude(ctx, value, 10);
    buffer.push_back('0' + remainder);
  }

  write_small_magnitude(buffer, value->front());

  std::reverse(buffer.begin(), buffer.end());
  out->write_string(buffer);
}

static void
write_fraction(context& ctx, ptr<fraction> const& value, ptr<port> const& out) {
  write_number(ctx, fraction_numerator(value), out);
  out->write_char('/');
  write_number(ctx, fraction_denominator(value), out);
}

static void
write_float(ptr<floating_point> const& value, ptr<port> const& out) {
  // Same as with string_to_double: std::to_chars would be the ideal way to implement this, but we'll go with an
  // std::ostringstream in its absence.

  if (value->value == floating_point::positive_infinity) {
    out->write_string("+inf.0");
    return;
  } else if (value->value == floating_point::negative_infinity) {
    out->write_string("-inf.0");
    return;
  } else if (std::isnan(value->value)) {
    if (std::signbit(value->value))
      out->write_char('-');
    else
      out->write_char('+');

    out->write_string("nan.0");
    return;
  }

  std::ostringstream os;
  os.imbue(std::locale("C"));

  os << std::showpoint
     << std::setprecision(std::numeric_limits<floating_point::value_type>::max_digits10 - 1)
     << value->value;

  std::string result = os.str();
  std::string::size_type dot = result.find('.');
  std::string::size_type last_nonzero = result.find_last_not_of('0');
  std::string::size_type end = std::max(dot + 1, last_nonzero);
  out->write_string(result.substr(0, end + 1));
}

void
write_number(context& ctx, generic_ptr const& value, ptr<port> const& out) {
  if (auto s = match<integer>(value))
    write_small(s, out);
  else if (auto b = match<big_integer>(value))
    write_big(ctx, b, out);
  else if (auto q = match<fraction>(value))
    write_fraction(ctx, q, out);
  else if (auto f = match<floating_point>(value))
    write_float(f, out);
  else
    assert(false);
}

void
export_numeric(context& ctx, module& result) {
  export_native(ctx, result, "+", add, special_top_level_tag::plus);
  export_native(ctx, result, "-", subtract, special_top_level_tag::minus);
  export_native(ctx, result, "*", multiply, special_top_level_tag::times);
  export_native(ctx, result, "/", truncate_quotient, special_top_level_tag::divide);
  export_native(ctx, result, "=", arith_equal, special_top_level_tag::arith_equal);
  export_native(ctx, result, "<", less, special_top_level_tag::less_than);
  export_native(ctx, result, ">", greater, special_top_level_tag::greater_than);
  define_lambda<gcd>(ctx, result, "gcd", true);
}

} // namespace insider
