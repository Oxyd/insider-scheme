#include "numeric.hpp"

#include "converters.hpp"
#include "io.hpp"
#include "scheme.hpp"

#include <algorithm>
#include <charconv>
#include <cstdlib>

namespace scm {

using limb_type = detail::limb_type;
using double_limb_type = detail::double_limb_type;

static constexpr limb_type max_limb_value = std::numeric_limits<limb_type>::max();
static constexpr unsigned  limb_width = std::numeric_limits<limb_type>::digits;
static constexpr double_limb_type limb_mask = (double_limb_type{1} << limb_width) - 1;

std::size_t
big_integer::extra_storage_size(std::size_t length) {
  return sizeof(limb_type) * length;
}

std::size_t
big_integer::extra_storage_size(std::vector<limb_type> const& limbs, bool) {
  return sizeof(limb_type) * limbs.size();
}

static std::size_t
number_of_limbs_for_small_integer(integer::value_type i) {
  if (i == 0)
    return 0;
  else if (i <= max_limb_value && -i <= max_limb_value)
    return 1;
  else
    return 2;
}

std::size_t
big_integer::extra_storage_size(ptr<integer> const& i) {
  return sizeof(limb_type) * number_of_limbs_for_small_integer(i->value());
}

std::size_t
big_integer::extra_storage_size(ptr<big_integer> const& i) {
  return sizeof(limb_type) * i->length();
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
  assert(end() - begin() == limbs.size());
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

auto
big_integer::begin() -> iterator {
  return dynamic_storage();
}

auto
big_integer::end() -> iterator{
  return dynamic_storage() + length_;
}

auto
big_integer::rbegin() -> reverse_iterator {
  return reverse_iterator{end()};
}

auto
big_integer::rend() -> reverse_iterator {
  return reverse_iterator{begin()};
}

static bool
digit(char c) {
  return c >= '0' && c <= '9';
}

static unsigned
digit_value(char c) {
  assert(digit(c));
  return c - '0';
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

  if (new_length == i->length())
    return i;

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

  auto result = make<big_integer>(ctx, new_length, big_integer::dont_initialize);
  std::copy(i->begin(), i->begin() + new_length, result->begin());
  result->set_positive(i->positive());
  return result;
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

static generic_ptr
mul_magnitude_by_limb_destructive(context& ctx, generic_ptr lhs, limb_type rhs) {
  if (auto b = match<big_integer>(lhs))
    return mul_big_magnitude_by_limb_destructive(ctx, b, b, rhs);

  auto lhs_int = assume<integer>(lhs);
  assert(lhs_int->value() >= 0);
  double_limb_type product = static_cast<double_limb_type>(lhs_int->value()) * double_limb_type{rhs};
  if (product <= integer::max) {
    lhs_int->set_value(static_cast<integer::value_type>(product));
    return lhs_int;
  }

  return mul_big_magnitude_by_limb_destructive(ctx, {}, make<big_integer>(ctx, lhs_int), rhs);
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

  limb_type* y = rhs->data();
  auto result = mul_big_magnitude_by_limb(ctx, lhs, rhs->front());
  for (std::size_t i = 1; i < rhs->length(); ++i) {
    auto term = mul_big_magnitude_by_limb(ctx, lhs, y[i]);
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

  double_limb_type product = double_limb_type{x} * double_limb_type{y};
  if ((result_positive && product > integer::max)
      || (!result_positive && product > -integer::min))
    return mul_big(ctx, make<big_integer>(ctx, lhs), make<big_integer>(ctx, rhs));
  else
    return make<integer>(ctx, result_positive ? integer::value_type(product) : -integer::value_type(product));
}

static ptr<big_integer>
bitshift_left(context& ctx, ptr<big_integer> i, unsigned k) {
  i = make<big_integer>(ctx, i);
  if (k == 0)
    return i;

  assert(k < limb_width);

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

  return i;
}

static ptr<big_integer>
bitshift_right(context& ctx, ptr<big_integer> i, unsigned k) {
  i = make<big_integer>(ctx, i);
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
    small,
    big
  };
}

static common_type
find_common_type(generic_ptr const& lhs, generic_ptr const& rhs) {
  if (is<integer>(lhs) && is<integer>(rhs))
    return common_type::small;
  else
    return common_type::big;
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

template <auto Small, auto Big>
generic_ptr
arithmetic_two(context& ctx, generic_ptr const& lhs, generic_ptr const& rhs) {
  switch (find_common_type(lhs, rhs)) {
  case common_type::small:
    return Small(ctx, assume<integer>(lhs), assume<integer>(rhs));
  case common_type::big:
    return normalize(ctx, Big(ctx, make_big(ctx, lhs), make_big(ctx, rhs)));
  }

  assert(false);
  return {};
}

generic_ptr
add(context& ctx, generic_ptr const& lhs, generic_ptr const& rhs) {
  return arithmetic_two<add_small, add_big>(ctx, lhs, rhs);
}

generic_ptr
add(context& ctx, std::vector<generic_ptr> const& xs) {
  return arithmetic<static_cast<primitive_arithmetic_type*>(&add)>(ctx, xs, true, 0);
}

generic_ptr
subtract(context& ctx, generic_ptr const& lhs, generic_ptr const& rhs) {
  return arithmetic_two<sub_small, sub_big>(ctx, lhs, rhs);
}

generic_ptr
subtract(context& ctx, std::vector<generic_ptr> const& xs) {
  return arithmetic<static_cast<primitive_arithmetic_type*>(&subtract)>(ctx, xs, false, 0);
}

generic_ptr
multiply(context& ctx, generic_ptr const& lhs, generic_ptr const& rhs) {
  return arithmetic_two<mul_small, mul_big>(ctx, lhs, rhs);
}

generic_ptr
multiply(context& ctx, std::vector<generic_ptr> const& xs) {
  return arithmetic<static_cast<primitive_arithmetic_type*>(&multiply)>(ctx, xs, true, 1);
}

generic_ptr
divide(context& ctx, generic_ptr const& lhs, generic_ptr const& rhs) {
  if (expect<integer>(rhs)->value() == 0)
    throw std::runtime_error{"Divide by zero"};
  else
    return make<integer>(ctx, expect<integer>(lhs)->value() / expect<integer>(rhs)->value());
}

generic_ptr
divide(context& ctx, std::vector<generic_ptr> const& xs) {
  return arithmetic<static_cast<primitive_arithmetic_type*>(&divide)>(ctx, xs, false, 1);
}

std::tuple<generic_ptr, generic_ptr>
quotient_remainder(context& ctx, generic_ptr const& lhs, generic_ptr const& rhs) {
  switch (find_common_type(lhs, rhs)) {
  case common_type::small:
    return div_rem_small(ctx, assume<integer>(lhs), assume<integer>(rhs));
  case common_type::big:
    return div_rem_big(ctx, make_big(ctx, lhs), make_big(ctx, rhs));
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
  case common_type::small:
    return assume<integer>(lhs)->value() == assume<integer>(rhs)->value() ? ctx.constants->t : ctx.constants->f;
  case common_type::big:
    return compare_big(make_big(ctx, lhs), make_big(ctx, rhs)) == compare::equal
           ? ctx.constants->t : ctx.constants->f;
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

static void
export_native(context& ctx, module& m, std::string const& name,
              generic_ptr (*f)(context&, std::vector<generic_ptr> const&), special_top_level_tag tag) {
  auto index = ctx.add_top_level(ctx.store.make<native_procedure>(f));
  ctx.tag_top_level(index, tag);
  m.add(name, index);
  m.export_(name);
}

generic_ptr
read_number(context& ctx, ptr<port> const& stream, bool negative) {
  std::optional<char> c = stream->peek_char();

  assert(c);
  if (c == '-' || c == '+') {
    negative = c == '-';
    stream->read_char();
    c = stream->peek_char();
  }

  generic_ptr result = make<integer>(ctx, 0);

  while (c && digit(*c)) {
    result = mul_magnitude_by_limb_destructive(ctx, result, 10);
    result = add_magnitude_to_limb_destructive(ctx, result, digit_value(*c));

    stream->read_char();
    c = stream->peek_char();
  }

  if (negative)
    result = flip_sign(result);

  if (auto b = match<big_integer>(result))
    result = normalize(ctx, b);

  return result;
}

void
write_number(ptr<integer> const& value, ptr<port> const& out) {
  std::array<char, std::numeric_limits<integer::value_type>::digits10 + 1> buffer;
  std::to_chars_result res = std::to_chars(buffer.data(), buffer.data() + buffer.size(),
                                           value->value());

  assert(res.ec == std::errc{});
  out->write_string(std::string(buffer.data(), res.ptr));
}

void
export_numeric(context& ctx, module& result) {
  export_native(ctx, result, "+", add, special_top_level_tag::plus);
  export_native(ctx, result, "-", subtract, special_top_level_tag::minus);
  export_native(ctx, result, "*", multiply, special_top_level_tag::times);
  export_native(ctx, result, "/", divide, special_top_level_tag::divide);
  export_native(ctx, result, "=", arith_equal, special_top_level_tag::arith_equal);
  export_native(ctx, result, "<", less, special_top_level_tag::less_than);
  export_native(ctx, result, ">", greater, special_top_level_tag::greater_than);
}

} // namespace scm
