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
  #define INSIDER_SCHEME_LIMB_WIDTH 64
  using limb_type = std::uint64_t;
  using double_limb_type = __uint128_t;
#else
  #define INSIDER_SCHEME_LIMB_WIDTH 32
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

bool
is_integer(ptr<>);

bool
is_number(ptr<>);

bool
is_exact(ptr<>);

bool
is_inexact(ptr<>);

bool
is_nan(ptr<>);

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
arithmetic_shift(context&, ptr<>, ptr<>);

ptr<>
bitwise_and(context&, ptr<>, ptr<>);

ptr<>
bitwise_or(context&, ptr<>, ptr<>);

ptr<>
bitwise_not(context&, ptr<>);

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

ptr<>
read_integer(context& ctx, std::string const& digits, unsigned base = 10);

ptr<>
read_number(context&, reader_stream&);

void
write_number(context&, ptr<> value, ptr<textual_output_port> out);

} // namespace insider

#endif
