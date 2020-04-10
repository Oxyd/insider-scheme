#include "numeric.hpp"

#include "converters.hpp"
#include "io.hpp"
#include "scheme.hpp"

#include <charconv>

namespace scm {

static constexpr std::size_t limb_storage_width = detail::limb_storage_width;
static constexpr std::size_t limb_value_width = detail::limb_value_width;

using limb_type = detail::limb_type;

static constexpr limb_type max_signed_limb_value =
  (limb_type{1} << (limb_value_width - 1)) - 1;

std::size_t
big_integer::extra_storage_size(std::size_t length) {
  return sizeof(detail::limb_type) * length;
}

big_integer::big_integer(std::size_t length)
  : length_{length}
{ }

limb_type*
big_integer::begin() {
  return dynamic_storage();
}

limb_type*
big_integer::end() {
  return dynamic_storage() + length_;
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

generic_ptr
read_number(context& ctx, ptr<port> const& stream, bool negative) {
  integer::storage_type result = 0;
  std::optional<char> c = stream->peek_char();

  assert(c);
  if (c == '-' || c == '+') {
    negative = c == '-';
    stream->read_char();
    c = stream->peek_char();
  }

  constexpr integer::storage_type overflow_divisor = max_signed_limb_value / 10;
  constexpr integer::storage_type overflow_remainder = max_signed_limb_value % 10;

  while (c && digit(*c)) {
    if (result > overflow_divisor
        || (result == overflow_divisor && digit_value(*c) > overflow_remainder))
      throw parse_error{"Integer literal overflow"};

    result *= 10;
    result += digit_value(*c);

    stream->read_char();
    c = stream->peek_char();
  }

  if (result > max_signed_limb_value + (negative ? 1 : 0))
    throw parse_error{"Integer literal overflow"};

  if (negative)
    result = ~result + 1;

  return make<integer>(ctx, result);
}

void
write_number(ptr<integer> const& value, ptr<port> const& out) {
  std::array<char, std::numeric_limits<integer::value_type>::digits10 + 1> buffer;
  std::to_chars_result res = std::to_chars(buffer.data(), buffer.data() + buffer.size(),
                                           value->value());

  assert(res.ec == std::errc{});
  out->write_string(std::string(buffer.data(), res.ptr));
}

ptr<integer>
add(context& ctx, ptr<integer> const& lhs, ptr<integer> const& rhs) {
  return make<integer>(ctx, lhs->value() + rhs->value());
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
    ptr<integer> result = expect<integer>(xs.front());
    for (auto rhs = xs.begin() + 1; rhs != xs.end(); ++rhs)
      result = F(ctx, result, expect<integer>(*rhs));

    return result;
  }
}

using primitive_arithmetic_type = ptr<integer>(context& ctx, ptr<integer> const&, ptr<integer> const&);

generic_ptr
add(context& ctx, std::vector<generic_ptr> const& xs) {
  return arithmetic<static_cast<primitive_arithmetic_type*>(&add)>(ctx, xs, true, 0);
}

ptr<integer>
subtract(context& ctx, ptr<integer> const& lhs, ptr<integer> const& rhs) {
  return make<integer>(ctx, lhs->value() - rhs->value());
}

generic_ptr
subtract(context& ctx, std::vector<generic_ptr> const& xs) {
  return arithmetic<static_cast<primitive_arithmetic_type*>(&subtract)>(ctx, xs, false, 0);
}

ptr<integer>
multiply(context& ctx, ptr<integer> const& lhs, ptr<integer> const& rhs) {
  return make<integer>(ctx, lhs->value() * rhs->value());
}

generic_ptr
multiply(context& ctx, std::vector<generic_ptr> const& xs) {
  return arithmetic<static_cast<primitive_arithmetic_type*>(&multiply)>(ctx, xs, true, 1);
}

ptr<integer>
divide(context& ctx, ptr<integer> const& lhs, ptr<integer> const& rhs) {
  if (rhs->value() == 0)
    throw std::runtime_error{"Divide by zero"};
  else
    return make<integer>(ctx, lhs->value() / rhs->value());
}

generic_ptr
divide(context& ctx, std::vector<generic_ptr> const& xs) {
  return arithmetic<static_cast<primitive_arithmetic_type*>(&divide)>(ctx, xs, false, 1);
}

using primitive_relational_type = ptr<boolean>(context&, ptr<integer> const&, ptr<integer> const&);

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
arith_equal(context& ctx, ptr<integer> const& lhs, ptr<integer> const& rhs) {
  return lhs->value() == rhs->value() ? ctx.constants->t : ctx.constants->f;
}

generic_ptr
arith_equal(context& ctx, std::vector<generic_ptr> const& xs) {
  return relational<arith_equal>(ctx, xs, "=");
}

ptr<boolean>
less(context& ctx, ptr<integer> const& lhs, ptr<integer> const& rhs) {
  return lhs->value() < rhs->value() ? ctx.constants->t : ctx.constants->f;
}

generic_ptr
less(context& ctx, std::vector<generic_ptr> const& xs) {
  return relational<less>(ctx, xs, "<");
}

ptr<boolean>
greater(context& ctx, ptr<integer> const& lhs, ptr<integer> const& rhs) {
  return lhs->value() > rhs->value() ? ctx.constants->t : ctx.constants->f;
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
