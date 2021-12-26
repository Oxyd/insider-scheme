#ifndef INSIDER_TEST_BIGNUM_HPP
#define INSIDER_TEST_BIGNUM_HPP

#include "context.hpp"
#include "numeric.hpp"

using limb_type = insider::big_integer::limb_type;
using limb_vector = std::vector<limb_type>;
constexpr std::uintmax_t limb_max = std::numeric_limits<limb_type>::max();

inline void
convert_limbs(limb_vector&) { }

template <typename Limb, typename... Limbs>
void
convert_limbs(limb_vector& limbs, Limb first, Limbs... rest) {
  if constexpr (sizeof(Limb) <= sizeof(limb_type)) {
    limbs.push_back(first);
    convert_limbs(limbs, rest...);
  } else if constexpr (sizeof(Limb) == 2 * sizeof(limb_type)) {
    Limb mask = (Limb{1} << std::numeric_limits<limb_type>::digits) - 1;
    limbs.push_back(static_cast<limb_type>(first & mask));
    limb_type hi = first >> std::numeric_limits<limb_type>::digits;
    if (hi > 0 || sizeof...(rest) > 0)
      limbs.push_back(hi);
    convert_limbs(limbs, rest...);
  } else
    static_assert(sizeof(Limb) == 0, "Unimplemented");
}

template <typename... Limbs>
insider::ptr<insider::big_integer>
make_big(insider::context& ctx, Limbs... limbs) {
  limb_vector ls;
  convert_limbs(ls, static_cast<std::uint64_t>(limbs)...);
  return insider::make<insider::big_integer>(ctx, ls, true);
}

inline insider::ptr<insider::big_integer>
make_big_literal(insider::context& ctx, limb_vector limbs) {
  return insider::make<insider::big_integer>(ctx, std::move(limbs), true);
}

template <typename... Limbs>
insider::ptr<insider::big_integer>
make_big_negative(insider::context& ctx, Limbs... limbs) {
  limb_vector ls;
  convert_limbs(ls, static_cast<std::uint64_t>(limbs)...);
  return insider::make<insider::big_integer>(ctx, ls, false);
}

inline insider::ptr<insider::big_integer>
make_big_negative_literal(insider::context& ctx, limb_vector limbs) {
  return insider::make<insider::big_integer>(ctx, std::move(limbs), false);
}

#endif
