#include "code_point_properties.hpp"

#include <array>
#include <cstdint>

namespace insider {

namespace {
  struct hash_function {
    std::uint64_t a;
    std::uint64_t b;
    std::uint64_t n;

    std::uint64_t
    operator () (char32_t x) const;
  };
}

#include "code_point_properties_table.inc"

std::uint64_t
hash_function::operator () (char32_t x) const {
  return (((a * x) + b) % p) % n;
}

std::optional<code_point_properties>
find_properties(char32_t c) {
  code_point_properties p = code_points[codepoint_hash(c)];
  if (p.code_point == c)
    return p;
  else
    return std::nullopt;
}

std::size_t
codepoint_hash(char32_t x) {
  return (g[f1(x)] + g[f2(x)]) % (code_points.size() + 1);
}

} // namespace insider
