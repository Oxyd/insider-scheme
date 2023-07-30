#ifndef INSIDER_UTIL_JOIN_HPP
#define INSIDER_UTIL_JOIN_HPP

#include <string>
#include <string_view>

namespace insider {

namespace detail {

  std::string
  make_string(auto x) { return std::to_string(x); }

  inline std::string const&
  make_string(std::string const& s) { return s; }

  inline std::string
  make_string(char const* s) { return std::string(s); }

} // namespace detail

std::string
join(auto&& range, std::string_view sep) {
  auto it = std::begin(range);
  auto end = std::end(range);

  std::string result;
  if (it != end)
    result += detail::make_string(*it++);

  for (; it != end; ++it) {
    result += sep;
    result += detail::make_string(*it);
  }

  return result;
}

} // namespace insider

#endif
