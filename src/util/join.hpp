#ifndef INSIDER_UTIL_JOIN_HPP
#define INSIDER_UTIL_JOIN_HPP

#include <string>
#include <string_view>

namespace insider {

std::string
join(auto&& range, std::string_view sep) {
  auto it = std::begin(range);
  auto end = std::end(range);

  std::string result;
  if (it != end)
    result += *it++;

  for (; it != end; ++it) {
    result += sep;
    result += *it;
  }

  return result;
}

} // namespace insider

#endif
