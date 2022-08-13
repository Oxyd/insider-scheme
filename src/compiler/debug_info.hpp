#ifndef INSIDER_COMPILER_DEBUG_INFO_HPP
#define INSIDER_COMPILER_DEBUG_INFO_HPP

#include <string>
#include <unordered_map>
#include <vector>

namespace insider {

struct debug_info {
  std::vector<std::string> inlined_call_chain;
};

using debug_info_map = std::unordered_map<std::size_t, debug_info>;

} // namespace insider

#endif
