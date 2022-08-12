#ifndef INSIDER_COMPILER_DEBUG_INFO_HPP
#define INSIDER_COMPILER_DEBUG_INFO_HPP

#include <string>
#include <vector>

namespace insider {

struct debug_info {
  std::vector<std::string> inlined_call_chain;
};

} // namespace insider

#endif
