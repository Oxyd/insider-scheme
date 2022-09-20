#include "compiler/cfg.hpp"

namespace insider {

void
find_incoming_blocks(cfg& g) {
  for (std::size_t i = 0; i < g.size(); ++i)
    if (std::get_if<flow_off>(&g[i].ending)) {
      if (i + 1 < g.size())
        g[i + 1].incoming_blocks.emplace(i);
    } else if (auto* uj = std::get_if<unconditional_jump>(&g[i].ending))
      g[uj->target_block].incoming_blocks.emplace(i);
    else if (auto* cj = std::get_if<conditional_jump>(&g[i].ending)) {
      g[cj->target_block].incoming_blocks.emplace(i);
      if (i + 1 < g.size())
        g[i + 1].incoming_blocks.emplace(i);
    }
}

} // namespace insider
