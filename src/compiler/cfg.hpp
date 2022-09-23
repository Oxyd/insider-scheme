#ifndef INSIDER_COMPILER_CFG_HPP
#define INSIDER_COMPILER_CFG_HPP

#include "compiler/debug_info.hpp"
#include "vm/bytecode.hpp"

#include <limits>
#include <unordered_set>
#include <variant>

namespace insider {

struct flow_off { };

struct unconditional_jump {
  std::size_t target_block;
};

struct conditional_jump {
  operand     test_register;
  std::size_t target_block;
};

constexpr std::size_t entry_block_idx = std::numeric_limits<std::size_t>::max();

struct basic_block {
  using ending_type
    = std::variant<flow_off, unconditional_jump, conditional_jump>;

  std::vector<instruction>        body;
  ending_type                     ending = flow_off{};
  debug_info_map                  debug_info;
  std::unordered_set<std::size_t> incoming_blocks;
  std::size_t                     start_offset = 0;
  std::size_t                     bytecode_length = 0;
};

using cfg = std::vector<basic_block>;

struct bytecode_and_debug_info {
  bytecode       bc;
  debug_info_map debug_info;
};

bytecode_and_debug_info
analyse_and_compile_cfg(cfg&);

} // namespace insider

#endif
