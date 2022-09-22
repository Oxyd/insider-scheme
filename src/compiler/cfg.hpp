#ifndef INSIDER_COMPILER_CFG_HPP
#define INSIDER_COMPILER_CFG_HPP

#include "vm/bytecode.hpp"

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

struct basic_block {
  using ending_type
    = std::variant<flow_off, unconditional_jump, conditional_jump>;

  std::vector<instruction>        body;
  ending_type                     ending = flow_off{};
  std::unordered_set<std::size_t> incoming_blocks;
  std::size_t                     start_offset = 0;
  std::size_t                     bytecode_length = 0;
};

using cfg = std::vector<basic_block>;

void
find_incoming_blocks(cfg&);

bytecode
cfg_to_bytecode(cfg&);

} // namespace insider

#endif
