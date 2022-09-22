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

static std::size_t
instructions_size(std::vector<instruction> const& instrs) {
  std::size_t result = 0;
  for (instruction const& i : instrs)
    result += instruction_size(i);
  return result;
}

static std::size_t
ending_length(basic_block::ending_type e) {
  if (std::holds_alternative<unconditional_jump>(e))
    return instruction_size(instruction{opcode::jump_absolute, operand{}});
  else if (std::holds_alternative<conditional_jump>(e))
    return instruction_size(
      instruction{opcode::jump_absolute_unless, operand{}, operand{}}
    );
  else
    return 0;
}

static void
find_block_lengths_and_offsets(cfg& g) {
  std::size_t offset = 0;
  for (basic_block& block : g) {
    block.start_offset = offset;
    block.bytecode_length = instructions_size(block.body)
                            + ending_length(block.ending);
    offset += block.bytecode_length;
  }
}

static void
encode_ending(bytecode&, flow_off, cfg const&) { }

static void
encode_ending(bytecode& bc, unconditional_jump uj, cfg const& g) {
  operand target = g[uj.target_block].start_offset;
  encode_instruction(bc, instruction{opcode::jump_absolute, target});
}

static void
encode_ending(bytecode& bc, conditional_jump cj, cfg const& g) {
  operand target = g[cj.target_block].start_offset;
  encode_instruction(bc, instruction{opcode::jump_absolute_unless,
                                     cj.test_register, target});
}

static void
encode_ending(bytecode& bc, basic_block::ending_type e, cfg const& g) {
  std::visit([&] (auto end) { encode_ending(bc, end, g); }, e);
}

static void
encode_block(bytecode& bc, basic_block const& block, cfg const& g) {
  for (instruction const& i : block.body)
    encode_instruction(bc, i);
  encode_ending(bc, block.ending, g);
}

bytecode
cfg_to_bytecode(cfg& g) {
  find_block_lengths_and_offsets(g);

  bytecode result;
  for (basic_block const& b : g)
    encode_block(result, b, g);
  return result;
}

} // namespace insider
