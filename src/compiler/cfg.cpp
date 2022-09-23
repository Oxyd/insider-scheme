#include "compiler/cfg.hpp"

#include <cassert>

namespace insider {

static bool
is_tail_call(opcode oc) {
  return oc == opcode::tail_call
         || oc == opcode::tail_call_top_level
         || oc == opcode::tail_call_static;
}

static void
prune_dead_code(basic_block& block) {
  for (auto instr = block.body.begin(); instr != block.body.end(); ++instr)
    if (is_tail_call(instr->opcode)) {
      block.body.erase(instr + 1, block.body.end());
      return;
    }
}

static void
prune_dead_code(cfg& g) {
  for (basic_block& block : g)
    prune_dead_code(block);
}

static void
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

  if (!g.empty())
    g.front().incoming_blocks.emplace(entry_block_idx);
}

static bool
is_collapsible(basic_block const& b) {
  return b.body.empty() && std::holds_alternative<unconditional_jump>(b.ending);
}

static std::size_t
collapse_jump_target(cfg const& g, std::size_t target) {
  while (is_collapsible(g[target]))
    target = std::get<unconditional_jump>(g[target].ending).target_block;
  return target;
}

static std::size_t
ending_target(basic_block::ending_type e) {
  if (auto* uj = std::get_if<unconditional_jump>(&e))
    return uj->target_block;
  else if (auto* cj = std::get_if<conditional_jump>(&e))
    return cj->target_block;

  assert(false);
  return 0;
}

static void
change_target(basic_block::ending_type& e, std::size_t new_target) {
  if (auto* uj = std::get_if<unconditional_jump>(&e))
    uj->target_block = new_target;
  else if (auto* cj = std::get_if<conditional_jump>(&e))
    cj->target_block = new_target;
  else
    assert(false);
}

static bool
is_return_block(basic_block const& block) {
  return block.body.size() == 1 && block.body.front().opcode == opcode::ret;
}

static void
collapse_return(basic_block& block, basic_block const& return_block) {
  assert(return_block.body.size() == 1);

  block.body.push_back(return_block.body.front());
  block.ending = flow_off{};
}

static void
collapse_block(cfg& g, basic_block& block) {
  std::size_t original_target = ending_target(block.ending);
  std::size_t new_target = collapse_jump_target(g, original_target);

  if (is_return_block(g[new_target])
      && std::holds_alternative<unconditional_jump>(block.ending))
    collapse_return(block, g[new_target]);
  else
    change_target(block.ending, new_target);
}

static void
collapse_jumps(cfg& g) {
  for (basic_block& block : g)
    if (!std::holds_alternative<flow_off>(block.ending))
      collapse_block(g, block);
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
    return instruction_size({opcode::jump, operand{}});
  else if (std::holds_alternative<conditional_jump>(e))
    return instruction_size(
      {opcode::jump_unless, operand{}, operand{}}
    );
  else
    return 0;
}

static void
find_block_lengths_and_offsets(cfg& g) {
  std::size_t offset = 0;
  for (basic_block& block : g)
    if (!block.incoming_blocks.empty()) {
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
  encode_instruction(bc, {opcode::jump, target});
}

static void
encode_ending(bytecode& bc, conditional_jump cj, cfg const& g) {
  operand target = g[cj.target_block].start_offset;
  encode_instruction(bc,
                     {opcode::jump_unless, cj.test_register, target});
}

static void
encode_ending(bytecode& bc, basic_block::ending_type e, cfg const& g) {
  std::visit([&] (auto end) { encode_ending(bc, end, g); }, e);
}

static void
encode_body(bytecode_and_debug_info& bc_di, basic_block const& block) {
  for (std::size_t k = 0; k < block.body.size(); ++k) {
    instruction const& i = block.body[k];
    std::size_t instruction_offset = encode_instruction(bc_di.bc, i);

    if (auto di = block.debug_info.find(k); di != block.debug_info.end())
      bc_di.debug_info[instruction_offset] = di->second;
  }
}

static void
encode_block(bytecode_and_debug_info& bc_di, basic_block const& block,
             cfg const& g) {
  encode_body(bc_di, block);
  encode_ending(bc_di.bc, block.ending, g);
}

bytecode_and_debug_info
analyse_and_compile_cfg(cfg& g) {
  prune_dead_code(g);
  collapse_jumps(g);
  find_incoming_blocks(g);
  find_block_lengths_and_offsets(g);

  bytecode_and_debug_info result;
  for (basic_block const& b : g)
    if (!b.incoming_blocks.empty())
      encode_block(result, b, g);
  return result;
}

} // namespace insider
