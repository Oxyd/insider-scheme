#include "bytecode.hpp"

#include <fmt/format.h>

#include <algorithm>
#include <cassert>
#include <cstddef>

namespace insider {

instruction::instruction(std::string_view mnemonic, std::vector<operand> operands)
  : opcode{mnemonic_to_info(mnemonic).opcode}
  , operands{std::move(operands)}
{ }

namespace {
  template <std::size_t N, std::size_t... Is>
  std::unordered_map<std::string_view, instruction_info>
  make_mnemonics(std::array<detail::info_tuple, N> const& instructions,
                 std::index_sequence<Is...>) {
    return {
      {std::get<0>(instructions[Is]),
       instruction_info{std::get<0>(instructions[Is]),
                        std::get<1>(instructions[Is]),
                        std::get<2>(instructions[Is]),
                        std::get<3>(instructions[Is])}}...
    };
  }

  template <std::size_t N>
  std::unordered_map<std::string_view, instruction_info>
  make_mnemonics(std::array<detail::info_tuple, N> const& instructions) {
    return make_mnemonics(instructions, std::make_index_sequence<N>{});
  }
}

static std::unordered_map<std::string_view, instruction_info> const
mnemonic_map = make_mnemonics(instructions);

instruction_info
mnemonic_to_info(std::string_view mnemonic) {
  auto it = mnemonic_map.find(mnemonic);
  assert(it != mnemonic_map.end());
  return it->second;
}

static void
encode_basic_instruction(bytecode& bc, basic_instruction bi) {
  bc.push_back(instruction_packet{bi});
}

static void
encode_extra_operands(bytecode& bc, extra_operands eo) {
  bc.push_back(instruction_packet{eo});
}

static basic_instruction
make_basic_instruction(instruction const& i) {
  switch (i.operands.size()) {
  case 0: return basic_instruction{i.opcode, {}};
  case 1: return basic_instruction{i.opcode, {i.operands[0]}};
  case 2: return basic_instruction{i.opcode, {i.operands[0], i.operands[1]}};
  default: return basic_instruction{i.opcode, {i.operands[0], i.operands[1], i.operands[2]}};
  }
}

static extra_operands
make_extra_operands(instruction const& i, std::size_t first_operand) {
  switch (i.operands.size() - first_operand) {
  case 0: assert(false); return {};
  case 1: return {i.operands[first_operand]};
  case 2: return {i.operands[first_operand], i.operands[first_operand + 1]};
  case 3: return {i.operands[first_operand], i.operands[first_operand + 1], i.operands[first_operand + 2]};
  default: return {i.operands[first_operand],
                   i.operands[first_operand + 1],
                   i.operands[first_operand + 2],
                   i.operands[first_operand + 3]};
  }
}

void
encode_instruction(bytecode& bc, instruction const& instr) {
  instruction_info info = opcode_to_info(instr.opcode);
  assert(info.extra_operands || instr.operands.size() == info.num_operands);
  assert(!info.extra_operands || instr.operands.size() >= info.num_operands);

  auto bi = make_basic_instruction(instr);
  if (info.extra_operands) {
    assert(info.num_operands <= 2);
    bi.operands[info.num_operands] = instr.operands.size() - info.num_operands;
  }

  encode_basic_instruction(bc, bi);

  if (info.extra_operands)
    for (std::size_t e = info.num_operands; e < instr.operands.size(); e += 4)
      encode_extra_operands(bc, make_extra_operands(instr, e));
}

instruction
read_instruction(bytecode const& bc, integer::value_type& pc) {
  basic_instruction bi = read_basic_instruction(bc, pc);
  instruction_info info = opcode_to_info(bi.opcode);

  instruction result{bi.opcode};
  result.operands.resize(info.num_operands);

  for (std::size_t i = 0; i < info.num_operands; ++i)
    result.operands[i] = bi.operands[i];

  if (info.extra_operands) {
    std::size_t num_extra = bi.operands[info.num_operands];
    for_each_extra_operand(bc, pc, num_extra,
                           [&] (operand op) { result.operands.push_back(op); });
  }

  return result;
}

} // namespace insider
