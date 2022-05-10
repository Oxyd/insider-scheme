#include "bytecode.hpp"

#include <fmt/format.h>

#include <algorithm>
#include <cassert>
#include <cstddef>

namespace insider {

instruction::instruction(std::string_view mnemonic,
                         std::vector<operand> operands)
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
encode(bytecode& bc, opcode oc) {
  bc.push_back(static_cast<std::uint16_t>(oc));
}

static void
encode(bytecode& bc, operand op) {
  bc.push_back(op);
}

void
encode_instruction(bytecode& bc, instruction const& instr) {
  instruction_info info = opcode_to_info(instr.opcode);
  assert(info.extra_operands || instr.operands.size() == info.num_operands);
  assert(!info.extra_operands || instr.operands.size() >= info.num_operands);

  encode(bc, instr.opcode);
  for (std::size_t i = 0; i < info.num_operands; ++i)
    encode(bc, instr.operands[i]);

  if (info.extra_operands) {
    encode(
      bc,
      static_cast<insider::operand>(instr.operands.size() - info.num_operands)
    );
    for (std::size_t i = info.num_operands; i < instr.operands.size(); ++i)
      encode(bc, instr.operands[i]);
  }
}

instruction
read_instruction(bytecode const& bc, integer::value_type& pc) {
  instruction result{read_opcode(bc, pc)};
  instruction_info info = opcode_to_info(result.opcode);

  for (std::size_t i = 0; i < info.num_operands; ++i)
    result.operands.push_back(read_operand(bc, pc));

  if (info.extra_operands) {
    std::size_t num_extra = read_operand(bc, pc);

    for (std::size_t i = 0; i < num_extra; ++i)
      result.operands.push_back(read_operand(bc, pc));
  }

  return result;
}

} // namespace insider
