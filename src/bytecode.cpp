#include "bytecode.hpp"

#include <fmt/format.h>

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
encode_opcode(bytecode& bc, opcode op) {
  bc.push_back(static_cast<std::uint8_t>(op));
}

static void
encode_operand(bytecode& bc, operand op) {
  if (op < 0xFF)
    bc.push_back(static_cast<std::uint8_t>(op));
  else {
    bc.push_back(std::uint8_t{0xFF});

    bc.resize(bc.size() + sizeof(operand));
    *reinterpret_cast<operand*>(&bc.back() - sizeof(operand)) = op;
  }
}

void
encode_instruction(bytecode& bc, instruction const& instr) {
  instruction_info info = opcode_to_info(instr.opcode);
  assert(info.extra_operands || instr.operands.size() == info.num_operands);
  assert(!info.extra_operands || instr.operands.size() >= info.num_operands);

  encode_opcode(bc, instr.opcode);

  for (std::size_t i = 0; i < info.num_operands; ++i)
    encode_operand(bc, instr.operands[i]);

  if (info.extra_operands) {
    operand num_extra = instr.operands.size() - info.num_operands;
    encode_operand(bc, num_extra);

    for (std::size_t i = 0; i < num_extra; ++i)
      encode_operand(bc, instr.operands[i + info.num_operands]);
  }
}

instruction
read_instruction(bytecode const& bc, integer::value_type& pc) {
  instruction result{read_opcode(bc, pc)};
  instruction_info info = opcode_to_info(result.opcode);

  for (std::size_t i = 0; i < info.num_operands; ++i)
    result.operands.push_back(read_operand(bc, pc));

  if (info.extra_operands) {
    operand num_extra = read_operand(bc, pc);
    for (std::size_t i = 0; i < num_extra; ++i)
      result.operands.push_back(read_operand(bc, pc));
  }

  return result;
}

} // namespace insider
