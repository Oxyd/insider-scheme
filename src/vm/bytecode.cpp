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
                        std::get<2>(instructions[Is])}}...
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

std::ostream&
operator << (std::ostream& out, instruction const& i) {
  auto info = opcode_to_info(i.opcode);
  if (i.operands.empty())
    return out << "{" << info.mnemonic << "}";
  else
    return out << fmt::format("{{{} {}}}",
                              info.mnemonic,
                              fmt::join(i.operands, ", "));
}

static void
encode(bytecode& bc, opcode oc) {
  bc.push_back(static_cast<std::uint16_t>(oc));
}

static void
encode(bytecode& bc, operand op) {
  bc.push_back(op);
}

std::size_t
encode_instruction(bytecode& bc, instruction const& instr) {
  instruction_info info = opcode_to_info(instr.opcode);
  assert(instr.operands.size() == info.num_operands);

  std::size_t index = bc.size();

  encode(bc, instr.opcode);
  for (std::size_t i = 0; i < info.num_operands; ++i)
    encode(bc, instr.operands[i]);

  return index;
}

std::size_t
instruction_size(instruction const& i) {
  return 1 + i.operands.size();
}

instruction
read_instruction(instruction_pointer& ip) {
  instruction result{read_opcode(ip)};
  instruction_info info = opcode_to_info(result.opcode);

  for (std::size_t i = 0; i < info.num_operands; ++i)
    result.operands.push_back(read_operand(ip));

  return result;
}

std::vector<instruction>
bytecode_to_instructions(bytecode const& bc) {
  instruction_pointer ip = bc.data();
  std::vector<instruction> result;
  while (ip != bc.data() + bc.size())
    result.push_back(read_instruction(ip));
  return result;
}

} // namespace insider
