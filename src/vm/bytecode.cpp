#include "bytecode.hpp"

#include <fmt/format.h>

#include <algorithm>
#include <cassert>
#include <cstddef>

namespace insider {

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
  if (info.num_operands == 0)
    return out << "{" << info.mnemonic << "}";
  else
    return out << fmt::format("{{{} {}}}",
                              info.mnemonic,
                              fmt::join(instruction_operands_vector(i), ", "));
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

  std::size_t index = bc.size();

  encode(bc, instr.opcode);
  for (operand op : instruction_operands_vector(instr))
    encode(bc, op);

  return index;
}

std::size_t
instruction_size(opcode oc) {
  return 1 + opcode_to_info(oc).num_operands;
}

std::vector<operand>
instruction_operands_vector(instruction instr) {
  std::size_t num_operands = opcode_to_info(instr.opcode).num_operands;
  switch (num_operands) {
  case 0: return {};
  case 1: return {instr.a};
  case 2: return {instr.a, instr.b};
  case 3: return {instr.a, instr.b, instr.c};
  default:
    assert(false);
    return {};
  }
}

instruction
read_instruction(instruction_pointer& ip) {
  instruction result{read_opcode(ip)};
  instruction_info info = opcode_to_info(result.opcode);

  operand* ops[]{&result.a, &result.b, &result.c};
  operand** op = ops;
  for (std::size_t i = 0; i < info.num_operands; ++i)
    (**op++) = read_operand(ip);

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
