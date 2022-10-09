#ifndef INSIDER_VM_OPERAND_HPP
#define INSIDER_VM_OPERAND_HPP

#include <cstdint>
#include <limits>

namespace insider {

using operand = std::uint16_t;

using immediate_type = std::int16_t;

constexpr operand operand_max = std::numeric_limits<operand>::max();

constexpr immediate_type immediate_bias
  = std::numeric_limits<operand>::max() / 2;

constexpr immediate_type immediate_max
  = std::numeric_limits<operand>::max() / 2;
constexpr immediate_type immediate_min = -immediate_bias;

inline operand
immediate_to_operand(immediate_type imm) {
  return static_cast<operand>(imm + immediate_bias);
}

inline immediate_type
operand_to_immediate(operand op) {
  return static_cast<immediate_type>(op - immediate_bias);
}

}

#endif
