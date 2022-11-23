#ifndef INSIDER_VM_BYTECODE_HPP
#define INSIDER_VM_BYTECODE_HPP

#include "vm/operand.hpp"

#include <array>
#include <cassert>
#include <climits>
#include <cstddef>
#include <cstdint>
#include <stdexcept>
#include <string>
#include <tuple>
#include <type_traits>
#include <unordered_map>
#include <utility>
#include <vector>

#include <fmt/format.h>

namespace insider {

// This enum defines the numeric opcode values.
enum class opcode : std::uint16_t {
  no_operation,
  load_constant,
  load_top_level,
  store_top_level,
  load_dynamic_top_level,
  load_null,
  load_void,
  load_t,
  load_f,
  load_eof,
  load_fixnum,
  add,
  subtract,
  multiply,
  divide,
  arith_equal,
  less,
  greater,
  less_or_equal,
  greater_or_equal,
  set,
  call,
  tail_call,
  call_known_scheme,
  tail_call_known_scheme,
  call_known_native,
  tail_call_known_native,
  ret,
  jump,
  jump_unless,
  make_closure,
  box,
  unbox,
  box_set,
  cons,
  car,
  cdr,
  vector_set,
  vector_ref,
  vector_length,
  bytevector_u8_set,
  bytevector_u8_ref,
  bytevector_length,
  string_ref,
  string_set,
  string_set_byte_index,
  string_length,
  string_byte_length,
  next_code_point_byte_index,
  previous_code_point_byte_index,
  string_append_char,
  string_null,
  type,
  eq,
  eqv,
  equal,
};

// Metainformation about an opcode. Used for decoding instructions.
struct instruction_info {
  std::string     mnemonic;
  insider::opcode opcode{};
  std::size_t     num_operands{};

  instruction_info() = default;

  instruction_info(std::string_view mnemonic, insider::opcode opcode,
                   std::size_t num_operands)
    : mnemonic{mnemonic}
    , opcode{opcode}
    , num_operands{num_operands}
  { }
};

// We need both a mapping from instruction mnemonics to instruction infos (for
// generating bytecode from text), and from numeric opcodes to instruction infos
// (for reading binary bytecode).

namespace detail {
  using info_tuple = std::tuple<char const*, opcode, std::size_t>;

  template <std::size_t N, std::size_t... Is>
  constexpr auto
  make_opcodes(std::array<info_tuple, N> const& instructions,
               std::index_sequence<Is...>) {
    std::array<instruction_info, N> result{};
    ((result[static_cast<std::size_t>(std::get<1>(instructions[Is]))]
      = instruction_info{std::get<0>(instructions[Is]),
                         std::get<1>(instructions[Is]),
                         std::get<2>(instructions[Is])}), ...);
    return result;
  }

  template <std::size_t N>
  constexpr auto
  make_opcodes(std::array<info_tuple, N> const& instructions) {
    return make_opcodes(instructions, std::make_index_sequence<N>{});
  }
}

// This array can be in any order; it does not have to match the order given in
// the opcode enum. Opcode values are still given by the enum.
constexpr std::array
instructions{
  std::tuple{"no-operation", opcode::no_operation, std::size_t{0}},
  std::tuple{"load-constant", opcode::load_constant, std::size_t{2}},
  std::tuple{"load-top-level", opcode::load_top_level, std::size_t{2}},
  std::tuple{"store-top-level", opcode::store_top_level, std::size_t{2}},
  std::tuple{"load-dynamic-top-level", opcode::load_dynamic_top_level,
             std::size_t{2}},
  std::tuple{"load-null", opcode::load_null, std::size_t{1}},
  std::tuple{"load-void", opcode::load_void, std::size_t{1}},
  std::tuple{"load-#t", opcode::load_t, std::size_t{1}},
  std::tuple{"load-#f", opcode::load_f, std::size_t{1}},
  std::tuple{"load-eof", opcode::load_eof, std::size_t{1}},
  std::tuple{"load-fixnum", opcode::load_fixnum, std::size_t{2}},
  std::tuple{"add", opcode::add, std::size_t{3}},
  std::tuple{"subtract", opcode::subtract, std::size_t{3}},
  std::tuple{"multiply", opcode::multiply, std::size_t{3}},
  std::tuple{"divide", opcode::divide, std::size_t{3}},
  std::tuple{"arith-equal", opcode::arith_equal, std::size_t{3}},
  std::tuple{"less", opcode::less, std::size_t{3}},
  std::tuple{"greater", opcode::greater, std::size_t{3}},
  std::tuple{"less-or-equal", opcode::less_or_equal, std::size_t{3}},
  std::tuple{"greater-or-equal", opcode::greater_or_equal, std::size_t{3}},
  std::tuple{"set!", opcode::set, std::size_t{2}},
  std::tuple{"call", opcode::call, std::size_t{3}},
  std::tuple{"tail-call", opcode::tail_call, std::size_t{2}},
  std::tuple{"call/known-scheme", opcode::call_known_scheme, std::size_t{3}},
  std::tuple{"tail-call/known-scheme", opcode::tail_call_known_scheme, std::size_t{2}},
  std::tuple{"call/known-native", opcode::call_known_native, std::size_t{3}},
  std::tuple{"tail-call/known-native", opcode::tail_call_known_native, std::size_t{2}},
  std::tuple{"ret", opcode::ret, std::size_t{1}},
  std::tuple{"jump", opcode::jump, std::size_t{1}},
  std::tuple{"jump-unless", opcode::jump_unless, std::size_t{2}},
  std::tuple{"make-closure", opcode::make_closure, std::size_t{3}},
  std::tuple{"box", opcode::box, std::size_t{2}},
  std::tuple{"unbox", opcode::unbox, std::size_t{2}},
  std::tuple{"box-set!", opcode::box_set, std::size_t{2}},
  std::tuple{"cons", opcode::cons, std::size_t{3}},
  std::tuple{"car", opcode::car, std::size_t{2}},
  std::tuple{"cdr", opcode::cdr, std::size_t{2}},
  std::tuple{"vector-set!", opcode::vector_set, std::size_t{3}},
  std::tuple{"vector-ref", opcode::vector_ref, std::size_t{3}},
  std::tuple{"vector-length", opcode::vector_length, std::size_t{2}},
  std::tuple{"bytevector-u8-set!", opcode::bytevector_u8_set, std::size_t{3}},
  std::tuple{"bytevector-u8-ref", opcode::bytevector_u8_ref, std::size_t{3}},
  std::tuple{"bytevector-length", opcode::bytevector_length, std::size_t{2}},
  std::tuple{"string-ref", opcode::string_ref, std::size_t{2}},
  std::tuple{"string-set!", opcode::string_set, std::size_t{3}},
  std::tuple{"string-set!/byte-index", opcode::string_set_byte_index, std::size_t{3}},
  std::tuple{"string-length", opcode::string_length, std::size_t{2}},
  std::tuple{"string-byte-length", opcode::string_byte_length, std::size_t{2}},
  std::tuple{"next-code-point-byte-index", opcode::next_code_point_byte_index, std::size_t{3}},
  std::tuple{"previous-code-point-byte-index", opcode::previous_code_point_byte_index, std::size_t{3}},
  std::tuple{"string-append-char!", opcode::string_append_char, std::size_t{2}},
  std::tuple{"string-null?", opcode::string_null, std::size_t{2}},
  std::tuple{"type", opcode::type, std::size_t{2}},
  std::tuple{"eq?", opcode::eq, std::size_t{3}},
  std::tuple{"eqv?", opcode::eqv, std::size_t{3}},
  std::tuple{"equal?", opcode::equal, std::size_t{3}}
};

inline auto const // std::array<instruction_info, N>
opcode_value_to_info = detail::make_opcodes(instructions);

inline auto
opcode_to_info(opcode oc) {
  return opcode_value_to_info[static_cast<std::size_t>(oc)];
}

instruction_info
mnemonic_to_info(std::string_view);

struct instruction {
  insider::opcode opcode;
  operand a;
  operand b;
  operand c;

  instruction() = default;

  instruction(insider::opcode oc)
    : opcode{oc}
    , a{}
    , b{}
    , c{}
  { }

  instruction(insider::opcode oc, operand a)
    : opcode{oc}
    , a{a}
    , b{}
    , c{}
  { }

  instruction(insider::opcode oc, operand a, operand b)
    : opcode{oc}
    , a{a}
    , b{b}
    , c{}
  { }

  instruction(insider::opcode oc, operand a, operand b, operand c)
    : opcode{oc}
    , a{a}
    , b{b}
    , c{c}
  { }

  friend bool
  operator == (instruction const&, instruction const&) = default;
};

std::ostream&
operator << (std::ostream&, instruction const&);

using mutable_bytecode = std::vector<std::uint16_t>;
using bytecode = std::shared_ptr<std::uint16_t[]>;
using instruction_pointer = std::uint16_t const*;

std::size_t
encode_instruction(mutable_bytecode&, instruction const&);

// Number of words an instruction will take when encoded.
std::size_t
instruction_size(opcode);

std::vector<operand>
instruction_operands_vector(instruction);

inline opcode
read_opcode(instruction_pointer& ip) {
  return static_cast<opcode>(*ip++);
}

inline operand
read_operand(instruction_pointer& ip) {
  return *ip++;
}

instruction
read_instruction(instruction_pointer& ip);

std::vector<instruction>
bytecode_to_instructions(mutable_bytecode const&);

} // namespace insider

namespace std {
  template <>
  struct hash<insider::instruction> {
    std::size_t
    operator () (insider::instruction i) const {
      return std::hash<insider::opcode>{}(i.opcode)
             ^ std::hash<insider::operand>{}(i.a)
             ^ std::hash<insider::operand>{}(i.b)
             ^ std::hash<insider::operand>{}(i.c);
    }
  };
}

#endif
