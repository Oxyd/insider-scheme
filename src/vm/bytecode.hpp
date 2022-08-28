#ifndef INSIDER_VM_BYTECODE_HPP
#define INSIDER_VM_BYTECODE_HPP

#include "runtime/integer.hpp"
#include "vm/operand.hpp"

#include <array>
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
  load_static,      // load-static <static number> <destination>
  load_top_level,   // load-top-level <top-level number> <destination>
  store_top_level,  // store-top-level <value> <top-level number>
  load_dynamic_top_level, // load-dynamic-top-level <static num> <destination>
  add,
  subtract,
  multiply,
  divide,
  arith_equal,
  less,
  greater,
  less_or_equal,
  greater_or_equal,
  set,              // set <source> <destination>
  push,             // push <value>
  pop,              // pop <destination>
  call,             // call <procedure> <base> <number of arguments> <result register>
  call_top_level,   // same as call, but <procedure> is the index of a top-level
  call_static,      //                   -- "" --                      static
  tail_call,        // tail-call <procedure> <base> <number of arguments>
  tail_call_top_level,
  tail_call_static,
  ret,              // ret <return value>
  jump,             // jump <offset>
  jump_back,        // jump-back <offset>
  jump_unless,      // jump-unless <register> <offset>
  jump_back_unless, // jump-back-unless <register> <offset>
  make_closure,     // make-closure <procedure> <base> <num free> <destination>
  box,              // box <what> <destination> -- make a box containing what -- 20
  unbox,            // unbox <what> <destination> -- extract contained value from a box
  box_set,          // box-set <box> <value> -- replace box's contained value with a new one
  cons,             // cons <a> <b> <destination> -- make a cons pair (a . b)
  car,              // car <x> <destination>
  cdr,              // cdr <x> <destination>
  make_vector,      // make-vector <num elements> <destination>
  vector_set,       // vector-set! <vector> <index> <value>
  vector_ref,       // vector-ref <vector> <index> <destination>
  type,             // type <value> <destination>
  eq                // eq <x> <y> <destination>
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
  std::tuple{"no-operation",
             opcode::no_operation,        std::size_t{0}},
  std::tuple{"load-static",
             opcode::load_static,         std::size_t{2}},
  std::tuple{"load-top-level",
            opcode::load_top_level,       std::size_t{2}},
  std::tuple{"store-top-level",
             opcode::store_top_level,     std::size_t{2}},
  std::tuple{"load-dynamic-top-level",
             opcode::load_dynamic_top_level, std::size_t{2}},
  std::tuple{"add",
             opcode::add,                 std::size_t{3}},
  std::tuple{"subtract",
             opcode::subtract,            std::size_t{3}},
  std::tuple{"multiply",
             opcode::multiply,            std::size_t{3}},
  std::tuple{"divide",
             opcode::divide,              std::size_t{3}},
  std::tuple{"arith-equal",
             opcode::arith_equal,         std::size_t{3}},
  std::tuple{"less",
             opcode::less,                std::size_t{3}},
  std::tuple{"greater",
             opcode::greater,             std::size_t{3}},
  std::tuple{"less-or-equal",
             opcode::less_or_equal,       std::size_t{3}},
  std::tuple{"greater-or-equal",
             opcode::greater_or_equal,    std::size_t{3}},
  std::tuple{"set!",
             opcode::set,                 std::size_t{2}},
  std::tuple{"push",
             opcode::push,                std::size_t{1}},
  std::tuple{"pop",
             opcode::pop,                 std::size_t{1}},
  std::tuple{"call",
             opcode::call,                std::size_t{4}},
  std::tuple{"call-top-level",
             opcode::call_top_level,      std::size_t{4}},
  std::tuple{"call-static",
             opcode::call_static,         std::size_t{4}},
  std::tuple{"tail-call",
             opcode::tail_call,           std::size_t{3}},
  std::tuple{"tail-call-top-level",
             opcode::tail_call_top_level, std::size_t{3}},
  std::tuple{"tail-call-static",
             opcode::tail_call_static,    std::size_t{3}},
  std::tuple{"ret",
             opcode::ret,                 std::size_t{1}},
  std::tuple{"jump",
             opcode::jump,                std::size_t{1}},
  std::tuple{"jump-back",
             opcode::jump_back,           std::size_t{1}},
  std::tuple{"jump-unless",
             opcode::jump_unless,         std::size_t{2}},
  std::tuple{"jump-back-unless",
             opcode::jump_back_unless,    std::size_t{2}},
  std::tuple{"make-closure",
             opcode::make_closure,        std::size_t{4}},
  std::tuple{"box",
             opcode::box,                 std::size_t{2}},
  std::tuple{"unbox",
             opcode::unbox,               std::size_t{2}},
  std::tuple{"box-set!",
             opcode::box_set,             std::size_t{2}},
  std::tuple{"cons",
             opcode::cons,                std::size_t{3}},
  std::tuple{"car",
             opcode::car,                 std::size_t{2}},
  std::tuple{"cdr",
             opcode::cdr,                 std::size_t{2}},
  std::tuple{"make-vector",
             opcode::make_vector,         std::size_t{2}},
  std::tuple{"vector-set!",
             opcode::vector_set,          std::size_t{3}},
  std::tuple{"vector-ref",
             opcode::vector_ref,          std::size_t{3}},
  std::tuple{"type",
             opcode::type,                std::size_t{2}},
  std::tuple{"eq?",
             opcode::eq,                  std::size_t{3}}
};

inline auto const // std::array<instruction_info, N>
opcode_value_to_info = detail::make_opcodes(instructions);

inline auto
opcode_to_info(opcode oc) { return opcode_value_to_info[static_cast<std::size_t>(oc)]; }

instruction_info
mnemonic_to_info(std::string_view);

struct instruction {
  insider::opcode opcode{};
  std::vector<operand> operands;

  instruction() = default;

  explicit
  instruction(insider::opcode oc)
    : opcode{oc}
  { }

  instruction(insider::opcode oc, std::vector<operand> operands)
    : opcode{oc}
    , operands{std::move(operands)}
  { }

  template <typename... Op>
  explicit
  instruction(insider::opcode oc, Op... operands)
    : opcode{oc}
    , operands{operands...}
  { }

  instruction(std::string_view mnemonic, std::vector<operand> operands);
};

using bytecode = std::vector<std::uint16_t>;

std::size_t
encode_instruction(bytecode&, instruction const&);

inline opcode
read_opcode(bytecode const& bc, integer::value_type& pc) {
  return static_cast<opcode>(bc[pc++]);
}

inline operand
read_operand(bytecode const& bc, integer::value_type& pc) {
  return bc[pc++];
}

instruction
read_instruction(bytecode const&, integer::value_type& pc);

} // namespace insider

namespace std {
  template <>
  struct hash<insider::instruction> {
    std::size_t
    operator () (insider::instruction i) const {
      return std::hash<insider::opcode>{}(i.opcode)
             ^ (!i.operands.empty() ? i.operands.front() : 0);
    }
  };
}

#endif
