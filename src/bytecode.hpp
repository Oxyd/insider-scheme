#ifndef INSIDER_BYTECODE_HPP
#define INSIDER_BYTECODE_HPP

#include <array>
#include <climits>
#include <cstddef>
#include <cstdint>
#include <stdexcept>
#include <string>
#include <tuple>
#include <unordered_map>
#include <utility>
#include <vector>

#include <fmt/format.h>

namespace insider {

using operand = std::uint64_t;

// This enum defines the numeric opcode values.
enum class opcode : std::uint8_t {
  no_operation,
  load_static,      // load-static <static number> <destination>
  load_top_level,   // load-top-level <top-level number> <destination>
  store_top_level,  // store-top-level <value> <top-level number>
  add,
  subtract,
  multiply,
  divide,
  arith_equal,
  less_than,
  greater_than, // 10
  set,              // set <source> <destination>
  call,             // call <procedure> <return value destination> <arguments ...>
  call_top_level,   // same as call, but <procedure> is the index of a top-level
  call_static,      //                   -- "" --                      static
  tail_call,        // tail-call <procedure> <arguments ...>
  tail_call_top_level,
  tail_call_static,
  ret,              // ret <return value>
  jump,             // jump <offset>
  jump_back,        // jump-back <offset>
  jump_unless,      // jump-unless <register> <offset>
  jump_back_unless, // jump-back-unless <register> <offset>
  make_closure,     // make-closure <procedure> <destination> <free variables ...>
  box,              // box <what> <destination> -- make a box containing what -- 20
  unbox,            // unbox <what> <destination> -- extract contained value from a box
  box_set,          // box-set <box> <value> -- replace box's contained value with a new one
  cons,             // cons <a> <b> <destination> -- make a cons pair (a . b)
  make_vector       // make-vector <destination> <elements ...>
};

// Metainformation about an opcode. Used for decoding instructions.
struct instruction_info {
  std::string     mnemonic;
  insider::opcode opcode{};
  std::size_t     num_operands;
  bool            extra_operands;

  instruction_info() = default;

  instruction_info(std::string_view mnemonic, insider::opcode opcode,
                   std::size_t num_operands, bool extra_operands = false)
    : mnemonic{mnemonic}
    , opcode{opcode}
    , num_operands{num_operands}
    , extra_operands{extra_operands}
  { }
};

// We need both a mapping from instruction mnemonics to instruction infos (for
// generating bytecode from text), and from numeric opcodes to instruction infos
// (for reading binary bytecode).

namespace detail {
  using info_tuple = std::tuple<char const*, opcode, std::size_t, bool>;

  template <std::size_t N, std::size_t... Is>
  constexpr auto
  make_opcodes(std::array<info_tuple, N> const& instructions,
               std::index_sequence<Is...>) {
    std::array<instruction_info, N> result{};
    ((result[static_cast<std::size_t>(std::get<1>(instructions[Is]))]
      = instruction_info{std::get<0>(instructions[Is]),
                         std::get<1>(instructions[Is]),
                         std::get<2>(instructions[Is]),
                         std::get<3>(instructions[Is])}), ...);
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
  std::tuple{"no-operation",        opcode::no_operation,        std::size_t{0}, false},
  std::tuple{"load-static",         opcode::load_static,         std::size_t{2}, false},
  std::tuple{"load-top-level",      opcode::load_top_level,      std::size_t{2}, false},
  std::tuple{"store-top-level",     opcode::store_top_level,     std::size_t{2}, false},
  std::tuple{"add",                 opcode::add,                 std::size_t{3}, false},
  std::tuple{"subtract",            opcode::subtract,            std::size_t{3}, false},
  std::tuple{"multiply",            opcode::multiply,            std::size_t{3}, false},
  std::tuple{"divide",              opcode::divide,              std::size_t{3}, false},
  std::tuple{"arith-equal",         opcode::arith_equal,         std::size_t{3}, false},
  std::tuple{"less-than",           opcode::less_than,           std::size_t{3}, false},
  std::tuple{"greater-than",        opcode::greater_than,        std::size_t{3}, false},
  std::tuple{"set!",                opcode::set,                 std::size_t{2}, false},
  std::tuple{"call",                opcode::call,                std::size_t{2}, true},
  std::tuple{"call-top-level",      opcode::call_top_level,      std::size_t{2}, true},
  std::tuple{"call-static",         opcode::call_static,         std::size_t{2}, true},
  std::tuple{"tail-call",           opcode::tail_call,           std::size_t{1}, true},
  std::tuple{"tail-call-top-level", opcode::tail_call_top_level, std::size_t{1}, true},
  std::tuple{"tail-call-static",    opcode::tail_call_static,    std::size_t{1}, true},
  std::tuple{"ret",                 opcode::ret,                 std::size_t{1}, false},
  std::tuple{"jump",                opcode::jump,                std::size_t{1}, false},
  std::tuple{"jump-back",           opcode::jump_back,           std::size_t{1}, false},
  std::tuple{"jump-unless",         opcode::jump_unless,         std::size_t{2}, false},
  std::tuple{"jump-back-unless",    opcode::jump_back_unless,    std::size_t{2}, false},
  std::tuple{"make-closure",        opcode::make_closure,        std::size_t{2}, true},
  std::tuple{"box",                 opcode::box,                 std::size_t{2}, false},
  std::tuple{"unbox",               opcode::unbox,               std::size_t{2}, false},
  std::tuple{"box-set!",            opcode::box_set,             std::size_t{2}, false},
  std::tuple{"cons",                opcode::cons,                std::size_t{3}, false},
  std::tuple{"make-vector",         opcode::make_vector,         std::size_t{1}, true}
};

inline auto // std::array<instruction_info, N>
opcode_value_to_info = detail::make_opcodes(instructions);

inline auto
opcode_to_info(opcode oc) { return opcode_value_to_info[static_cast<std::size_t>(oc)]; }

instruction_info
mnemonic_to_info(std::string_view);

struct instruction {
  insider::opcode opcode;
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

using bytecode = std::vector<std::byte>;

void
encode_instruction(bytecode&, instruction const&);

inline opcode
read_opcode(bytecode const& bc, std::size_t& pc) {
  auto opcode_num = std::to_integer<std::uint8_t>(bc[pc++]);
  if (opcode_num > instructions.size())
    throw std::runtime_error{fmt::format("Invalid opcode number: {}", +opcode_num)};
  return opcode{opcode_num};
}

inline operand
read_operand(bytecode const& bc, std::size_t& pc) {
  operand result = std::to_integer<std::uint8_t>(bc[pc++]);
  if (result < 0xFF)
    return result;

  result = 0;
  for (std::size_t i = 0; i < sizeof(operand); ++i)
    result |= std::to_integer<std::uint8_t>(bc[pc++]) << (i * CHAR_BIT);

  return result;
}

instruction
read_instruction(bytecode const&, std::size_t& pc);

} // namespace insider

#endif
