#ifndef SCHEME_BYTECODE_HPP
#define SCHEME_BYTECODE_HPP

#include <array>
#include <cstdint>
#include <string_view>
#include <tuple>
#include <unordered_map>
#include <utility>
#include <vector>

namespace insider {

class operand {
public:
  using representation_type = std::uint32_t;
  using offset_type = std::int32_t;

  enum class scope_type : representation_type {
    local,
    global,
    static_,
    closure
  };

  operand() = default;

  operand(scope_type scope, representation_type value)
    : value_{(value << scope_bits) | static_cast<representation_type>(scope)}
  { }

  static operand
  raw(representation_type v) { return operand{v}; }

  static operand
  immediate(representation_type value) { return operand{value}; }

  static operand
  offset(offset_type value) { return operand{static_cast<representation_type>(value)}; }

  static operand
  local(representation_type value) { return operand{scope_type::local, value}; }

  static operand
  global(representation_type value) { return operand{scope_type::global, value}; }

  static operand
  static_(representation_type value) { return operand{scope_type::static_, value}; }

  static operand
  closure(representation_type value) { return operand{scope_type::closure, value}; }

  scope_type
  scope() const { return scope_type{value_ & scope_mask}; }

  representation_type
  value() const { return value_ >> scope_bits; }

  representation_type
  immediate_value() const { return value_; }

  offset_type
  offset() const { return static_cast<offset_type>(value_); }

  representation_type
  representation() const { return value_; }

private:
  // Bottom two bits give the scope. Remaining bits give the value of the operand.
  static constexpr std::size_t scope_bits = 2;
  static constexpr representation_type scope_mask = (1u << scope_bits) - 1;

  representation_type value_ = static_cast<representation_type>(scope_type::local);

  explicit
  operand(representation_type v) : value_{v} { }
};

// This enum defines the numeric opcode values.
enum class opcode : std::uint8_t {
  no_operation,
  add,
  subtract,
  multiply,
  divide,
  arith_equal,
  less_than,
  greater_than,
  set,             // set <source> <> <destination>
  call,            // call <procedure> <number of arguments> <return value destination>
  tail_call,       // tail-call <procedure> <number of arguments>
                   // call and tail-call are then followed by a number of data pseudo-instructions giving
                   // the actual arguments to the procedure.
  ret,             // ret <return value>
  jump,            // jump <offset>
  jump_unless,     // jump-unless <register> <offset>
  data,            // data <x> <y> <z> -- a way to embed extra operands in the instruction stream
  make_closure,    // make-closure <procedure> <number of free variables> <destination>
  box,             // box <what> <> <destination> -- make a box containing what
  unbox,           // unbox <what> <> <destination> -- extract contained value from a box
  box_set,         // box-set <box> <value> -- replace box's contained value with a new one
  cons,            // cons <a> <b> <destination> -- make a cons pair (a . b)
  make_vector      // make-vector <number of elements> <> <destination>
};

// Metainformation about an opcode. Used for decoding instructions.
struct instruction_info {
  std::string_view mnemonic;
  insider::opcode  opcode{};
  bool             has_x{};
  bool             has_y{};
  bool             has_dest{};

  constexpr
  instruction_info() = default;

  constexpr
  instruction_info(std::string_view mnemonic, insider::opcode opcode, bool has_x, bool has_y, bool has_dest)
    : mnemonic{mnemonic}
    , opcode{opcode}
    , has_x{has_x}
    , has_y{has_y}
    , has_dest{has_dest}
  { }
};

// We need both a mapping from instruction mnemonics to instruction infos (for
// generating bytecode from text), and from numeric opcodes to instruction infos
// (for reading binary bytecode).

namespace detail {
  template <std::size_t N, std::size_t... Is>
  constexpr auto
  make_opcodes(std::array<std::tuple<char const*, opcode, bool, bool, bool>, N> const& instructions,
               std::index_sequence<Is...>) {
    std::array<instruction_info, N> result{};
    ((result[static_cast<std::size_t>(std::get<1>(instructions[Is]))]
      = instruction_info{std::get<0>(instructions[Is]),
                         std::get<1>(instructions[Is]),
                         std::get<2>(instructions[Is]),
                         std::get<3>(instructions[Is]),
                         std::get<4>(instructions[Is])}), ...);
    return result;
  }

  template <std::size_t N>
  constexpr auto
  make_opcodes(std::array<std::tuple<char const*, opcode, bool, bool, bool>, N> const& instructions) {
    return make_opcodes(instructions, std::make_index_sequence<N>{});
  }
}

// This array can be in any order; it does not have to match the order given in
// the opcode enum. Opcode values are still given by the enum.
constexpr std::array
instructions{
  //         mnemonic           opcode                   has_x  has_y  has_dest
  std::tuple{"no-operation",    opcode::no_operation,    false, false, false},
  std::tuple{"add",             opcode::add,             true,  true,  true},
  std::tuple{"subtract",        opcode::subtract,        true,  true,  true},
  std::tuple{"multiply",        opcode::multiply,        true,  true,  true},
  std::tuple{"divide",          opcode::divide,          true,  true,  true},
  std::tuple{"arith-equal",     opcode::arith_equal,     true,  true,  true},
  std::tuple{"less-than",       opcode::less_than,       true,  true,  true},
  std::tuple{"greater-than",    opcode::greater_than,    true,  true,  true},
  std::tuple{"set",             opcode::set,             true,  false, true},
  std::tuple{"call",            opcode::call,            true,  true,  true},
  std::tuple{"tail-call",       opcode::tail_call,       true,  true,  false},
  std::tuple{"ret",             opcode::ret,             true,  false, false},
  std::tuple{"jump",            opcode::jump,            true,  false, false},
  std::tuple{"jump-unless",     opcode::jump_unless,     true,  true,  false},
  std::tuple{"data",            opcode::data,            true,  true,  true},
  std::tuple{"make-closure",    opcode::make_closure,    true,  true,  true}
};

constexpr auto // std::array<instruction_info, N>
opcode_value_to_info = detail::make_opcodes(instructions);

constexpr auto
opcode_to_info(opcode oc) { return opcode_value_to_info[static_cast<std::size_t>(oc)]; }

instruction_info
mnemonic_to_info(std::string_view);

struct instruction {
  insider::opcode opcode;
  operand     x, y, dest;

  instruction() = default;
  instruction(insider::opcode oc, operand x, operand y, operand dest)
    : opcode{oc}
    , x{x}
    , y{y}
    , dest{dest}
  { }
  instruction(std::string_view mnemonic, operand, operand, operand);
};

using bytecode = std::vector<instruction>;

} // namespace insider

#endif
