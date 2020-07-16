#ifndef SCHEME_BYTECODE_HPP
#define SCHEME_BYTECODE_HPP

#include <array>
#include <cstdint>
#include <string>
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

enum class opcode_category {
  none,
  register_,
  absolute,
  offset
};

// Metainformation about an opcode. Used for decoding instructions.
struct instruction_info {
  std::string      mnemonic;
  insider::opcode  opcode{};
  opcode_category  x = opcode_category::none;
  opcode_category  y = opcode_category::none;
  opcode_category  dest = opcode_category::none;

  instruction_info() = default;

  instruction_info(std::string_view mnemonic, insider::opcode opcode,
                   opcode_category x, opcode_category y, opcode_category dest)
    : mnemonic{mnemonic}
    , opcode{opcode}
    , x{x}
    , y{y}
    , dest{dest}
  { }
};

// We need both a mapping from instruction mnemonics to instruction infos (for
// generating bytecode from text), and from numeric opcodes to instruction infos
// (for reading binary bytecode).

namespace detail {
  using info_tuple = std::tuple<char const*, opcode, opcode_category, opcode_category, opcode_category>;

  template <std::size_t N, std::size_t... Is>
  constexpr auto
  make_opcodes(std::array<info_tuple, N> const& instructions,
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
  make_opcodes(std::array<info_tuple, N> const& instructions) {
    return make_opcodes(instructions, std::make_index_sequence<N>{});
  }
}

// This array can be in any order; it does not have to match the order given in
// the opcode enum. Opcode values are still given by the enum.
constexpr std::array
instructions{
  //         mnemonic           opcode                   x      y      dest
  std::tuple{"no-operation",    opcode::no_operation,    opcode_category::none, opcode_category::none, opcode_category::none},
  std::tuple{"add",             opcode::add,             opcode_category::register_, opcode_category::register_, opcode_category::register_},
  std::tuple{"subtract",        opcode::subtract,        opcode_category::register_, opcode_category::register_, opcode_category::register_},
  std::tuple{"multiply",        opcode::multiply,        opcode_category::register_, opcode_category::register_, opcode_category::register_},
  std::tuple{"divide",          opcode::divide,          opcode_category::register_, opcode_category::register_, opcode_category::register_},
  std::tuple{"arith-equal",     opcode::arith_equal,     opcode_category::register_, opcode_category::register_, opcode_category::register_},
  std::tuple{"less-than",       opcode::less_than,       opcode_category::register_, opcode_category::register_, opcode_category::register_},
  std::tuple{"greater-than",    opcode::greater_than,    opcode_category::register_, opcode_category::register_, opcode_category::register_},
  std::tuple{"set!",            opcode::set,             opcode_category::register_, opcode_category::none, opcode_category::register_},
  std::tuple{"call",            opcode::call,            opcode_category::register_, opcode_category::absolute, opcode_category::register_},
  std::tuple{"tail-call",       opcode::tail_call,       opcode_category::register_, opcode_category::absolute, opcode_category::none},
  std::tuple{"ret",             opcode::ret,             opcode_category::register_, opcode_category::none, opcode_category::none},
  std::tuple{"jump",            opcode::jump,            opcode_category::offset, opcode_category::none, opcode_category::none},
  std::tuple{"jump-unless",     opcode::jump_unless,     opcode_category::register_, opcode_category::offset, opcode_category::none},
  std::tuple{"data",            opcode::data,            opcode_category::register_, opcode_category::register_, opcode_category::register_},
  std::tuple{"make-closure",    opcode::make_closure,    opcode_category::register_, opcode_category::absolute, opcode_category::register_},
  std::tuple{"box",             opcode::box,             opcode_category::register_, opcode_category::none, opcode_category::register_},
  std::tuple{"unbox",           opcode::unbox,           opcode_category::register_, opcode_category::none, opcode_category::register_},
  std::tuple{"box-set!",        opcode::box_set,         opcode_category::register_, opcode_category::register_, opcode_category::none},
  std::tuple{"cons",            opcode::cons,            opcode_category::register_, opcode_category::register_, opcode_category::register_},
  std::tuple{"make-vector",     opcode::make_vector,     opcode_category::absolute, opcode_category::none, opcode_category::register_},
};

inline auto // std::array<instruction_info, N>
opcode_value_to_info = detail::make_opcodes(instructions);

inline auto
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
