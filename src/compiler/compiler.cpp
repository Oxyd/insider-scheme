#include "compiler.hpp"

#include "compiler/analyser.hpp"
#include "compiler/source_code_provider.hpp"
#include "io/read.hpp"
#include "runtime/action.hpp"
#include "runtime/basic_types.hpp"

#include <fmt/format.h>

#include <limits>
#include <memory>
#include <optional>
#include <stdexcept>
#include <type_traits>
#include <unordered_map>
#include <unordered_map>

namespace insider {

namespace {
  class shared_register;

  class register_allocator {
  public:
    shared_register
    allocate_local();

    void
    ref(operand);

    void
    unref(operand);

    unsigned
    locals_used() const { return locals_max_; }

  private:
    operand locals_max_ = 0;
    std::vector<operand> locals_free_;
    std::unordered_map<operand, unsigned> local_ref_counts_;
  };

  class shared_register {
  public:
    shared_register() = default;
    shared_register(register_allocator&, operand);
    shared_register(shared_register const&);
    shared_register(shared_register&&);
    ~shared_register();

    shared_register&
    operator = (shared_register const&);

    shared_register&
    operator = (shared_register&&);

    operand
    operator * () const { assert(has_value()); return *value_; }

    bool
    has_value() const { return (bool) value_; }

    explicit operator
    bool () const { return has_value(); }

    bool
    operator == (shared_register other) const { return alloc_ == other.alloc_ && value_ == other.value_; }

    bool
    operator != (shared_register other) const { return !operator == (other); }

  private:
    register_allocator*    alloc_ = nullptr;
    std::optional<operand> value_;
  };

  class variable_bindings {
  public:
    struct binding {
      std::shared_ptr<insider::variable> variable;
      shared_register                    destination;

      binding() = default;

      binding(std::shared_ptr<insider::variable> variable, shared_register destination)
        : variable{std::move(variable)}
        , destination{std::move(destination)}
      { }
    };
    using scope = std::vector<binding>;
    using free_variables_map = std::unordered_map<std::shared_ptr<variable>, shared_register>;

    class unique_scope {
    public:
      explicit
      unique_scope(variable_bindings* b) : b_{b} { }
      unique_scope(unique_scope const&) = delete;
      void operator = (unique_scope const&) = delete;

      ~unique_scope() { b_->pop_scope(); }

    private:
      variable_bindings* b_;
    };

    unique_scope
    push_scope(scope = {});

    void
    pop_scope();

    shared_register
    lookup(std::shared_ptr<variable> const&) const;

  private:
    std::vector<scope> scopes_;
  };

  struct procedure_context {
    procedure_context*             parent = nullptr;
    register_allocator             registers;
    variable_bindings              bindings;
    std::vector<insider::bytecode> bytecode_stack;  // Stack of bytecodes to facilitate compiling ifs.
    insider::module_&              module_;

    procedure_context(procedure_context* parent, insider::module_& m)
      : parent{parent}
      , module_{m}
    {
      bytecode_stack.emplace_back();
    }
  };
}

shared_register
register_allocator::allocate_local() {
  if (locals_free_.empty())
    return {*this, operand{locals_max_++}};
  else {
    auto result = operand{locals_free_.back()};
    locals_free_.pop_back();
    return {*this, result};
  }
}

void
register_allocator::ref(operand o) {
  ++local_ref_counts_[o];
}

void
register_allocator::unref(operand o) {
  auto ref_count = local_ref_counts_.find(o);
  assert(ref_count != local_ref_counts_.end());

  if (--ref_count->second == 0) {
    locals_free_.push_back(o);
    local_ref_counts_.erase(ref_count);
  }
}

shared_register::shared_register(register_allocator& alloc, operand value)
  : alloc_{&alloc}
  , value_{value}
{
  alloc_->ref(*value_);
}

shared_register::shared_register(shared_register const& other)
  : alloc_{other.alloc_}
  , value_{other.value_}
{
  if (alloc_)
    alloc_->ref(*value_);
}

shared_register::shared_register(shared_register&& other)
  : alloc_{other.alloc_}
  , value_{other.value_}
{
  other.alloc_ = nullptr;
}

shared_register::~shared_register() {
  if (alloc_)
    alloc_->unref(*value_);
}

shared_register&
shared_register::operator = (shared_register const& other) {
  if (alloc_)
    alloc_->unref(*value_);

  alloc_ = other.alloc_;
  value_ = other.value_;
  if (alloc_)
    alloc_->ref(*value_);

  return *this;
}

shared_register&
shared_register::operator = (shared_register&& other) {
  if (alloc_)
    alloc_->unref(*value_);

  alloc_ = other.alloc_;
  value_ = other.value_;
  other.alloc_ = nullptr;

  return *this;
}

auto
variable_bindings::push_scope(scope s) -> unique_scope {
  scopes_.emplace_back(std::move(s));
  return unique_scope{this};
}

void
variable_bindings::pop_scope() {
  scopes_.pop_back();
}

shared_register
variable_bindings::lookup(std::shared_ptr<variable> const& v) const {
  for (auto scope = scopes_.rbegin(); scope != scopes_.rend(); ++scope)
    for (binding const& b : *scope)
      if (b.variable == v)
        return b.destination;

  assert(false); // TODO: This shouldn't ever happen any longer.
  return {};
}

namespace {
  // Caller can either not be interested in any result, or it can want the
  // result in any register, or it can want the result in a specific
  // register. The callee on the other hand may or may not produce a result at
  // all.
  class result_register {
  public:
    explicit
    result_register(bool result_used = true, bool may_alias = false)
      : used_{result_used}
      , may_alias_{may_alias}
    { }

    bool
    result_used() const { return used_; }

    bool
    has_result() const { return reg_.has_value(); }

    shared_register
    get(procedure_context&);

    void
    set(shared_register reg) { assert(!reg_); reg_ = reg; }

    bool
    may_alias() const { return may_alias_; }

  private:
    shared_register reg_;
    bool used_;
    bool may_alias_;
  };
}

shared_register
result_register::get(procedure_context& proc) {
  if (!reg_)
    reg_ = proc.registers.allocate_local();

  return reg_;
}

template <typename T>
operand
to_operand(T value) {
  static_assert(!std::is_signed_v<T>);
  if (value > std::numeric_limits<operand>::max())
    throw std::runtime_error{fmt::format("Implementation limit exceeded. Cannot encode {} as an instruction operand",
                                         value)};

  return operand(value);
}

static void
compile_expression(context& ctx, procedure_context& proc, expression const&, bool tail, result_register&);

static shared_register
compile_expression_to_register(context& ctx, procedure_context& proc, expression const& stx, bool tail) {
  result_register result;
  compile_expression(ctx, proc, stx, tail, result);
  return result.has_result() ? result.get(proc) : shared_register{};
}

static shared_register
compile_expression_to_fresh_register(context& ctx, procedure_context& proc, expression const& stx) {
  result_register result{true, false};
  compile_expression(ctx, proc, stx, false, result);
  return result.has_result() ? result.get(proc) : shared_register{};
}

static void
compile_sequence(context& ctx, procedure_context& proc, sequence_expression const&, bool tail, result_register&);

static void
compile_local_reference(procedure_context& proc, local_reference_expression const& stx, result_register&);

static void
compile_static_reference(procedure_context& proc, operand static_num, result_register&);

static shared_register
compile_static_reference_to_register(procedure_context& proc, operand static_num) {
  result_register result;
  compile_static_reference(proc, static_num, result);
  return result.get(proc);
}

static void
compile_global_reference(procedure_context& proc, operand global_num, result_register&);

static void
compile_fold(context& ctx, procedure_context& proc, std::vector<std::unique_ptr<expression>> const& arguments,
             opcode op, result_register& result) {
  std::vector<shared_register> arg_registers;
  for (auto const& arg : arguments)
    arg_registers.push_back(compile_expression_to_register(ctx, proc, *arg, false));

  if (!result.result_used())
    return;

  operand previous = *arg_registers.front();
  for (auto operand = arg_registers.begin() + 1; operand != arg_registers.end(); ++operand) {
    encode_instruction(proc.bytecode_stack.back(), instruction{op, previous, **operand, *result.get(proc)});
    previous = *result.get(proc);
  }
}

static void
compile_arithmetic(context& ctx, procedure_context& proc, application_expression const& stx, special_top_level_tag tag,
                   result_register& result) {
  if (stx.arguments.empty()) {
    if (tag == special_top_level_tag::plus) {
      compile_static_reference(proc, ctx.statics.zero, result);
      return;
    }
    else if (tag == special_top_level_tag::times) {
      compile_static_reference(proc, ctx.statics.one, result);
      return;
    }
    else
      throw std::runtime_error{fmt::format("Not enough arguments for {}",
                                           std::get<top_level_reference_expression>(stx.target->value).name)};
  }

  opcode op{};
  switch (tag) {
  case special_top_level_tag::plus:
    op = opcode::add;
    break;
  case special_top_level_tag::minus:
    op = opcode::subtract;
    break;
  case special_top_level_tag::times:
    op = opcode::multiply;
    break;
  case special_top_level_tag::divide:
    op = opcode::divide;
    break;
  default:
    assert(!"Invalid tag");
  }

  compile_fold(ctx, proc, stx.arguments, op, result);
}

static void
compile_relational(context& ctx, procedure_context& proc, application_expression const& stx, special_top_level_tag tag,
                   result_register& result) {
  if (stx.arguments.size() < 2)
    throw std::runtime_error{fmt::format("Not enough arguments for {}",
                                         std::get<local_reference_expression>(stx.target->value).variable->name)};

  opcode op{};
  switch (tag) {
  case special_top_level_tag::arith_equal:      op = opcode::arith_equal; break;
  case special_top_level_tag::less_than:        op = opcode::less; break;
  case special_top_level_tag::greater_than:     op = opcode::greater; break;
  case special_top_level_tag::less_or_equal:    op = opcode::less_or_equal; break;
  case special_top_level_tag::greater_or_equal: op = opcode::greater_or_equal; break;
  default:
    assert(!"Unimplemented");
  }

  compile_fold(ctx, proc, stx.arguments, op, result);
}

static void
compile_vector_set(context& ctx, procedure_context& proc, application_expression const& stx, result_register& result) {
  if (stx.arguments.size() != 3)
    throw std::runtime_error{"vector-set!: Expected exactly 3 arguments"};

  std::array<shared_register, 3> arg_registers;
  for (std::size_t i = 0; i < 3; ++i)
    arg_registers[i] = compile_expression_to_register(ctx, proc, *stx.arguments[i], false);

  encode_instruction(proc.bytecode_stack.back(),
                     instruction{opcode::vector_set, *arg_registers[0], *arg_registers[1], *arg_registers[2]});

  compile_static_reference(proc, ctx.statics.void_, result);
}

static void
compile_vector_ref(context& ctx, procedure_context& proc, application_expression const& stx, result_register& result) {
  if (stx.arguments.size() != 2)
    throw std::runtime_error{"vector-ref: Expected exactly 2 arguments"};

  shared_register v_reg = compile_expression_to_register(ctx, proc, *stx.arguments[0], false);
  shared_register i_reg = compile_expression_to_register(ctx, proc, *stx.arguments[1], false);

  if (result.result_used())
    encode_instruction(proc.bytecode_stack.back(),
                       instruction{opcode::vector_ref, *v_reg, *i_reg, *result.get(proc)});
}

static void
compile_type(context& ctx, procedure_context& proc, application_expression const& stx, result_register& result) {
  if (stx.arguments.size() != 1)
    throw std::runtime_error{"type: Expected exactly 1 argument"};

  shared_register expr_reg = compile_expression_to_register(ctx, proc, *stx.arguments[0], false);
  if (result.result_used())
    encode_instruction(proc.bytecode_stack.back(),
                       instruction{opcode::type, *expr_reg, *result.get(proc)});
}

static void
compile_let(context& ctx, procedure_context& proc, let_expression const& stx, bool tail, result_register& result) {
  variable_bindings::scope scope;
  for (auto const& def : stx.definitions) {
    shared_register value = compile_expression_to_fresh_register(ctx, proc, *def.expression);
    scope.emplace_back(variable_bindings::binding{def.variable, std::move(value)});
  }

  variable_bindings::unique_scope us = proc.bindings.push_scope(std::move(scope));
  compile_sequence(ctx, proc, stx.body, tail, result);
}

static void
compile_local_set(context& ctx, procedure_context& proc, local_set_expression const& stx, result_register& result) {
  shared_register dest = proc.bindings.lookup(stx.target);
  assert(dest);

  shared_register value = compile_expression_to_register(ctx, proc, *stx.expression, false);
  encode_instruction(proc.bytecode_stack.back(), instruction{opcode::set, *value, *dest});

  compile_static_reference(proc, ctx.statics.void_, result);
}

static void
compile_top_level_set(context& ctx, procedure_context& proc, top_level_set_expression const& stx,
                      result_register& result) {
  shared_register value = compile_expression_to_register(ctx, proc, *stx.expression, false);
  encode_instruction(proc.bytecode_stack.back(), instruction{opcode::store_top_level, *value, stx.location});

  compile_static_reference(proc, ctx.statics.void_, result);
}

static ptr<procedure>
make_procedure(context& ctx, procedure_context const& pc, unsigned min_args, bool has_rest,
               std::optional<std::string> name) {
  return make_procedure(ctx, pc.bytecode_stack.back(), pc.registers.locals_used(), min_args, has_rest, std::move(name));
}

static void
compile_lambda(context& ctx, procedure_context& parent, lambda_expression const& stx, result_register& result) {
  procedure_context proc{&parent, parent.module_};
  variable_bindings::scope args_scope;

  for (auto const& free : stx.free_variables)
    args_scope.push_back(variable_bindings::binding{free, proc.registers.allocate_local()});

  for (auto const& param : stx.parameters)
    args_scope.push_back(variable_bindings::binding{param, proc.registers.allocate_local()});

  variable_bindings::unique_scope us = proc.bindings.push_scope(std::move(args_scope));

  result_register body_result;
  compile_sequence(ctx, proc, stx.body, true, body_result);
  if (body_result.has_result())
    encode_instruction(proc.bytecode_stack.back(), instruction{opcode::ret, *body_result.get(proc)});

  assert(proc.bytecode_stack.size() == 1);
  auto p = make_procedure(ctx, proc, static_cast<unsigned>(stx.parameters.size() - (stx.has_rest ? 1 : 0)),
                          stx.has_rest, stx.name);

  if (!stx.free_variables.empty()) {
    shared_register p_reg = compile_static_reference_to_register(parent, ctx.intern_static(p));
    instruction make_closure{opcode::make_closure, *p_reg, *result.get(parent)};
    for (std::shared_ptr<variable> const& var : stx.free_variables)
      make_closure.operands.push_back(*parent.bindings.lookup(var));

    encode_instruction(parent.bytecode_stack.back(), std::move(make_closure));
  }
  else
    compile_static_reference(parent, ctx.intern_static(p), result);
}

static void
compile_if(context& ctx, procedure_context& proc, if_expression const& stx, bool tail, result_register& result) {
  shared_register test_value = compile_expression_to_register(ctx, proc, *stx.test, false);

  proc.bytecode_stack.emplace_back();
  compile_expression(ctx, proc, *stx.consequent, tail, result);
  std::size_t skip_num{};

  if (stx.alternative) {
    // If we got here by executing the then-branch, skip over the
    // else-branch. If the test condition was false, we'll jump right after this
    // jump, skipping it. We'll have to backpatch the actual offset.

    proc.bytecode_stack.emplace_back();
    compile_expression(ctx, proc, *stx.alternative, tail, result);

    bytecode else_bc = std::move(proc.bytecode_stack.back());
    proc.bytecode_stack.pop_back();

    encode_instruction(proc.bytecode_stack.back(), instruction{opcode::jump, to_operand(else_bc.size())});
    skip_num = proc.bytecode_stack.back().size();
    proc.bytecode_stack.back().insert(proc.bytecode_stack.back().end(), else_bc.begin(), else_bc.end());
  } else {
    // No else branch -- we'll set the result to #void. First, if we landed here
    // because we executed the then-branch, we'll have to skip this branch. We
    // need to be mindful that the then-branch might have been compiled into a
    // tail-call and so may not have produced a meaningful result register.

    proc.bytecode_stack.emplace_back();
    compile_static_reference(proc, ctx.statics.void_, result);

    bytecode else_bc = std::move(proc.bytecode_stack.back());
    proc.bytecode_stack.pop_back();

    encode_instruction(proc.bytecode_stack.back(), instruction{opcode::jump, to_operand(else_bc.size())});
    skip_num = proc.bytecode_stack.back().size();
    proc.bytecode_stack.back().insert(proc.bytecode_stack.back().end(), else_bc.begin(), else_bc.end());
  }

  bytecode then_bc = std::move(proc.bytecode_stack.back());
  proc.bytecode_stack.pop_back();
  encode_instruction(proc.bytecode_stack.back(), instruction{opcode::jump_unless, *test_value, to_operand(skip_num)});
  proc.bytecode_stack.back().insert(proc.bytecode_stack.back().end(), then_bc.begin(), then_bc.end());
}

static void
compile_application(context& ctx, procedure_context& proc, application_expression const& stx, bool tail,
                    result_register& result) {
  if (auto* ref = std::get_if<top_level_reference_expression>(&stx.target->value))
    if (std::optional<special_top_level_tag> tag = ctx.find_tag(ref->location)) {
      if ((*tag == special_top_level_tag::plus || *tag == special_top_level_tag::minus
           || *tag == special_top_level_tag::times || *tag == special_top_level_tag::divide)
          && stx.arguments.size() >= 2) {
        compile_arithmetic(ctx, proc, stx, *tag, result);
        return;
      } else if ((*tag == special_top_level_tag::less_than
                  || *tag == special_top_level_tag::greater_than
                  || *tag == special_top_level_tag::less_or_equal
                  || *tag == special_top_level_tag::greater_or_equal
                  || *tag == special_top_level_tag::arith_equal)
                 && stx.arguments.size() == 2){
        compile_relational(ctx, proc, stx, *tag, result);
        return;
      } else if (*tag == special_top_level_tag::vector_set) {
        compile_vector_set(ctx, proc, stx, result);
        return;
      } else if (*tag == special_top_level_tag::vector_ref) {
        compile_vector_ref(ctx, proc, stx, result);
        return;
      } else if (*tag == special_top_level_tag::type) {
        compile_type(ctx, proc, stx, result);
        return;
      }
    }

  opcode oc = tail ? opcode::tail_call : opcode::call;
  operand f;
  shared_register f_reg;

  if (auto* global = std::get_if<top_level_reference_expression>(&stx.target->value)) {
    f = global->location;
    oc = tail ? opcode::tail_call_top_level : opcode::call_top_level;
  } else if (auto* lit = std::get_if<literal_expression>(&stx.target->value)) {
    f = ctx.intern_static(lit->value.get());
    oc = tail ? opcode::tail_call_static : opcode::call_static;
  } else {
    f_reg = compile_expression_to_register(ctx, proc, *stx.target, false);
    f = *f_reg;
  }

  std::vector<shared_register> arg_registers;
  for (auto const& arg : stx.arguments)
    arg_registers.push_back(compile_expression_to_register(ctx, proc, *arg, false));

  instruction instr{oc, f};

  if (!tail)
    instr.operands.push_back(*result.get(proc));

  for (shared_register const& arg : arg_registers)
    instr.operands.push_back(*arg);

  encode_instruction(proc.bytecode_stack.back(), instr);
}

static void
compile_local_reference(procedure_context& proc, local_reference_expression const& stx,
                        result_register& result) {
  if (!result.result_used())
    return;

  shared_register var_reg = proc.bindings.lookup(stx.variable);

  if (!result.has_result() && result.may_alias())
    result.set(var_reg);
  else {
    shared_register result_reg = result.get(proc);
    if (var_reg != result_reg)
      encode_instruction(proc.bytecode_stack.back(), instruction{opcode::set, *var_reg, *result_reg});
  }
}

static void
compile_static_reference(procedure_context& proc, operand location, result_register& result) {
  if (!result.result_used())
    return;

  encode_instruction(proc.bytecode_stack.back(), instruction{opcode::load_static, location, *result.get(proc)});
}

static void
compile_global_reference(procedure_context& proc, operand global_num, result_register& result) {
  if (!result.result_used())
    return;

  encode_instruction(proc.bytecode_stack.back(), instruction{opcode::load_top_level, global_num, *result.get(proc)});
}

static void
compile_global_reference(procedure_context& proc, top_level_reference_expression const& stx, result_register& result) {
  compile_global_reference(proc, stx.location, result);
}

static void
compile_box(context& ctx, procedure_context& proc, box_expression const& stx, result_register& result) {
  shared_register value = compile_expression_to_register(ctx, proc, *stx.expression, false);
  encode_instruction(proc.bytecode_stack.back(), instruction{opcode::box, *value, *result.get(proc)});
}

static void
compile_unbox(context& ctx, procedure_context& proc, unbox_expression const& stx, result_register& result) {
  shared_register box = compile_expression_to_register(ctx, proc, *stx.box_expr, false);
  encode_instruction(proc.bytecode_stack.back(), instruction{opcode::unbox, *box, *result.get(proc)});
}

static void
compile_box_set(context& ctx, procedure_context& proc, box_set_expression const& stx, result_register& result) {
  shared_register box = compile_expression_to_register(ctx, proc, *stx.box_expr, false);
  shared_register value = compile_expression_to_register(ctx, proc, *stx.value_expr, false);
  encode_instruction(proc.bytecode_stack.back(), instruction{opcode::box_set, *box, *value});
  compile_static_reference(proc, ctx.statics.void_, result);
}

static void
compile_cons(context& ctx, procedure_context& proc, cons_expression const& stx, result_register& result) {
  shared_register car = compile_expression_to_register(ctx, proc, *stx.car, false);
  shared_register cdr = compile_expression_to_register(ctx, proc, *stx.cdr, false);
  encode_instruction(proc.bytecode_stack.back(), instruction{opcode::cons, *car, *cdr, *result.get(proc)});
}

static void
compile_make_vector(context& ctx, procedure_context& proc, make_vector_expression const& stx, result_register& result) {
  std::vector<shared_register> exprs;
  exprs.reserve(stx.elements.size());
  for (std::unique_ptr<expression> const& e : stx.elements)
    exprs.push_back(compile_expression_to_register(ctx, proc, *e, false));

  instruction make_vec{opcode::make_vector, *result.get(proc)};
  for (shared_register const& elem : exprs)
    make_vec.operands.push_back(*elem);

  encode_instruction(proc.bytecode_stack.back(), make_vec);
}

// Translate an expression and return the register where the result is stored.
static void
compile_expression(context& ctx, procedure_context& proc, expression const& stx, bool tail, result_register& result) {
  if (auto* lit = std::get_if<literal_expression>(&stx.value))
    compile_static_reference(proc, ctx.intern_static(lit->value.get()), result);
  else if (auto* local_ref = std::get_if<local_reference_expression>(&stx.value))
    compile_local_reference(proc, *local_ref, result);
  else if (auto* top_level_ref = std::get_if<top_level_reference_expression>(&stx.value))
    compile_global_reference(proc, *top_level_ref, result);
  else if (auto* app = std::get_if<application_expression>(&stx.value))
    compile_application(ctx, proc, *app, tail, result);
  else if (auto* let = std::get_if<let_expression>(&stx.value))
    compile_let(ctx, proc, *let, tail, result);
  else if (auto* local_set = std::get_if<local_set_expression>(&stx.value))
    compile_local_set(ctx, proc, *local_set, result);
  else if (auto* top_level_set = std::get_if<top_level_set_expression>(&stx.value))
    compile_top_level_set(ctx, proc, *top_level_set, result);
  else if (auto* lambda = std::get_if<lambda_expression>(&stx.value))
    compile_lambda(ctx, proc, *lambda, result);
  else if (auto* if_ = std::get_if<if_expression>(&stx.value))
    compile_if(ctx, proc, *if_, tail, result);
  else if (auto* box = std::get_if<box_expression>(&stx.value))
    compile_box(ctx, proc, *box, result);
  else if (auto* unbox = std::get_if<unbox_expression>(&stx.value))
    compile_unbox(ctx, proc, *unbox, result);
  else if (auto* box_set = std::get_if<box_set_expression>(&stx.value))
    compile_box_set(ctx, proc, *box_set, result);
  else if (auto* cons = std::get_if<cons_expression>(&stx.value))
    compile_cons(ctx, proc, *cons, result);
  else if (auto* make_vector = std::get_if<make_vector_expression>(&stx.value))
    compile_make_vector(ctx, proc, *make_vector, result);
  else if (auto* sequence = std::get_if<sequence_expression>(&stx.value))
    compile_sequence(ctx, proc, *sequence, tail, result);
  else
    assert(!"Unexpected expression");
}

static void
compile_sequence(context& ctx, procedure_context& proc, sequence_expression const& stx, bool tail,
                 result_register& result) {
  if (stx.expressions.empty()) {
    compile_static_reference(proc, ctx.statics.void_, result);
  }

  for (auto expr = stx.expressions.begin(); expr != stx.expressions.end(); ++expr) {
    bool last = std::next(expr) == stx.expressions.end();
    if (last)
      compile_expression(ctx, proc, **expr, tail && last, result);
    else {
      result_register subresult{false};
      compile_expression(ctx, proc, **expr, false, subresult);
    }
  }
}

ptr<procedure>
compile_expression(context& ctx, ptr<syntax> datum, module_& mod, source_file_origin const& origin) {
  return compile_syntax(ctx, analyse(ctx, datum, mod, origin), mod);
}

ptr<procedure>
compile_syntax(context& ctx, std::unique_ptr<expression> e, module_& mod) {
  procedure_context proc{nullptr, mod};
  shared_register result = compile_expression_to_register(ctx, proc, *e, true);
  if (result)
    encode_instruction(proc.bytecode_stack.back(), instruction{opcode::ret, *result});

  assert(proc.bytecode_stack.size() == 1);
  return make_procedure(ctx, proc, 0, false, std::nullopt);
}

module_
compile_module(context& ctx, std::vector<ptr<syntax>> const& data, source_file_origin const& origin,
               bool main_module) {
  simple_action a(ctx, "Analysing main module");
  protomodule pm = read_module(ctx, data, origin);
  module_ result{ctx};
  perform_imports(ctx, result, pm);
  compile_module_body(ctx, result, pm, main_module);
  return result;
}

module_
compile_module(context& ctx, std::filesystem::path const& path, bool main_module) {
  filesystem_source_code_provider provider{"."};
  if (auto file = provider.find_file(ctx, path))
    return compile_module(ctx, insider::read_syntax_multiple(ctx, file->port.get().get()), file->origin, main_module);
  else
    throw std::runtime_error{fmt::format("Can't open input file {}", path.string())};
}

void
compile_module_body(context& ctx, module_& m, protomodule const& pm, bool main_module) {
  sequence_expression body = analyse_module(ctx, m, pm, main_module);

  procedure_context proc{nullptr, m};
  result_register result;
  compile_sequence(ctx, proc, body, true, result);
  if (result.has_result())
    encode_instruction(proc.bytecode_stack.back(), instruction{opcode::ret, *result.get(proc)});

  assert(proc.bytecode_stack.size() == 1);
  m.set_top_level_procedure(make_procedure(ctx, proc, 0, false, std::nullopt));
}

} // namespace insider
