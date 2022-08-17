#include "compiler.hpp"

#include "compiler/analyser.hpp"
#include "compiler/ast.hpp"
#include "compiler/source_code_provider.hpp"
#include "compiler/variable.hpp"
#include "io/read.hpp"
#include "runtime/basic_types.hpp"

#include <fmt/format.h>

#include <limits>
#include <memory>
#include <optional>
#include <ranges>
#include <stdexcept>
#include <type_traits>
#include <unordered_map>
#include <utility>

namespace insider {

namespace {
  class shared_local;

  class locals_allocator {
  public:
    shared_local
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

  class shared_local {
  public:
    shared_local() = default;
    shared_local(locals_allocator&, operand);
    shared_local(shared_local const&);
    shared_local(shared_local&&) noexcept;
    ~shared_local();

    shared_local&
    operator = (shared_local&&) noexcept;

    operand
    operator * () const { assert(has_value()); return *value_; }

    bool
    has_value() const { return (bool) value_; }

    explicit operator
    bool () const { return has_value(); }

    bool
    operator == (shared_local const& other) const {
      return alloc_ == other.alloc_ && value_ == other.value_;
    }

    bool
    operator != (shared_local const& other) const {
      return !operator == (other);
    }

  private:
    locals_allocator*      alloc_ = nullptr;
    std::optional<operand> value_;
  };

  class variable_bindings {
  public:
    struct binding {
      ptr<local_variable> variable;
      shared_local        destination;

      binding() = default;

      binding(ptr<insider::local_variable> variable, shared_local destination)
        : variable{variable}
        , destination{std::move(destination)}
      { }
    };
    using scope = std::vector<binding>;
    using free_variables_map = std::unordered_map<ptr<local_variable>,
                                                  shared_local>;

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

    shared_local
    lookup(ptr<local_variable> const&) const;

    bool
    local_is_store_for_variable(shared_local const&) const;

  private:
    std::vector<scope> scopes_;
  };

  struct bytecode_and_debug_info {
    bytecode       bc;
    debug_info_map debug_info;
  };

  struct procedure_context {
    procedure_context*                   parent = nullptr;
    locals_allocator                     registers;
    variable_bindings                    bindings;
    // Stack of bytecodes to facilitate compiling ifs.
    std::vector<bytecode_and_debug_info> bytecode_stack;
    tracked_ptr<insider::module_>        module_;

    procedure_context(procedure_context* parent,
                      tracked_ptr<insider::module_> m)
      : parent{parent}
      , module_{std::move(m)}
    {
      bytecode_stack.emplace_back();
    }
  };
}

shared_local
locals_allocator::allocate_local() {
  if (locals_free_.empty())
    return {*this, operand{locals_max_++}};
  else {
    auto result = operand{locals_free_.back()};
    locals_free_.pop_back();
    return {*this, result};
  }
}

void
locals_allocator::ref(operand o) {
  ++local_ref_counts_[o];
}

void
locals_allocator::unref(operand o) {
  auto ref_count = local_ref_counts_.find(o);
  assert(ref_count != local_ref_counts_.end());

  if (--ref_count->second == 0) {
    locals_free_.push_back(o);
    local_ref_counts_.erase(ref_count);
  }
}

shared_local::shared_local(locals_allocator& alloc, operand value)
  : alloc_{&alloc}
  , value_{value}
{
  alloc_->ref(*value_);
}

shared_local::shared_local(shared_local const& other)
  : alloc_{other.alloc_}
  , value_{other.value_}
{
  if (alloc_)
    alloc_->ref(*value_);
}

shared_local::shared_local(shared_local&& other) noexcept
  : alloc_{other.alloc_}
  , value_{other.value_}
{
  other.alloc_ = nullptr;
}

shared_local::~shared_local() {
  if (alloc_)
    alloc_->unref(*value_);
}

shared_local&
shared_local::operator = (shared_local&& other) noexcept {
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

shared_local
variable_bindings::lookup(ptr<local_variable> const& v) const {
  for (auto const& scope : scopes_ | std::views::reverse)
    for (binding const& b : scope)
      if (b.variable == v)
        return b.destination;

  assert(false); // TODO: This shouldn't ever happen any longer.
  return {};
}

bool
variable_bindings::local_is_store_for_variable(shared_local const& loc) const {
  for (auto const& scope : scopes_)
    for (binding const& b : scope)
      if (b.destination == loc)
        return true;
  return false;
}

namespace {
  // Caller can either not be interested in any result, or it can want the
  // result in any register, or it can want the result in a specific
  // register. The callee on the other hand may or may not produce a result at
  // all.
  class result_location {
  public:
    explicit
    result_location(bool result_used = true)
      : used_{result_used}
    { }

    bool
    result_used() const { return used_; }

    bool
    has_result() const { return reg_.has_value(); }

    shared_local
    get(procedure_context&);

    void
    set(shared_local reg) {
      assert(!reg_);
      reg_ = std::move(reg);
    }

  private:
    shared_local reg_;
    bool         used_;
  };
}

shared_local
result_location::get(procedure_context& proc) {
  if (!reg_)
    reg_ = proc.registers.allocate_local();

  return reg_;
}

template <typename T>
operand
to_operand(T value) {
  static_assert(!std::is_signed_v<T>);
  if (value > std::numeric_limits<operand>::max())
    throw std::runtime_error{
      fmt::format("Implementation limit exceeded. Cannot encode {} as an "
                  "instruction operand",
                  value)
    };

  return operand(value);
}

static void
compile_expression(context& ctx, procedure_context& proc, expression,
                   bool tail, result_location&);

static shared_local
compile_expression_to_register(context& ctx, procedure_context& proc,
                               expression const& stx, bool tail) {
  result_location result;
  compile_expression(ctx, proc, stx, tail, result);
  return result.has_result() ? result.get(proc) : shared_local{};
}

static void
compile_expression(context& ctx, procedure_context& proc,
                   ptr <sequence_expression>, bool tail, result_location&);

static void
compile_expression(context&, procedure_context& proc,
                   ptr<local_reference_expression> stx, bool tail,
                   result_location& result);

static void
compile_static_reference(procedure_context& proc, operand location,
                         result_location&);

static shared_local
compile_static_reference_to_register(procedure_context& proc,
                                     operand static_num) {
  result_location result;
  compile_static_reference(proc, static_num, result);
  return result.get(proc);
}

static void
compile_global_reference(procedure_context& proc, operand global_num,
                         result_location&);

static void
compile_fold(context& ctx, procedure_context& proc,
             std::vector<expression> const& arguments,
             opcode op, result_location& result) {
  std::vector<shared_local> arg_registers;
  arg_registers.reserve(arguments.size());
  for (expression arg : arguments)
    arg_registers.push_back(
      compile_expression_to_register(ctx, proc, arg, false)
    );

  if (!result.result_used())
    return;

  operand previous = *arg_registers.front();
  for (auto operand = arg_registers.begin() + 1;
       operand != arg_registers.end();
       ++operand) {
    encode_instruction(proc.bytecode_stack.back().bc,
                       instruction{op, previous, **operand, *result.get(proc)});
    previous = *result.get(proc);
  }
}

static void
compile_arithmetic(context& ctx, procedure_context& proc,
                   ptr<application_expression> stx, special_top_level_tag tag,
                   result_location& result) {
  if (stx->arguments().empty()) {
    if (tag == special_top_level_tag::plus) {
      compile_static_reference(proc, ctx.statics.zero, result);
      return;
    }
    else if (tag == special_top_level_tag::times) {
      compile_static_reference(proc, ctx.statics.one, result);
      return;
    }
    else
      throw std::runtime_error{fmt::format(
        "Not enough arguments for {}",
        assume<top_level_reference_expression>(
          stx->target())->variable()->name()
        )
      };
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

  compile_fold(ctx, proc, stx->arguments(), op, result);
}

static void
compile_relational(context& ctx, procedure_context& proc,
                   ptr<application_expression> stx, special_top_level_tag tag,
                   result_location& result) {
  if (stx->arguments().size() < 2)
    throw std::runtime_error{
      fmt::format(
        "Not enough arguments for {}",
        assume<local_reference_expression>(stx->target())->variable()->name()
      )
    };

  opcode op{};
  switch (tag) {
  case special_top_level_tag::arith_equal:
    op = opcode::arith_equal;
    break;
  case special_top_level_tag::less_than:
    op = opcode::less;
    break;
  case special_top_level_tag::greater_than:
    op = opcode::greater;
    break;
  case special_top_level_tag::less_or_equal:
    op = opcode::less_or_equal;
    break;
  case special_top_level_tag::greater_or_equal:
    op = opcode::greater_or_equal;
    break;
  default:
    assert(!"Unimplemented");
  }

  compile_fold(ctx, proc, stx->arguments(), op, result);
}

static void
compile_vector_set(context& ctx, procedure_context& proc,
                   ptr<application_expression> stx, result_location& result) {
  if (stx->arguments().size() != 3)
    throw std::runtime_error{"vector-set!: Expected exactly 3 arguments"};

  std::array<shared_local, 3> arg_registers;
  for (std::size_t i = 0; i < 3; ++i)
    arg_registers[i]
      = compile_expression_to_register(ctx, proc, stx->arguments()[i], false);

  encode_instruction(proc.bytecode_stack.back().bc,
                     instruction{opcode::vector_set,
                                 *arg_registers[0],
                                 *arg_registers[1],
                                 *arg_registers[2]});

  compile_static_reference(proc, ctx.statics.void_, result);
}

static void
compile_vector_ref(context& ctx, procedure_context& proc,
                   ptr<application_expression> stx, result_location& result) {
  if (stx->arguments().size() != 2)
    throw std::runtime_error{"vector-ref: Expected exactly 2 arguments"};

  shared_local v_reg
    = compile_expression_to_register(ctx, proc, stx->arguments()[0], false);
  shared_local i_reg
    = compile_expression_to_register(ctx, proc, stx->arguments()[1], false);

  if (result.result_used())
    encode_instruction(proc.bytecode_stack.back().bc,
                       instruction{opcode::vector_ref, *v_reg, *i_reg,
                                   *result.get(proc)});
}

static void
compile_type(context& ctx, procedure_context& proc,
             ptr<application_expression> stx, result_location& result) {
  if (stx->arguments().size() != 1)
    throw std::runtime_error{"type: Expected exactly 1 argument"};

  shared_local expr_reg
    = compile_expression_to_register(ctx, proc, stx->arguments()[0], false);
  if (result.result_used())
    encode_instruction(proc.bytecode_stack.back().bc,
                       instruction{opcode::type, *expr_reg, *result.get(proc)});
}

static void
compile_cons_application(context& ctx, procedure_context& proc,
                         ptr<application_expression> stx,
                         result_location& result) {
  if (stx->arguments().size() != 2)
    throw std::runtime_error{"cons: Expected exactly 2 arguments"};

  shared_local car_reg
    = compile_expression_to_register(ctx, proc, stx->arguments()[0], false);
  shared_local cdr_reg
    = compile_expression_to_register(ctx, proc, stx->arguments()[1], false);

  if (result.result_used())
    encode_instruction(proc.bytecode_stack.back().bc,
                       instruction{opcode::cons,
                                   *car_reg, *cdr_reg,
                                   *result.get(proc)});
}

static void
compile_car(context& ctx, procedure_context& proc,
            ptr<application_expression> stx,
            result_location& result) {
  if (stx->arguments().size() != 1)
    throw std::runtime_error{"car: Expected exactly 1 argument"};

  shared_local pair_reg
    = compile_expression_to_register(ctx, proc, stx->arguments()[0], false);

  if (result.result_used())
    encode_instruction(proc.bytecode_stack.back().bc,
                       instruction{opcode::car, *pair_reg, *result.get(proc)});
}

static void
compile_cdr(context& ctx, procedure_context& proc,
            ptr<application_expression> stx,
            result_location& result) {
  if (stx->arguments().size() != 1)
    throw std::runtime_error{"cdr: Expected exactly 1 argument"};

  shared_local pair_reg
    = compile_expression_to_register(ctx, proc, stx->arguments()[0], false);

  if (result.result_used())
    encode_instruction(proc.bytecode_stack.back().bc,
                       instruction{opcode::cdr, *pair_reg, *result.get(proc)});
}

static void
compile_eq(context& ctx, procedure_context& proc,
           ptr<application_expression> stx,
           result_location& result) {
  if (stx->arguments().size() != 2)
    throw std::runtime_error{"eq?: Expected exactly 2 arguments"};

  shared_local lhs_reg
    = compile_expression_to_register(ctx, proc, stx->arguments()[0], false);
  shared_local rhs_reg
    = compile_expression_to_register(ctx, proc, stx->arguments()[1], false);

  if (result.result_used())
    encode_instruction(proc.bytecode_stack.back().bc,
                       instruction{opcode::eq,
                                   *lhs_reg, *rhs_reg,
                                   *result.get(proc)});
}

static void
compile_box(context& ctx, procedure_context& proc,
            ptr<application_expression> stx,
            result_location& result) {
  if (stx->arguments().size() != 1)
    throw std::runtime_error{"box: Expected exactly 1 argument"};

  shared_local value
    = compile_expression_to_register(ctx, proc, stx->arguments()[0], false);
  if (result.result_used())
    encode_instruction(proc.bytecode_stack.back().bc,
                       instruction{opcode::box, *value, *result.get(proc)});
}

static void
compile_unbox(context& ctx, procedure_context& proc,
              ptr<application_expression> stx,
              result_location& result) {
  if (stx->arguments().size() != 1)
    throw std::runtime_error{"unbox: Expected exactly 1 argument"};

  shared_local box
    = compile_expression_to_register(ctx, proc, stx->arguments()[0], false);
  if (result.result_used())
    encode_instruction(proc.bytecode_stack.back().bc,
                       instruction{opcode::unbox, *box, *result.get(proc)});
}

static void
compile_box_set(context& ctx, procedure_context& proc,
                ptr<application_expression> stx,
                result_location& result) {
  if (stx->arguments().size() != 2)
    throw std::runtime_error{"box-set!: Expected exactly 2 argument"};

  shared_local box
    = compile_expression_to_register(ctx, proc, stx->arguments()[0], false);
  shared_local value
    = compile_expression_to_register(ctx, proc, stx->arguments()[1], false);

  encode_instruction(proc.bytecode_stack.back().bc,
                     instruction{opcode::box_set, *box, *value});
  compile_static_reference(proc, ctx.statics.void_, result);
}

static void
compile_expression(context& ctx, procedure_context& proc,
                   ptr<let_expression> stx, bool tail,
                   result_location& result) {
  variable_bindings::scope scope;
  for (auto const& def : stx->definitions()) {
    shared_local value
      = compile_expression_to_register(ctx, proc, def.expression(), false);
    scope.emplace_back(variable_bindings::binding{def.variable(),
                                                  std::move(value)});
  }

  variable_bindings::unique_scope us
    = proc.bindings.push_scope(std::move(scope));
  compile_expression(ctx, proc, stx->body(), tail, result);
}

static void
compile_expression(context& ctx, procedure_context& proc,
                   ptr<local_set_expression> stx, bool,
                   result_location& result) {
  result_location dest_loc;
  dest_loc.set(proc.bindings.lookup(stx->target()));
  compile_expression(ctx, proc, stx->expression(), false, dest_loc);
  compile_static_reference(proc, ctx.statics.void_, result);
}

static void
compile_expression(context& ctx, procedure_context& proc,
                   ptr<top_level_set_expression> stx, bool,
                   result_location& result) {
  shared_local value
    = compile_expression_to_register(ctx, proc, stx->expression(), false);
  encode_instruction(proc.bytecode_stack.back().bc,
                     instruction{opcode::store_top_level, *value,
                                 stx->target()->index});

  compile_static_reference(proc, ctx.statics.void_, result);
}

static void
append_bytecode(bytecode& to_bc,
                debug_info_map& to_di,
                bytecode_and_debug_info const& from) {
  for (std::size_t i = 0; i < from.bc.size(); ++i) {
    std::size_t to_index = to_bc.size();
    to_bc.push_back(from.bc[i]);

    if (auto di = from.debug_info.find(i); di != from.debug_info.end())
      to_di.emplace(to_index, di->second);
  }
}

static void
append_bytecode(bytecode_and_debug_info& to,
                bytecode_and_debug_info const& from) {
  append_bytecode(to.bc, to.debug_info, from);
}

static ptr<procedure>
make_procedure_from_bytecode(context& ctx, procedure_context const& pc,
                             unsigned min_args, bool has_rest,
                             std::string name) {
  std::size_t entry = ctx.program.size();

  assert(pc.bytecode_stack.size() == 1);
  append_bytecode(ctx.program, ctx.program_debug_info,
                  pc.bytecode_stack.back());

  return make<procedure>(ctx, entry, pc.bytecode_stack.back().bc.size(),
                         pc.registers.locals_used(), min_args, has_rest,
                         std::move(name));
}

static ptr<closure>
make_closure_from_bytecode(context& ctx, procedure_context const& pc,
                           unsigned min_args, bool has_rest,
                           std::string name) {
  return make_empty_closure(ctx, make_procedure_from_bytecode(ctx, pc, min_args,
                                                              has_rest,
                                                              std::move(name)));
}

static operand
nth_parameter_index(std::size_t n, std::size_t total_args) {
  return static_cast<operand>(
    static_cast<operand>(n)
      - static_cast<operand>(total_args)
      - static_cast<operand>(call_stack::stack_frame_header_size)
  );
}

static variable_bindings::unique_scope
push_parameters_and_closure_scope(procedure_context& proc,
                                  ptr<lambda_expression> stx) {
  variable_bindings::scope args_scope;

  for (ptr<local_variable> free : stx->free_variables())
    args_scope.push_back(variable_bindings::binding{
        free, proc.registers.allocate_local()
      });

  for (std::size_t i = 0; i < stx->parameters().size(); ++i) {
    auto param = stx->parameters()[i];
    args_scope.push_back(variable_bindings::binding{
        param,
        shared_local{proc.registers,
          nth_parameter_index(i, stx->parameters().size())}
      });
  }

  return proc.bindings.push_scope(std::move(args_scope));
}

static void
compile_lambda_body(context& ctx, procedure_context& proc,
                    ptr<lambda_expression> stx) {
  result_location body_result;
  compile_expression(ctx, proc, stx->body(), true, body_result);
  if (body_result.has_result())
    encode_instruction(proc.bytecode_stack.back().bc,
                       instruction{opcode::ret, *body_result.get(proc)});
}

static void
emit_make_closure(context& ctx, procedure_context& parent,
                  ptr<procedure> proc, ptr<lambda_expression> stx,
                  result_location& result) {
  shared_local p_reg
    = compile_static_reference_to_register(parent, ctx.intern_static(proc));

  for (ptr<local_variable> var : stx->free_variables())
    encode_instruction(
      parent.bytecode_stack.back().bc,
      instruction{opcode::push, *parent.bindings.lookup(var)}
    );

  encode_instruction(
    parent.bytecode_stack.back().bc,
    instruction{opcode::make_closure,
                *p_reg,
                static_cast<operand>(stx->free_variables().size()),
                *result.get(parent)}
  );
}

static void
compile_expression(context& ctx, procedure_context& parent,
                   ptr<lambda_expression> stx, bool, result_location& result) {
  procedure_context proc{&parent, parent.module_};

  auto us = push_parameters_and_closure_scope(proc, stx);
  compile_lambda_body(ctx, proc, stx);

  assert(proc.bytecode_stack.size() == 1);
  auto p = make_procedure_from_bytecode(
    ctx, proc,
    static_cast<unsigned>(stx->parameters().size() - (stx->has_rest() ? 1 : 0)),
    stx->has_rest(), stx->name()
  );

  emit_make_closure(ctx, parent, p, stx, result);
}

static void
compile_if_branches_to_specified_register(context& ctx, procedure_context& proc,
                                          ptr<if_expression> stx, bool tail,
                                          result_location& result) {
  proc.bytecode_stack.emplace_back();
  compile_expression(ctx, proc, stx->consequent(), tail, result);

  proc.bytecode_stack.emplace_back();
  compile_expression(ctx, proc, stx->alternative(), tail, result);
}

static void
unify_if_branch_results(procedure_context& proc,
                        shared_local const& consequent_reg,
                        shared_local const& alternative_reg,
                        bytecode& consequent_bc,
                        bytecode& alternative_bc,
                        result_location& result) {
  if (!consequent_reg && !alternative_reg)
    return;
  else if (!consequent_reg && alternative_reg)
    result.set(alternative_reg);
  else if ((consequent_reg && !alternative_reg)
           || consequent_reg == alternative_reg)
    result.set(consequent_reg);
  else {
    assert(proc.bindings.local_is_store_for_variable(consequent_reg));
    if (!proc.bindings.local_is_store_for_variable(alternative_reg)) {
      encode_instruction(
        consequent_bc,
        instruction{opcode::set, *consequent_reg, *alternative_reg}
      );
      result.set(alternative_reg);
    } else {
      shared_local new_reg = proc.registers.allocate_local();
      encode_instruction(
        consequent_bc,
        instruction{opcode::set, *consequent_reg, *new_reg}
      );
      encode_instruction(
        alternative_bc,
        instruction{opcode::set, *alternative_reg, *new_reg}
      );
      result.set(new_reg);
    }
  }
}

static void
compile_if_branches_to_unspecified_register(context& ctx,
                                            procedure_context& proc,
                                            ptr<if_expression> stx,
                                            bool tail,
                                            result_location& result) {
  proc.bytecode_stack.emplace_back();
  shared_local consequent_reg
    = compile_expression_to_register(ctx, proc, stx->consequent(), tail);

  if (consequent_reg &&
      !proc.bindings.local_is_store_for_variable(consequent_reg)) {
    result.set(consequent_reg);

    proc.bytecode_stack.emplace_back();
    compile_expression(ctx, proc, stx->alternative(), tail, result);
  } else {
    proc.bytecode_stack.emplace_back();
    shared_local alternative_reg
      = compile_expression_to_register(ctx, proc, stx->alternative(), tail);
    unify_if_branch_results(
      proc,
      consequent_reg,
      alternative_reg,
      proc.bytecode_stack[proc.bytecode_stack.size() - 2].bc,
      proc.bytecode_stack[proc.bytecode_stack.size() - 1].bc,
      result
    );
  }
}

static void
compile_expression(context& ctx, procedure_context& proc,
                   ptr<if_expression> stx, bool tail, result_location& result) {
  shared_local test_value
    = compile_expression_to_register(ctx, proc, stx->test(), false);

  if (result.has_result() || !result.result_used())
    compile_if_branches_to_specified_register(ctx, proc, stx, tail, result);
  else
    compile_if_branches_to_unspecified_register(ctx, proc, stx, tail, result);

  bytecode_and_debug_info else_bc = std::move(proc.bytecode_stack.back());
  proc.bytecode_stack.pop_back();

  encode_instruction(proc.bytecode_stack.back().bc,
                     instruction{opcode::jump, to_operand(else_bc.bc.size())});
  std::size_t skip_num = proc.bytecode_stack.back().bc.size();
  append_bytecode(proc.bytecode_stack.back(), else_bc);

  bytecode_and_debug_info then_bc = std::move(proc.bytecode_stack.back());
  proc.bytecode_stack.pop_back();
  encode_instruction(proc.bytecode_stack.back().bc,
                     instruction{opcode::jump_unless,
                                 *test_value,
                                 to_operand(skip_num)});
  append_bytecode(proc.bytecode_stack.back(), then_bc);
}

static void
compile_expression(context& ctx, procedure_context& proc,
                   ptr<application_expression> stx, bool tail,
                   result_location& result) {
  if (auto ref = match<top_level_reference_expression>(stx->target()))
    if (std::optional<special_top_level_tag> tag
        = ctx.find_tag(ref->variable()->index)) {
      if ((*tag == special_top_level_tag::plus
           || *tag == special_top_level_tag::minus
           || *tag == special_top_level_tag::times
           || *tag == special_top_level_tag::divide)
          && stx->arguments().size() >= 2) {
        compile_arithmetic(ctx, proc, stx, *tag, result);
        return;
      } else if ((*tag == special_top_level_tag::less_than
                  || *tag == special_top_level_tag::greater_than
                  || *tag == special_top_level_tag::less_or_equal
                  || *tag == special_top_level_tag::greater_or_equal
                  || *tag == special_top_level_tag::arith_equal)
                 && stx->arguments().size() == 2){
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
      } else if (*tag == special_top_level_tag::cons) {
        compile_cons_application(ctx, proc, stx, result);
        return;
      } else if (*tag == special_top_level_tag::car) {
        compile_car(ctx, proc, stx, result);
        return;
      } else if (*tag == special_top_level_tag::cdr) {
        compile_cdr(ctx, proc, stx, result);
        return;
      } else if (*tag == special_top_level_tag::eq) {
        compile_eq(ctx, proc, stx, result);
        return;
      } else if (*tag == special_top_level_tag::box) {
        compile_box(ctx, proc, stx, result);
        return;
      } else if (*tag == special_top_level_tag::unbox) {
        compile_unbox(ctx, proc, stx, result);
        return;
      } else if (*tag == special_top_level_tag::box_set) {
        compile_box_set(ctx, proc, stx, result);
        return;
      }
    }

  opcode oc = tail ? opcode::tail_call : opcode::call;
  operand f;
  shared_local f_reg;

  for (expression arg : stx->arguments()) {
    shared_local reg = compile_expression_to_register(ctx, proc, arg, false);
    encode_instruction(proc.bytecode_stack.back().bc,
                       instruction{opcode::push, *reg});
  }

  if (auto global = match<top_level_reference_expression>(stx->target())) {
    f = global->variable()->index;
    oc = tail ? opcode::tail_call_top_level : opcode::call_top_level;
  } else if (auto lit = match<literal_expression>(stx->target())) {
    f = ctx.intern_static(lit->value());
    oc = tail ? opcode::tail_call_static : opcode::call_static;
  } else {
    f_reg = compile_expression_to_register(ctx, proc, stx->target(), false);
    f = *f_reg;
  }

  std::size_t call_idx = encode_instruction(
    proc.bytecode_stack.back().bc,
    instruction{oc, f,
                static_cast<operand>(stx->arguments().size())}
  );
  if (stx->debug_info())
    proc.bytecode_stack.back().debug_info[call_idx] = *stx->debug_info();

  if (!tail)
    encode_instruction(proc.bytecode_stack.back().bc,
                       instruction{opcode::pop, *result.get(proc)});
}

static void
compile_expression(context&, procedure_context& proc,
                  ptr<local_reference_expression> stx, bool,
                  result_location& result) {
  if (!result.result_used())
    return;

  shared_local var_reg = proc.bindings.lookup(stx->variable());

  if (!result.has_result())
    result.set(var_reg);
  else {
    shared_local result_reg = result.get(proc);
    if (var_reg != result_reg)
      encode_instruction(proc.bytecode_stack.back().bc,
                         instruction{opcode::set, *var_reg, *result_reg});
  }
}

static void
compile_static_reference(procedure_context& proc, operand location,
                         result_location& result) {
  if (!result.result_used())
    return;

  encode_instruction(proc.bytecode_stack.back().bc,
                     instruction{opcode::load_static,
                                 location,
                                 *result.get(proc)});
}

static void
compile_global_reference(procedure_context& proc, operand global_num,
                         result_location& result) {
  if (!result.result_used())
    return;

  encode_instruction(proc.bytecode_stack.back().bc,
                     instruction{opcode::load_top_level, global_num,
                                 *result.get(proc)});
}

static void
compile_expression(context&, procedure_context& proc,
                   ptr<top_level_reference_expression> stx, bool,
                   result_location& result) {
  compile_global_reference(proc, stx->variable()->index, result);
}

static void
compile_expression(context& ctx, procedure_context& proc,
                   ptr<unknown_reference_expression> stx, bool,
                   result_location& result) {
  if (!result.result_used())
    return;

  operand id_index = ctx.intern_static(stx->name());
  encode_instruction(proc.bytecode_stack.back().bc,
                     instruction{opcode::load_dynamic_top_level,
                                 id_index,
                                 *result.get(proc)});
}

static void
compile_expression(context& ctx, procedure_context& proc,
                   ptr<literal_expression> lit, bool, result_location& result) {
  compile_static_reference(proc, ctx.intern_static(lit->value()), result);
}

// Translate an expression and return the register where the result is stored.
static void
compile_expression(context& ctx, procedure_context& proc, expression stx,
                   bool tail, result_location& result) {
  visit([&] (auto e) { compile_expression(ctx, proc, e, tail, result); },
        stx);
}

static void
compile_expression(context& ctx, procedure_context& proc,
                   ptr<sequence_expression> stx, bool tail,
                   result_location& result) {
  if (stx->expressions().empty())
    compile_static_reference(proc, ctx.statics.void_, result);

  for (auto expr = stx->expressions().begin();
       expr != stx->expressions().end();
       ++expr) {
    bool last = std::next(expr) == stx->expressions().end();
    if (last)
      compile_expression(ctx, proc, *expr, tail && last, result);
    else {
      result_location subresult{false};
      compile_expression(ctx, proc, *expr, false, subresult);
    }
  }
}

ptr<closure>
compile_expression(context& ctx, ptr<syntax> datum,
                   tracked_ptr<module_> const& mod,
                   source_file_origin const& origin,
                   pass_list passes) {
  return compile_syntax(ctx,
                        analyse(ctx, datum, mod, std::move(passes), origin),
                        mod);
}

ptr<closure>
compile_syntax(context& ctx, expression e, tracked_ptr<module_> const& mod) {
  procedure_context proc{nullptr, mod};
  shared_local result = compile_expression_to_register(ctx, proc, e, true);
  if (result)
    encode_instruction(proc.bytecode_stack.back().bc,
                       instruction{opcode::ret, *result});

  assert(proc.bytecode_stack.size() == 1);
  return make_empty_closure(
    ctx, make_procedure_from_bytecode(ctx, proc, 0, false, "<expression>")
  );
}

tracked_ptr<module_>
compile_module(context& ctx, std::vector<ptr<syntax>> const& data,
               source_file_origin const& origin, pass_list passes,
               bool main_module) {
  module_specifier pm = read_module(ctx, data, origin);
  auto result = make_tracked<module_>(ctx, ctx, pm.name);
  perform_imports(ctx, result, pm.imports);
  compile_module_body(ctx, result, pm, std::move(passes), main_module);
  return result;
}

tracked_ptr<module_>
compile_module(context& ctx, std::filesystem::path const& path,
               pass_list passes, bool main_module) {
  filesystem_source_code_provider provider{"."};
  if (auto file = provider.find_file(ctx, path))
    return compile_module(ctx,
                          insider::read_syntax_multiple(ctx,
                                                        file->port.get().get()),
                          file->origin,
                          std::move(passes),
                          main_module);
  else
    throw std::runtime_error{fmt::format("Can't open input file {}",
                                         path.string())};
}

void
compile_module_body(context& ctx, tracked_ptr<module_> const& m,
                    module_specifier const& pm, pass_list passes,
                    bool main_module) {
  auto body_expr = analyse_module(ctx, m, pm, std::move(passes), main_module);

  procedure_context proc{nullptr, m};
  result_location result;
  compile_expression(ctx, proc, body_expr, true, result);
  if (result.has_result())
    encode_instruction(proc.bytecode_stack.back().bc,
                       instruction{opcode::ret, *result.get(proc)});

  m->set_top_level_procedure(
    ctx.store,
    make_closure_from_bytecode(ctx, proc, 0, false,
                               fmt::format("<module {} top-level>",
                                           pm.name
                                           ? module_name_to_string(*pm.name)
                                           : "<unknown>"))
  );
}

} // namespace insider
