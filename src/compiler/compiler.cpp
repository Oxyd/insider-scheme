#include "compiler.hpp"

#include "compiler/analyser.hpp"
#include "compiler/ast.hpp"
#include "compiler/cfg.hpp"
#include "compiler/registers.hpp"
#include "compiler/source_code_provider.hpp"
#include "compiler/variable.hpp"
#include "io/read.hpp"
#include "runtime/basic_types.hpp"

#include <fmt/format.h>

#include <concepts>
#include <limits>
#include <memory>
#include <optional>
#include <ranges>
#include <set>
#include <stdexcept>
#include <type_traits>
#include <unordered_map>
#include <utility>

namespace insider {

namespace {
  struct procedure_context {
    procedure_context*                            parent = nullptr;
    register_allocator                            registers;
    variable_bindings                             bindings;
    insider::cfg                                  cfg;
    tracked_ptr<insider::module_>                 module_;
    std::unordered_map<ptr<loop_id>, std::size_t> loop_headers;
    std::vector<ptr<>>                            constants;

    procedure_context(procedure_context* parent,
                      tracked_ptr<insider::module_> m)
      : parent{parent}
      , module_{std::move(m)}
    {
      cfg.emplace_back();
    }

    basic_block&
    current_block() { return cfg.back(); }

    std::size_t
    current_block_index() const { return cfg.size() - 1; }

    std::size_t
    start_new_block() {
      cfg.emplace_back();
      return current_block_index();
    }

    void
    emit(insider::opcode oc, std::same_as<operand> auto... operands) {
      current_block().body.emplace_back(oc, operands...);
    }

    void
    emit(instruction i) {
      current_block().body.emplace_back(std::move(i));
    }

    operand
    intern_constant(context&, ptr<> value);
  };

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

    explicit
    result_location(shared_register r)
      : reg_{std::move(r)}
    { }

    bool
    result_used() const { return used_; }

    bool
    has_result() const { return reg_.has_value(); }

    shared_register
    get(procedure_context&);

    void
    set(shared_register reg) {
      assert(!reg_);
      reg_ = std::move(reg);
    }

  private:
    shared_register reg_;
    bool            used_ = true;
  };
}

operand
procedure_context::intern_constant(context& ctx, ptr<> value) {
  for (operand i = 0; i < static_cast<operand>(constants.size()); ++i)
    if (eqv(ctx, constants[i], value))
      return i;

  constants.push_back(value);
  assert(constants.size() < operand_max);
  return constants.size() - 1;
}

shared_register
result_location::get(procedure_context& proc) {
  if (!reg_)
    reg_ = proc.registers.allocate_register();

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
emit_register_reference(procedure_context& proc, shared_register const& reg,
                        result_location& result) {
  if (!result.has_result())
    result.set(reg);
  else {
    shared_register result_reg = result.get(proc);
    if (reg != result_reg)
      proc.emit(opcode::set, *reg, *result_reg);
  }
}

static void
emit_variable_reference(procedure_context& proc, ptr<local_variable> var,
                        result_location& result) {
  shared_register var_reg = proc.bindings.lookup(var);
  assert(var_reg);
  emit_register_reference(proc, var_reg, result);
}

static void
compile_expression(context& ctx, procedure_context& proc, expression,
                   bool tail, result_location&);

static void
compile_expression_with_unused_result(context& ctx, procedure_context& proc,
                                      expression expr) {
  result_location subresult{false};
  compile_expression(ctx, proc, expr, false, subresult);
}

static shared_register
compile_expression_to_register(context& ctx, procedure_context& proc,
                               expression const& stx, bool tail) {
  result_location result;
  compile_expression(ctx, proc, stx, tail, result);
  return result.has_result() ? result.get(proc) : shared_register{};
}

static void
compile_expression(context& ctx, procedure_context& proc,
                   ptr <sequence_expression>, bool tail, result_location&);

static void
compile_expression(context&, procedure_context& proc,
                   ptr<local_reference_expression> stx, bool tail,
                   result_location& result);

static void
compile_static_reference(context& ctx, procedure_context& proc, ptr<> value,
                         result_location&);

static void
compile_global_reference(procedure_context& proc, operand global_num,
                         result_location&);

static void
compile_fold(context& ctx, procedure_context& proc,
             std::vector<expression> const& arguments,
             opcode op, result_location& result) {
  std::vector<shared_register> arg_registers;
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
    proc.emit(op, previous, **operand, *result.get(proc));
    previous = *result.get(proc);
  }
}

static void
compile_arithmetic(context& ctx, procedure_context& proc,
                   ptr<application_expression> stx, special_top_level_tag tag,
                   result_location& result) {
  if (stx->arguments().empty()) {
    if (tag == special_top_level_tag::plus) {
      compile_static_reference(ctx, proc, integer_to_ptr(0), result);
      return;
    }
    else if (tag == special_top_level_tag::times) {
      compile_static_reference(ctx, proc, integer_to_ptr(1), result);
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

  std::array<shared_register, 3> arg_registers;
  for (std::size_t i = 0; i < 3; ++i)
    arg_registers[i]
      = compile_expression_to_register(ctx, proc, stx->arguments()[i], false);

  proc.emit(opcode::vector_set,
            *arg_registers[0], *arg_registers[1], *arg_registers[2]);

  compile_static_reference(ctx, proc, ctx.constants->void_, result);
}

static void
compile_vector_ref(context& ctx, procedure_context& proc,
                   ptr<application_expression> stx, result_location& result) {
  if (stx->arguments().size() != 2)
    throw std::runtime_error{"vector-ref: Expected exactly 2 arguments"};

  shared_register v_reg
    = compile_expression_to_register(ctx, proc, stx->arguments()[0], false);
  shared_register i_reg
    = compile_expression_to_register(ctx, proc, stx->arguments()[1], false);

  if (result.result_used())
    proc.emit(opcode::vector_ref, *v_reg, *i_reg, *result.get(proc));
}

static void
compile_type(context& ctx, procedure_context& proc,
             ptr<application_expression> stx, result_location& result) {
  if (stx->arguments().size() != 1)
    throw std::runtime_error{"type: Expected exactly 1 argument"};

  shared_register expr_reg
    = compile_expression_to_register(ctx, proc, stx->arguments()[0], false);
  if (result.result_used())
    proc.emit(opcode::type, *expr_reg, *result.get(proc));
}

static void
compile_cons_application(context& ctx, procedure_context& proc,
                         ptr<application_expression> stx,
                         result_location& result) {
  if (stx->arguments().size() != 2)
    throw std::runtime_error{"cons: Expected exactly 2 arguments"};

  shared_register car_reg
    = compile_expression_to_register(ctx, proc, stx->arguments()[0], false);
  shared_register cdr_reg
    = compile_expression_to_register(ctx, proc, stx->arguments()[1], false);

  if (result.result_used())
    proc.emit(opcode::cons, *car_reg, *cdr_reg, *result.get(proc));
}

static void
compile_car(context& ctx, procedure_context& proc,
            ptr<application_expression> stx,
            result_location& result) {
  if (stx->arguments().size() != 1)
    throw std::runtime_error{"car: Expected exactly 1 argument"};

  shared_register pair_reg
    = compile_expression_to_register(ctx, proc, stx->arguments()[0], false);

  if (result.result_used())
    proc.emit(opcode::car, *pair_reg, *result.get(proc));
}

static void
compile_cdr(context& ctx, procedure_context& proc,
            ptr<application_expression> stx,
            result_location& result) {
  if (stx->arguments().size() != 1)
    throw std::runtime_error{"cdr: Expected exactly 1 argument"};

  shared_register pair_reg
    = compile_expression_to_register(ctx, proc, stx->arguments()[0], false);

  if (result.result_used())
    proc.emit(opcode::cdr, *pair_reg, *result.get(proc));
}

static void
compile_eq(context& ctx, procedure_context& proc,
           ptr<application_expression> stx,
           result_location& result) {
  if (stx->arguments().size() != 2)
    throw std::runtime_error{"eq?: Expected exactly 2 arguments"};

  shared_register lhs_reg
    = compile_expression_to_register(ctx, proc, stx->arguments()[0], false);
  shared_register rhs_reg
    = compile_expression_to_register(ctx, proc, stx->arguments()[1], false);

  if (result.result_used())
    proc.emit(opcode::eq, *lhs_reg, *rhs_reg, *result.get(proc));
}

static void
compile_box(context& ctx, procedure_context& proc,
            ptr<application_expression> stx,
            result_location& result) {
  if (stx->arguments().size() != 1)
    throw std::runtime_error{"box: Expected exactly 1 argument"};

  shared_register value
    = compile_expression_to_register(ctx, proc, stx->arguments()[0], false);
  if (result.result_used())
    proc.emit(opcode::box, *value, *result.get(proc));
}

static void
compile_unbox(context& ctx, procedure_context& proc,
              ptr<application_expression> stx,
              result_location& result) {
  if (stx->arguments().size() != 1)
    throw std::runtime_error{"unbox: Expected exactly 1 argument"};

  shared_register box
    = compile_expression_to_register(ctx, proc, stx->arguments()[0], false);
  if (result.result_used())
    proc.emit(opcode::unbox, *box, *result.get(proc));
}

static void
compile_box_set(context& ctx, procedure_context& proc,
                ptr<application_expression> stx,
                result_location& result) {
  if (stx->arguments().size() != 2)
    throw std::runtime_error{"box-set!: Expected exactly 2 argument"};

  shared_register box
    = compile_expression_to_register(ctx, proc, stx->arguments()[0], false);
  shared_register value
    = compile_expression_to_register(ctx, proc, stx->arguments()[1], false);

  proc.emit(opcode::box_set, *box, *value);
  compile_static_reference(ctx, proc, ctx.constants->void_, result);
}

static void
compile_loop_variable_definition(context& ctx, procedure_context& proc,
                                 variable_bindings::scope& scope,
                                 ptr<local_variable> var,
                                 expression expr) {
  shared_register reg = proc.registers.allocate_register();
  result_location res{reg};
  compile_expression(ctx, proc, expr, false, res);
  scope.emplace_back(variable_bindings::binding{var, std::move(reg)});
}

static void
compile_non_eliminable_variable_definition(context& ctx, procedure_context& proc,
                                           variable_bindings::scope& scope,
                                           ptr<local_variable> var,
                                           expression expr) {
  shared_register value
    = compile_expression_to_register(ctx, proc, expr, false);
  scope.emplace_back(variable_bindings::binding{var, std::move(value)});
}

static void
compile_eliminable_variable_definition(context& ctx, procedure_context& proc,
                                       variable_bindings::scope& scope,
                                       ptr<local_variable> var,
                                       expression expr) {
  compile_expression_with_unused_result(ctx, proc, expr);
  scope.emplace_back(
    variable_bindings::binding{var, proc.registers.allocate_register()}
  );
}

static variable_bindings::scope
compile_let_definitions(context& ctx, procedure_context& proc,
                        ptr<let_expression> stx) {
  variable_bindings::scope scope;
  for (auto const& def : stx->definitions())
    if (def.variable()->flags().is_loop_variable)
      compile_loop_variable_definition(ctx, proc, scope,
                                       def.variable(),
                                       def.expression());
    else if (def.variable()->flags().is_set_eliminable)
      compile_eliminable_variable_definition(ctx, proc, scope,
                                             def.variable(),
                                             def.expression());
    else
      compile_non_eliminable_variable_definition(ctx, proc, scope,
                                                 def.variable(),
                                                 def.expression());

  return scope;
}

static void
compile_expression(context& ctx, procedure_context& proc,
                   ptr<let_expression> stx, bool tail,
                   result_location& result) {
  variable_bindings::scope scope = compile_let_definitions(ctx, proc, stx);
  variable_bindings::unique_scope us
    = proc.bindings.push_scope(std::move(scope));
  compile_expression(ctx, proc, stx->body(), tail, result);
}

static void
compile_expression(context& ctx, procedure_context& proc,
                   ptr<local_set_expression> stx, bool,
                   result_location& result) {
  result_location dest_loc;
  shared_register reg = proc.bindings.lookup(stx->target());
  assert(reg);
  dest_loc.set(reg);
  compile_expression(ctx, proc, stx->expression(), false, dest_loc);
  compile_static_reference(ctx, proc, ctx.constants->void_, result);
}

static void
compile_expression(context& ctx, procedure_context& proc,
                   ptr<top_level_set_expression> stx, bool,
                   result_location& result) {
  shared_register value
    = compile_expression_to_register(ctx, proc, stx->expression(), false);
  proc.emit(opcode::store_top_level, *value, stx->target()->index);

  compile_static_reference(ctx, proc, ctx.constants->void_, result);
}

static ptr<procedure_prototype>
make_procedure_prototype(context& ctx, procedure_context& pc,
                         unsigned min_args, bool has_rest,
                         std::string name,
                         std::vector<ptr<>> constants) {
  auto [bc, di] = analyse_and_compile_cfg(pc.cfg);
  return make<procedure_prototype>(ctx,
                                   std::move(bc),
                                   std::move(di),
                                   pc.registers.registers_used(),
                                   min_args,
                                   has_rest,
                                   std::move(name),
                                   std::move(constants));
}

static ptr<procedure>
make_procedure(context& ctx, procedure_context& pc, unsigned min_args,
               bool has_rest, std::string name, std::vector<ptr<>> constants) {
  return make_captureless_procedure(
    ctx,
    make_procedure_prototype(ctx, pc, min_args,
                             has_rest,
                             std::move(name),
                             std::move(constants))
  );
}

static variable_bindings::unique_scope
push_parameters_and_closure_scope(procedure_context& proc,
                                  ptr<lambda_expression> stx) {
  variable_bindings::scope param_and_closure_scope;

  // Self variable.
  param_and_closure_scope.push_back(
    variable_bindings::binding{stx->self_variable(),
                               proc.registers.allocate_register()}
  );

  for (ptr<local_variable> param : stx->parameters())
    param_and_closure_scope.push_back(
      variable_bindings::binding{param, proc.registers.allocate_register()}
    );

  for (ptr<local_variable> free : stx->free_variables())
    param_and_closure_scope.push_back(
      variable_bindings::binding{free, proc.registers.allocate_register()}
    );

  return proc.bindings.push_scope(std::move(param_and_closure_scope));
}

static void
compile_lambda_body(context& ctx, procedure_context& proc,
                    ptr<lambda_expression> stx) {
  result_location body_result;
  compile_expression(ctx, proc, stx->body(), true, body_result);
  if (body_result.has_result())
    proc.emit(opcode::ret, *body_result.get(proc));
}

static void
emit_make_closure(context& ctx, procedure_context& parent,
                  ptr<procedure_prototype> proc, ptr<lambda_expression> stx,
                  result_location& result) {
  operand base = parent.registers.first_argument_register();
  result_location parent_loc{parent.registers.allocate_argument_register()};
  compile_static_reference(ctx, parent, proc, parent_loc);

  std::vector<shared_register> temp_registers;
  for (ptr<local_variable> var : stx->free_variables()) {
    shared_register r = parent.registers.allocate_argument_register();
    result_location result{r};
    emit_variable_reference(parent, var, result);
    temp_registers.push_back(std::move(r));
  }

  parent.emit(opcode::make_closure,
              base,
              static_cast<operand>(temp_registers.size()),
              *result.get(parent));
}

static void
emit_reference_to_empty_closure(context& ctx, procedure_context& parent,
                                ptr<procedure_prototype> proc,
                                result_location& result) {
  auto cls = make_captureless_procedure(ctx, proc);
  compile_static_reference(ctx, parent, cls, result);
}

static void
compile_expression(context& ctx, procedure_context& parent,
                   ptr<lambda_expression> stx, bool, result_location& result) {
  procedure_context proc{&parent, parent.module_};

  auto us = push_parameters_and_closure_scope(proc, stx);
  compile_lambda_body(ctx, proc, stx);

  auto p = make_procedure_prototype(
    ctx, proc,
    static_cast<unsigned>(stx->parameters().size() - (stx->has_rest() ? 1 : 0)),
    stx->has_rest(), stx->name(), std::move(proc.constants)
  );

  if (stx->free_variables().empty())
    emit_reference_to_empty_closure(ctx, parent, p, result);
  else
    emit_make_closure(ctx, parent, p, stx, result);
}

namespace {
  // Chunk of blocks. Head is the entry block of the chunk; tail is the exit
  // block
  struct block_chunk {
    std::size_t head_index{};
    std::size_t tail_index{};
  };
}

static block_chunk
compile_expression_in_new_block(context& ctx, procedure_context& proc,
                                expression stx, bool tail,
                                result_location& result) {
  std::size_t head_idx = proc.start_new_block();
  compile_expression(ctx, proc, stx, tail, result);
  std::size_t tail_idx = proc.current_block_index();
  return {head_idx, tail_idx};
}

static std::tuple<shared_register, block_chunk>
compile_expression_in_new_block_to_register(context& ctx,
                                            procedure_context& proc,
                                            expression stx,
                                            bool tail) {
  std::size_t head_idx = proc.start_new_block();
  shared_register reg = compile_expression_to_register(ctx, proc, stx, tail);
  std::size_t tail_idx = proc.current_block_index();
  return {reg, {head_idx, tail_idx}};
}

static std::tuple<block_chunk, block_chunk>
compile_if_branches_to_specified_register(context& ctx, procedure_context& proc,
                                          ptr<if_expression> stx, bool tail,
                                          result_location& result) {
  block_chunk consequent
    = compile_expression_in_new_block(ctx, proc, stx->consequent(), tail,
                                      result);
  block_chunk alternative
    = compile_expression_in_new_block(ctx, proc, stx->alternative(), tail,
                                      result);
  return {consequent, alternative};
}

static void
unify_if_branch_results(procedure_context& proc,
                        shared_register const& consequent_reg,
                        shared_register const& alternative_reg,
                        basic_block& consequent_block,
                        basic_block& alternative_block,
                        result_location& result) {
  if (!consequent_reg && !alternative_reg)
    return;
  else if (!consequent_reg && alternative_reg)
    result.set(alternative_reg);
  else if ((consequent_reg && !alternative_reg)
           || consequent_reg == alternative_reg)
    result.set(consequent_reg);
  else {
    assert(proc.bindings.register_is_store_for_variable(consequent_reg));
    if (!proc.bindings.register_is_store_for_variable(alternative_reg)) {
      consequent_block.body.emplace_back(opcode::set,
                                         *consequent_reg, *alternative_reg);
      result.set(alternative_reg);
    } else {
      shared_register new_reg = proc.registers.allocate_register();
      consequent_block.body.emplace_back(opcode::set,
                                         *consequent_reg, *new_reg);
      alternative_block.body.emplace_back(opcode::set,
                                          *alternative_reg, *new_reg);
      result.set(new_reg);
    }
  }
}

static block_chunk
compile_alternative_branch_to_unspecified_register(
  context& ctx,
  procedure_context& proc,
  expression stx,
  bool tail,
  result_location& result,
  shared_register const& consequent_reg,
  std::size_t consequent_tail_idx
) {
  if (consequent_reg &&
      !proc.bindings.register_is_store_for_variable(consequent_reg)) {
    result.set(consequent_reg);
    return compile_expression_in_new_block(ctx, proc, stx, tail, result);
  } else {
    auto [alternative_reg, chunk]
      = compile_expression_in_new_block_to_register(ctx, proc, stx, tail);

    unify_if_branch_results(
      proc,
      consequent_reg,
      alternative_reg,
      proc.cfg[consequent_tail_idx],
      proc.current_block(),
      result
    );

    return chunk;
  }
}

static std::tuple<block_chunk, block_chunk>
compile_if_branches_to_unspecified_register(context& ctx,
                                            procedure_context& proc,
                                            ptr<if_expression> stx,
                                            bool tail,
                                            result_location& result) {
  auto [consequent_reg, consequent_chunk]
    = compile_expression_in_new_block_to_register(ctx, proc, stx->consequent(),
                                                  tail);

  block_chunk alternative_chunk
    = compile_alternative_branch_to_unspecified_register(
        ctx, proc, stx->alternative(), tail, result, consequent_reg,
        consequent_chunk.tail_index
      );

  return {consequent_chunk, alternative_chunk};
}

static void
compile_expression(context& ctx, procedure_context& proc,
                   ptr<if_expression> stx, bool tail, result_location& result) {
  shared_register test_value
    = compile_expression_to_register(ctx, proc, stx->test(), false);

  std::size_t test_block_index = proc.current_block_index();
  block_chunk consequent_chunk;
  block_chunk alternative_chunk;

  if (result.has_result() || !result.result_used())
    std::tie(consequent_chunk, alternative_chunk)
      = compile_if_branches_to_specified_register(ctx, proc, stx, tail, result);
  else
    std::tie(consequent_chunk, alternative_chunk)
      = compile_if_branches_to_unspecified_register(ctx, proc, stx, tail,
                                                    result);

  std::size_t after_block_idx = proc.start_new_block();
  basic_block& test_block = proc.cfg[test_block_index];
  basic_block& consequent_tail = proc.cfg[consequent_chunk.tail_index];

  assert(std::holds_alternative<flow_off>(consequent_tail.ending));
  assert(std::holds_alternative<flow_off>(test_block.ending));

  consequent_tail.ending = unconditional_jump{after_block_idx};
  test_block.ending = conditional_jump{*test_value,
                                       alternative_chunk.head_index};
}

static std::vector<shared_register>
compile_application_arguments(context& ctx, procedure_context& proc,
                              ptr<application_expression> stx) {
  std::vector<shared_register> result;

  for (expression arg : stx->arguments()) {
    result_location reg{proc.registers.allocate_argument_register()};
    compile_expression(ctx, proc, arg, false, reg);
    result.emplace_back(reg.get(proc));
  }

  return result;
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

  operand call_base = proc.registers.first_argument_register();

  result_location f_loc{proc.registers.allocate_argument_register()};
  compile_expression(ctx, proc, stx->target(), false, f_loc);

  auto arg_registers = compile_application_arguments(ctx, proc, stx);

  instruction i;
  if (!tail)
    i = {opcode::call, call_base,
         static_cast<operand>(stx->arguments().size()),
         *result.get(proc)};
  else
    i = {opcode::tail_call, call_base,
         static_cast<operand>(stx->arguments().size())};

  proc.emit(std::move(i));
  if (stx->debug_info())
    proc.current_block().debug_info[proc.current_block().body.size() - 1]
      = *stx->debug_info();
}

static void
compile_expression(context&, procedure_context& proc,
                   ptr<local_reference_expression> stx, bool,
                   result_location& result) {
  if (result.result_used())
    emit_variable_reference(proc, stx->variable(), result);
}

static void
compile_fixnum_reference(context& ctx, procedure_context& proc,
                         integer::value_type value, result_location& result) {
  if (value >= immediate_min && value <= immediate_max)
    proc.emit(opcode::load_fixnum,
              immediate_to_operand(static_cast<immediate_type>(value)),
              *result.get(proc));
  else
    proc.emit(opcode::load_constant,
              proc.intern_constant(ctx, integer_to_ptr(value)),
              *result.get(proc));
}

static void
compile_static_reference(context& ctx, procedure_context& proc, ptr<> value,
                         result_location& result) {
  if (!result.result_used())
    return;

  if (value == ctx.constants->null)
    proc.emit(opcode::load_null, *result.get(proc));
  else if (value == ctx.constants->void_)
    proc.emit(opcode::load_void, *result.get(proc));
  else if (value == ctx.constants->t)
    proc.emit(opcode::load_t, *result.get(proc));
  else if (value == ctx.constants->f)
    proc.emit(opcode::load_f, *result.get(proc));
  else if (value == ctx.constants->eof)
    proc.emit(opcode::load_eof, *result.get(proc));
  else if (auto fx = match<integer>(value))
    compile_fixnum_reference(ctx, proc, fx->value(), result);
  else
    proc.emit(opcode::load_constant, proc.intern_constant(ctx, value),
              *result.get(proc));
}

static void
compile_global_reference(procedure_context& proc, operand global_num,
                         result_location& result) {
  if (!result.result_used())
    return;

  proc.emit(opcode::load_top_level, global_num, *result.get(proc));
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

  operand id_index = proc.intern_constant(ctx, stx->name());
  proc.emit(opcode::load_dynamic_top_level, id_index, *result.get(proc));
}

static void
compile_expression(context& ctx, procedure_context& proc,
                   ptr<literal_expression> lit, bool, result_location& result) {
  compile_static_reference(ctx, proc, lit->value(), result);
}

static void
compile_expression(context& ctx, procedure_context& proc,
                   ptr<loop_body> loop, bool tail, result_location& result) {
  assert(!proc.loop_headers.contains(loop->id()));
  proc.loop_headers.emplace(loop->id(), proc.start_new_block());
  compile_expression(ctx, proc, loop->body(), tail, result);
}

static void
compile_loop_assignments(context& ctx, procedure_context& proc,
                         ptr<loop_continue> cont) {
  for (definition_pair_expression const& var : cont->variables()) {
    shared_register reg = proc.bindings.lookup(var.variable());
    result_location dest{reg};
    compile_expression(ctx, proc, var.expression(), false, dest);
  }
}

static void
compile_expression(context& ctx, procedure_context& proc,
                   ptr<loop_continue> cont, bool, result_location&) {
  compile_loop_assignments(ctx, proc, cont);
  proc.current_block().ending
    = unconditional_jump{proc.loop_headers.at(cont->id())};
  proc.start_new_block();
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
    compile_static_reference(ctx, proc, ctx.constants->void_, result);

  for (auto expr = stx->expressions().begin();
       expr != stx->expressions().end();
       ++expr) {
    bool last = std::next(expr) == stx->expressions().end();
    if (last)
      compile_expression(ctx, proc, *expr, tail && last, result);
    else
      compile_expression_with_unused_result(ctx, proc, *expr);
  }
}

ptr<procedure>
compile_expression(context& ctx, ptr<syntax> datum,
                   tracked_ptr<module_> const& mod,
                   source_file_origin const& origin,
                   pass_list passes) {
  return compile_syntax(ctx,
                        analyse(ctx, datum, mod, std::move(passes), origin),
                        mod);
}

static shared_register
reserve_register_for_self_variable(procedure_context& proc) {
  shared_register self = proc.registers.allocate_register();
  assert(*self == operand{0});
  return self;
}

ptr<procedure>
compile_syntax(context& ctx, expression e, tracked_ptr<module_> const& mod) {
  procedure_context proc{nullptr, mod};
  shared_register self = reserve_register_for_self_variable(proc);
  shared_register result = compile_expression_to_register(ctx, proc, e, true);
  if (result)
    proc.emit(opcode::ret, *result);

  return make_captureless_procedure(
    ctx, make_procedure_prototype(ctx, proc, 0, false, "<expression>",
                                      std::move(proc.constants))
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
  shared_register self = reserve_register_for_self_variable(proc);
  result_location result;
  compile_expression(ctx, proc, body_expr, true, result);
  if (result.has_result())
    proc.emit(opcode::ret, *result.get(proc));

  m->set_top_level_procedure(
    ctx.store,
    make_procedure(ctx, proc, 0, false,
                   fmt::format("<module {} top-level>",
                               pm.name
                               ? module_name_to_string(*pm.name)
                               : "<unknown>"),
                   std::move(proc.constants))
  );
}

} // namespace insider
