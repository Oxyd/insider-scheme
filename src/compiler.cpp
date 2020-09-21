#include "compiler.hpp"

#include "action.hpp"
#include "analyser.hpp"

#include <fmt/format.h>

#include <memory>
#include <optional>
#include <stdexcept>
#include <unordered_map>
#include <unordered_map>

namespace insider {

namespace {
  class shared_register;

  class register_allocator {
  public:
    shared_register
    allocate_local();

    shared_register
    allocate_closure();

    void
    ref(operand);

    void
    unref(operand);

    unsigned
    locals_used() const { return locals_max_; }

  private:
    operand::representation_type              locals_max_ = 0;
    operand::representation_type              closure_max_ = 0;
    std::vector<operand::representation_type> locals_free_;
    std::unordered_map<operand::representation_type, unsigned> local_ref_counts_;
  };

  class shared_register {
  public:
    shared_register() = default;
    shared_register(register_allocator&, operand);
    explicit
    shared_register(operand);
    shared_register(shared_register const&);
    shared_register(shared_register&&);
    ~shared_register();

    shared_register&
    operator = (shared_register const&);

    shared_register&
    operator = (shared_register&&);

    operand
    operator * () const { assert(has_value()); return value_; }

    operand const*
    operator -> () const { assert(has_value()); return &value_; }

    bool
    has_value() const { return value_.scope() != operand::scope_type::local || alloc_ != nullptr; }

    operator
    bool () const { return has_value(); }

  private:
    register_allocator* alloc_ = nullptr;
    operand             value_;
  };

  class variable_bindings {
  public:
    struct binding {
      std::shared_ptr<insider::variable> variable;
      shared_register                    destination;
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

    void
    add_free(std::shared_ptr<variable>, shared_register);

    free_variables_map const&
    free() const { return free_variables_; }

  private:
    std::vector<scope> scopes_;
    free_variables_map free_variables_;
  };

  struct procedure_context {
    procedure_context* parent = nullptr;
    register_allocator registers;
    variable_bindings  bindings;
    insider::bytecode  bytecode;
    insider::module&   module;

    procedure_context(procedure_context* parent, insider::module& m)
      : parent{parent}
      , module{m} { }
  };
}

shared_register
register_allocator::allocate_local() {
  if (locals_free_.empty())
    return {*this, operand::local(locals_max_++)};
  else {
    auto result = operand::local(locals_free_.back());
    locals_free_.pop_back();
    return {*this, result};
  }
}

shared_register
register_allocator::allocate_closure() {
  return {*this, operand::closure(closure_max_++)};
}

void
register_allocator::ref(operand o) {
  if (o.scope() == operand::scope_type::local)
    ++local_ref_counts_[o.representation()];
}

void
register_allocator::unref(operand o) {
  if (o.scope() == operand::scope_type::local) {
    auto ref_count = local_ref_counts_.find(o.representation());
    assert(ref_count != local_ref_counts_.end());

    if (--ref_count->second == 0) {
      locals_free_.push_back(o.value());
      local_ref_counts_.erase(ref_count);
    }
  }
}

shared_register::shared_register(register_allocator& alloc, operand value)
  : alloc_{&alloc}
  , value_{value}
{
  alloc_->ref(value_);
}

shared_register::shared_register(operand value)
  : value_{value}
{
  assert(value.scope() == operand::scope_type::global
         || value.scope() == operand::scope_type::static_);
}

shared_register::shared_register(shared_register const& other)
  : alloc_{other.alloc_}
  , value_{other.value_}
{
  if (alloc_)
    alloc_->ref(value_);
}

shared_register::shared_register(shared_register&& other)
  : alloc_{other.alloc_}
  , value_{other.value_}
{
  other.alloc_ = nullptr;
}

shared_register::~shared_register() {
  if (alloc_)
    alloc_->unref(value_);
}

shared_register&
shared_register::operator = (shared_register const& other) {
  if (alloc_)
    alloc_->unref(value_);

  alloc_ = other.alloc_;
  value_ = other.value_;
  if (alloc_)
    alloc_->ref(value_);

  return *this;
}

shared_register&
shared_register::operator = (shared_register&& other) {
  if (alloc_)
    alloc_->unref(value_);

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

  if (auto free = free_variables_.find(v); free != free_variables_.end())
    return free->second;

  return {};
}

void
variable_bindings::add_free(std::shared_ptr<variable> v, shared_register dest) {
  free_variables_.emplace(std::move(v), dest);
}

static shared_register
compile_expression(context& ctx, procedure_context& proc, syntax const&, bool tail);

static shared_register
compile_sequence(context& ctx, procedure_context& proc, sequence_syntax const&, bool tail);

static shared_register
compile_local_reference(procedure_context& proc, local_reference_syntax const& stx);

static shared_register
compile_fold(context& ctx, procedure_context& proc, std::vector<std::unique_ptr<syntax>> const& arguments,
             opcode op) {
  std::vector<shared_register> arg_registers;
  for (auto const& arg : arguments)
    arg_registers.push_back(compile_expression(ctx, proc, *arg, false));

  shared_register result = proc.registers.allocate_local();
  operand previous = *arg_registers.front();
  for (auto operand = arg_registers.begin() + 1; operand != arg_registers.end(); ++operand) {
    proc.bytecode.push_back(instruction{op, previous, **operand, *result});
    previous = *result;
  }

  return result;
}

static shared_register
compile_arithmetic(context& ctx, procedure_context& proc, application_syntax const& stx, special_top_level_tag tag) {
  if (stx.arguments.empty()) {
    if (tag == special_top_level_tag::plus)
      return shared_register{ctx.statics.zero};
    else if (tag == special_top_level_tag::times)
      return shared_register{ctx.statics.one};
    else
      throw std::runtime_error{fmt::format("Not enough arguments for {}",
                                           std::get<top_level_reference_syntax>(stx.target->value).name)};
  }

  opcode op;
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

  return compile_fold(ctx, proc, stx.arguments, op);
}

static shared_register
compile_relational(context& ctx, procedure_context& proc, application_syntax const& stx, special_top_level_tag tag) {
  if (stx.arguments.size() < 2)
    throw std::runtime_error{fmt::format("Not enough arguments for {}",
                                         std::get<local_reference_syntax>(stx.target->value).variable->name)};

  opcode op;
  switch (tag) {
  case special_top_level_tag::arith_equal:  op = opcode::arith_equal; break;
  case special_top_level_tag::less_than:    op = opcode::less_than; break;
  case special_top_level_tag::greater_than: op = opcode::greater_than; break;
  default:
    assert(!"Unimplemented");
  }

  return compile_fold(ctx, proc, stx.arguments, op);
}

static shared_register
compile_let(context& ctx, procedure_context& proc, let_syntax const& stx, bool tail) {
  variable_bindings::scope scope;
  for (auto const& def : stx.definitions) {
    shared_register value = compile_expression(ctx, proc, *def.expression, false);
    scope.emplace_back(variable_bindings::binding{def.variable, std::move(value)});
  }

  variable_bindings::unique_scope us = proc.bindings.push_scope(std::move(scope));
  return compile_sequence(ctx, proc, stx.body, tail);
}

static shared_register
compile_local_set(context& ctx, procedure_context& proc, local_set_syntax const& stx) {
  shared_register dest = proc.bindings.lookup(stx.target);
  assert(dest);

  shared_register value = compile_expression(ctx, proc, *stx.expression, false);
  proc.bytecode.push_back(instruction{opcode::set, *value, {}, *dest});

  return shared_register{ctx.statics.void_};
}

static shared_register
compile_top_level_set(context& ctx, procedure_context& proc, top_level_set_syntax const& stx) {
  shared_register value = compile_expression(ctx, proc, *stx.expression, false);
  proc.bytecode.push_back(instruction{opcode::set, *value, {}, stx.location});

  return shared_register{ctx.statics.void_};
}

static void
emit_data_pushes(bytecode& bc, std::vector<shared_register> const& values) {
  for (std::size_t i = 0; i < values.size(); i += 3) {
    std::size_t to_push = std::min(values.size() - i, static_cast<std::size_t>(3));

    switch (to_push) {
    case 1:
      bc.push_back(instruction{opcode::push1, *values[i], {}, {}});
      break;

    case 2:
      bc.push_back(instruction{opcode::push2, *values[i], *values[i + 1], {}});
      break;

    case 3:
      bc.push_back(instruction{opcode::push3, *values[i], *values[i + 1], *values[i + 2]});
      break;

    default:
      assert(!"Can't get here");
    }
  }
}

static shared_register
compile_lambda(context& ctx, procedure_context& parent, lambda_syntax const& stx) {
  procedure_context proc{&parent, parent.module};
  variable_bindings::scope args_scope;

  for (auto const& param : stx.parameters)
    args_scope.push_back(variable_bindings::binding{param, proc.registers.allocate_local()});

  variable_bindings::unique_scope us = proc.bindings.push_scope(std::move(args_scope));

  shared_register return_value = compile_sequence(ctx, proc, stx.body, true);
  if (return_value)
    proc.bytecode.push_back(instruction{opcode::ret, *return_value, {}, {}});

  ptr<procedure> p = ctx.store.make<procedure>(std::move(proc.bytecode),
                                               proc.registers.locals_used(),
                                               stx.parameters.size() - (stx.has_rest ? 1 : 0),
                                               stx.has_rest,
                                               stx.name);
  operand p_reg = operand::static_(ctx.intern_static(p));

  if (!proc.bindings.free().empty()) {
    std::vector<std::shared_ptr<variable>> free_vars;
    free_vars.resize(proc.bindings.free().size());
    for (auto const& [var, reg] : proc.bindings.free()) {
      assert(reg->value() < free_vars.size());
      free_vars[reg->value()] = var;
    }

    std::vector<shared_register> closure;
    closure.reserve(free_vars.size());
    for (std::shared_ptr<variable> const& var : free_vars)
      closure.push_back(compile_local_reference(parent, local_reference_syntax{var}));

    emit_data_pushes(parent.bytecode, closure);

    shared_register result = parent.registers.allocate_local();
    parent.bytecode.push_back(instruction{opcode::make_closure,
                                          p_reg,
                                          operand::immediate(proc.bindings.free().size()),
                                          *result});

    return result;
  }
  else
    return shared_register{p_reg};
}

// Make sure a value is placed in a local register. If it already is, nothing is
// done. If it isn't, a new local register is allocated, and the original value
// is copied into the local register.
static shared_register
make_local(procedure_context& proc, shared_register const& original) {
  if (original->scope() == operand::scope_type::local)
    return original;

  shared_register result = proc.registers.allocate_local();
  proc.bytecode.push_back({opcode::set, *original, {}, *result});
  return result;
}

static shared_register
compile_if(context& ctx, procedure_context& proc, if_syntax const& stx, bool tail) {
  shared_register test_value = compile_expression(ctx, proc, *stx.test, false);

  // The actual offset will be backpatched in later -- it needs to skip over the then-expr.
  proc.bytecode.push_back({opcode::jump_unless, *test_value, operand::offset(0), {}});
  std::size_t jump_index = proc.bytecode.size() - 1;

  // The two subexpressions, then-expr and else-expr, will possibly place their
  // result in a different register. We will handle this by appending a set to
  // else-expr that moves its result to the same register then-expr uses. This
  // works out nicely in the case this is a one-armed if, where we can simply
  // set the then-expr's result register to #void if the else branch happens to
  // be taken.
  //
  // Since both subexpressions are in tail position with respect to the if, it
  // is possible that one or both will exit with a tail-call and as such, they
  // won't have any value to return to the if.

  shared_register result = compile_expression(ctx, proc, *stx.consequent, tail);
  std::size_t else_begin_index;

  if (result)
    result = make_local(proc, result);

  if (stx.alternative) {
    // If we got here by executing the then-branch, skip over the
    // else-branch. If the test condition was false, we'll jump right after this
    // jump, skipping it. Again, we'll have to backpatch the actual offset.
    proc.bytecode.push_back({opcode::jump, operand::offset(0), {}, {}});
    std::size_t else_jump_index = proc.bytecode.size() - 1;
    else_begin_index = proc.bytecode.size() - 1;

    shared_register else_result = compile_expression(ctx, proc, *stx.alternative, tail);
    if (result && else_result)
      proc.bytecode.push_back({opcode::set, *else_result, {}, *result});
    else if (else_result && !result)
      result = else_result;

    proc.bytecode[else_jump_index].x = operand::offset(proc.bytecode.size() - 1 - else_jump_index);
  } else {
    // No else branch -- we'll set the result to #void. First, if we landed here
    // because we executed the then-branch, we'll have to skip this branch -- we
    // know it's a single instruction, so we don't need to do any
    // backpatching. Second, we need to be mindful that the then-branch might
    // have been compiled into a tail-call and so may not have produced a
    // meaningful result register.

    proc.bytecode.push_back({opcode::jump, operand::offset(1), {}, {}});
    if (!result)
      result = proc.registers.allocate_local();

    else_begin_index = proc.bytecode.size() - 1;
    proc.bytecode.push_back({opcode::set, ctx.statics.void_, {}, *result});
  }

  proc.bytecode[jump_index].y = operand::offset(else_begin_index - jump_index);
  return result;
}

static shared_register
compile_application(context& ctx, procedure_context& proc, application_syntax const& stx, bool tail) {
  if (auto* ref = std::get_if<top_level_reference_syntax>(&stx.target->value))
    if (std::optional<special_top_level_tag> tag = ctx.find_tag(ref->location.value())) {
      if (*tag == special_top_level_tag::plus || *tag == special_top_level_tag::minus
          || *tag == special_top_level_tag::times || *tag == special_top_level_tag::divide)
        return compile_arithmetic(ctx, proc, stx, *tag);
      else if (*tag == special_top_level_tag::less_than
               || *tag == special_top_level_tag::greater_than
               || *tag == special_top_level_tag::arith_equal)
        return compile_relational(ctx, proc, stx, *tag);
    }

  std::vector<shared_register> arg_registers;
  for (auto const& arg : stx.arguments)
    arg_registers.push_back(compile_expression(ctx, proc, *arg, false));

  emit_data_pushes(proc.bytecode, arg_registers);

  shared_register f = compile_expression(ctx, proc, *stx.target, false);
  shared_register result;
  if (!tail) {
    result = proc.registers.allocate_local();
    proc.bytecode.push_back({opcode::call, *f, operand::immediate(arg_registers.size()), *result});
  } else
    proc.bytecode.push_back({opcode::tail_call, *f, operand::immediate(arg_registers.size()), {}});

  return result;
}

static shared_register
compile_local_reference(procedure_context& proc, local_reference_syntax const& stx) {
  if (shared_register r = proc.bindings.lookup(stx.variable))
    return r;

  // Not found in this procedure, and it's a local reference, so it must be
  // referring to a free variable.

  shared_register result = proc.registers.allocate_closure();
  proc.bindings.add_free(stx.variable, result);
  return result;
}

static shared_register
compile_top_level_reference(top_level_reference_syntax const& stx) {
  return shared_register{stx.location};
}

static shared_register
compile_box(context& ctx, procedure_context& proc, box_syntax const& stx) {
  shared_register value = compile_expression(ctx, proc, *stx.expression, false);
  shared_register result = proc.registers.allocate_local();
  proc.bytecode.push_back({opcode::box, *value, {}, *result});
  return result;
}

static shared_register
compile_unbox(context& ctx, procedure_context& proc, unbox_syntax const& stx) {
  shared_register box = compile_expression(ctx, proc, *stx.box_expr, false);
  shared_register result = proc.registers.allocate_local();
  proc.bytecode.push_back({opcode::unbox, *box, {}, *result});
  return result;
}

static shared_register
compile_box_set(context& ctx, procedure_context& proc, box_set_syntax const& stx) {
  shared_register box = compile_expression(ctx, proc, *stx.box_expr, false);
  shared_register value = compile_expression(ctx, proc, *stx.value_expr, false);
  proc.bytecode.push_back({opcode::box_set, *box, *value, {}});
  return shared_register{ctx.statics.void_};
}

static shared_register
compile_cons(context& ctx, procedure_context& proc, cons_syntax const& stx) {
  shared_register car = compile_expression(ctx, proc, *stx.car, false);
  shared_register cdr = compile_expression(ctx, proc, *stx.cdr, false);
  shared_register result = proc.registers.allocate_local();
  proc.bytecode.push_back({opcode::cons, *car, *cdr, *result});
  return result;
}

static shared_register
compile_make_vector(context& ctx, procedure_context& proc, make_vector_syntax const& stx) {
  std::vector<shared_register> exprs;
  exprs.reserve(stx.elements.size());
  for (std::unique_ptr<syntax> const& e : stx.elements)
    exprs.push_back(compile_expression(ctx, proc, *e, false));

  shared_register result = proc.registers.allocate_local();
  emit_data_pushes(proc.bytecode, exprs);
  proc.bytecode.push_back({opcode::make_vector, operand::immediate(exprs.size()), {}, *result});

  return result;
}

// Translate an expression and return the register where the result is stored.
static shared_register
compile_expression(context& ctx, procedure_context& proc, syntax const& stx, bool tail) {
  if (auto* lit = std::get_if<literal_syntax>(&stx.value))
    return shared_register{operand::static_(ctx.intern_static(lit->value))};
  else if (auto* local_ref = std::get_if<local_reference_syntax>(&stx.value))
    return compile_local_reference(proc, *local_ref);
  else if (auto* top_level_ref = std::get_if<top_level_reference_syntax>(&stx.value))
    return compile_top_level_reference(*top_level_ref);
  else if (auto* app = std::get_if<application_syntax>(&stx.value))
    return compile_application(ctx, proc, *app, tail);
  else if (auto* let = std::get_if<let_syntax>(&stx.value))
    return compile_let(ctx, proc, *let, tail);
  else if (auto* local_set = std::get_if<local_set_syntax>(&stx.value))
    return compile_local_set(ctx, proc, *local_set);
  else if (auto* top_level_set = std::get_if<top_level_set_syntax>(&stx.value))
    return compile_top_level_set(ctx, proc, *top_level_set);
  else if (auto* lambda = std::get_if<lambda_syntax>(&stx.value))
    return compile_lambda(ctx, proc, *lambda);
  else if (auto* if_ = std::get_if<if_syntax>(&stx.value))
    return compile_if(ctx, proc, *if_, tail);
  else if (auto* box = std::get_if<box_syntax>(&stx.value))
    return compile_box(ctx, proc, *box);
  else if (auto* unbox = std::get_if<unbox_syntax>(&stx.value))
    return compile_unbox(ctx, proc, *unbox);
  else if (auto* box_set = std::get_if<box_set_syntax>(&stx.value))
    return compile_box_set(ctx, proc, *box_set);
  else if (auto* cons = std::get_if<cons_syntax>(&stx.value))
    return compile_cons(ctx, proc, *cons);
  else if (auto* make_vector = std::get_if<make_vector_syntax>(&stx.value))
    return compile_make_vector(ctx, proc, *make_vector);
  else if (auto* sequence = std::get_if<sequence_syntax>(&stx.value))
    return compile_sequence(ctx, proc, *sequence, tail);
  else {
    assert(!"Unexpected syntax");
    return {};
  }
}

static shared_register
compile_sequence(context& ctx, procedure_context& proc, sequence_syntax const& stx, bool tail) {
  if (stx.expressions.empty())
    return shared_register{ctx.statics.void_};

  shared_register result;
  for (auto expr = stx.expressions.begin(); expr != stx.expressions.end(); ++expr) {
    bool last = std::next(expr) == stx.expressions.end();
    result = compile_expression(ctx, proc, **expr, tail && last);
  }

  return result;
}

ptr<procedure>
compile_expression(context& ctx, generic_ptr const& datum, module& mod) {
  auto stx = analyse(ctx, datum, mod);

  procedure_context proc{nullptr, mod};
  shared_register result = compile_expression(ctx, proc, *stx, true);
  if (result)
    proc.bytecode.push_back(instruction{opcode::ret, *result, {}, {}});
  return ctx.store.make<procedure>(std::move(proc.bytecode),
                                   proc.registers.locals_used(),
                                   0);
}

module
compile_main_module(context& ctx, std::vector<generic_ptr> const& data) {
  simple_action a(ctx, "Analysing main module");
  protomodule pm = read_main_module(ctx, data);
  module result{ctx};
  perform_imports(ctx, result, pm);
  compile_module_body(ctx, result, pm);
  return result;
}

void
compile_module_body(context& ctx, module& m, protomodule const& pm) {
  sequence_syntax body = analyse_module(ctx, m, pm);

  procedure_context proc{nullptr, m};
  shared_register result = compile_sequence(ctx, proc, body, true);
  if (result)
    proc.bytecode.push_back(instruction{opcode::ret, *result, {}, {}});

  m.set_top_level_procedure(make<procedure>(ctx,
                                            std::move(proc.bytecode),
                                            proc.registers.locals_used(),
                                            0));
}

} // namespace insider
