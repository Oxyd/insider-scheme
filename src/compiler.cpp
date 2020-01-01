#include "compiler.hpp"

#include "analyser.hpp"

#include <fmt/format.h>

#include <memory>
#include <optional>
#include <stdexcept>
#include <unordered_map>
#include <unordered_map>

namespace game::scm {

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
    operator * () const { return value_; }

    operand const*
    operator -> () const { return &value_; }

    operator
    bool () const { return value_.scope() != operand::scope_type::local || alloc_ != nullptr; }

  private:
    register_allocator* alloc_ = nullptr;
    operand             value_;
  };

  class symbol_bindings {
  public:
    struct binding {
      ptr<scm::symbol> symbol;
      shared_register  destination;
    };
    using scope = std::vector<binding>;

    class unique_scope {
    public:
      explicit
      unique_scope(symbol_bindings* b) : b_{b} { }
      unique_scope(unique_scope const&) = delete;
      void operator = (unique_scope const&) = delete;

      ~unique_scope() { b_->pop_scope(); }

    private:
      symbol_bindings* b_;
    };

    unique_scope
    push_scope(scope = {});

    void
    pop_scope();

    shared_register
    lookup(ptr<symbol> const&) const;

    void
    add_free(ptr<symbol> const&, shared_register);

    eqv_unordered_map<shared_register> const&
    free() const { return free_variables_; }

  private:
    std::vector<scope> scopes_;
    eqv_unordered_map<shared_register> free_variables_;
  };

  struct procedure_context {
    procedure_context* parent = nullptr;
    register_allocator registers;
    symbol_bindings    bindings;
    scm::bytecode      bytecode;
    ptr<scm::module>   module;

    procedure_context(procedure_context* parent, ptr<scm::module> const& m)
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
symbol_bindings::push_scope(scope s) -> unique_scope {
  scopes_.emplace_back(std::move(s));
  return unique_scope{this};
}

void
symbol_bindings::pop_scope() {
  scopes_.pop_back();
}

shared_register
symbol_bindings::lookup(ptr<symbol> const& s) const {
  for (auto scope = scopes_.rbegin(); scope != scopes_.rend(); ++scope)
    for (binding const& b : *scope)
      if (b.symbol.get() == s.get())
        return b.destination;

  if (auto free = free_variables_.find(s); free != free_variables_.end())
    return free->second;

  return {};
}

void
symbol_bindings::add_free(ptr<symbol> const& s, shared_register dest) {
  free_variables_.emplace(s, dest);
}

static shared_register
compile_expression(context& ctx, procedure_context& proc, syntax const&, bool tail);

static shared_register
compile_body(context& ctx, procedure_context& proc, body_syntax const&, bool tail);

static shared_register
compile_reference(procedure_context& proc, reference_syntax const& stx);

static std::optional<std::size_t>
find_global(ptr<module> const& m, ptr<symbol> const& s) {
  auto global = m->symbols.find(s.get());
  if (global == m->symbols.end()) {
    global = m->imports.find(s.get());

    if (global == m->imports.end())
      return std::nullopt;
  }

  return global->second;
}

static std::optional<module::binding_tag>
find_tag(procedure_context const& proc, ptr<symbol> const& sym) {
  shared_register r = proc.bindings.lookup(sym);
  if (r)
    // It's a local, so it can't be an imported symbol.
    return std::nullopt;
  else {
    std::optional<std::size_t> binding_index = find_global(proc.module, sym);
    if (!binding_index)
      return std::nullopt;

    module::binding const& binding = proc.module->bindings[*binding_index];
    return binding.tag;
  }
}

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
compile_arithmetic(context& ctx, procedure_context& proc, application_syntax const& stx, module::binding_tag tag) {
  if (stx.arguments.empty()) {
    if (tag == module::binding_tag::plus)
      return shared_register{ctx.statics.zero};
    else if (tag == module::binding_tag::times)
      return shared_register{ctx.statics.one};
    else
      throw std::runtime_error{fmt::format("Not enough arguments for {}",
                                           std::get<reference_syntax>(stx.target->value).symbol->value())};
  }

  opcode op;
  switch (tag) {
  case module::binding_tag::plus:
    op = opcode::add;
    break;
  case module::binding_tag::minus:
    op = opcode::subtract;
    break;
  case module::binding_tag::times:
    op = opcode::multiply;
    break;
  case module::binding_tag::divide:
    op = opcode::divide;
    break;
  default:
    assert(!"Invalid tag");
  }

  return compile_fold(ctx, proc, stx.arguments, op);
}

static shared_register
compile_relational(context& ctx, procedure_context& proc, application_syntax const& stx, module::binding_tag tag) {
  if (stx.arguments.size() < 2)
    throw std::runtime_error{fmt::format("Not enough arguments for {}",
                                         std::get<reference_syntax>(stx.target->value).symbol->value())};

  opcode op;
  switch (tag) {
  case module::binding_tag::arith_equal:  op = opcode::arith_equal; break;
  case module::binding_tag::less_than:    op = opcode::less_than; break;
  case module::binding_tag::greater_than: op = opcode::greater_than; break;
  default:
    assert(!"Unimplemented");
  }

  return compile_fold(ctx, proc, stx.arguments, op);
}

ptr<symbol>
compile_let_binding(context& ctx, ptr<pair> const& binding, std::string const& form_name) {
  if (!is<symbol>(binding->car()))
    throw std::runtime_error{fmt::format("Invalid {} syntax: Binding not a symbol", form_name)};

  auto name = assume<symbol>(binding->car());

  if (binding->cdr() == ctx.constants->null)
    throw std::runtime_error{fmt::format("Invalid {} syntax: No expression for symbol {}",
                                         form_name, name->value())};
  if (!is<pair>(binding->cdr()))
    throw std::runtime_error{fmt::format("Invalid {} syntax in binding definition", form_name)};

  return name;
}

static shared_register
compile_let(context& ctx, procedure_context& proc, let_syntax const& stx, bool tail) {
  symbol_bindings::scope scope;
  for (auto const& def : stx.definitions) {
    shared_register value = compile_expression(ctx, proc, *def.expression, false);
    scope.emplace_back(symbol_bindings::binding{def.name, std::move(value)});
  }

  symbol_bindings::unique_scope us = proc.bindings.push_scope(std::move(scope));
  return compile_body(ctx, proc, stx.body, tail);
}

static shared_register
compile_letrec(context& ctx, procedure_context& proc, letrec_syntax const& stx, bool tail) {
  struct parsed_binding {
    ptr<symbol> name;
    syntax const* expr;
    shared_register reg;
  };

  std::vector<parsed_binding> bindings;
  for (auto const& def : stx.definitions)
    bindings.push_back({def.name, def.expression.get(), {}});

  symbol_bindings::scope scope;
  for (parsed_binding& binding : bindings) {
    shared_register value = proc.registers.allocate_local();
    proc.bytecode.push_back({opcode::set, ctx.statics.void_, {}, *value});
    scope.emplace_back(symbol_bindings::binding{binding.name, value});
    binding.reg = std::move(value);
  }

  symbol_bindings::unique_scope us = proc.bindings.push_scope(std::move(scope));
  for (parsed_binding const& binding : bindings) {
    shared_register value = compile_expression(ctx, proc, *binding.expr, false);
    proc.bytecode.push_back({opcode::set, *value, {}, *binding.reg});
  }

  return compile_body(ctx, proc, stx.body, tail);
}

static void
emit_data(bytecode& bc, std::vector<shared_register> const& values) {
  instruction data{opcode::data, {}, {}, {}};
  for (std::size_t i = 0; i < values.size(); ++i) {
    switch (i % 3) {
    case 0: data.x = *values[i]; break;
    case 1: data.y = *values[i]; break;
    case 2: data.dest = *values[i]; break;
    default: assert(!"Can't get here");
    }

    if (i % 3 == 2) {
      bc.push_back(data);
      data.x = data.y = data.dest = {};
    }
  }

  if (values.size() % 3 != 0)
    bc.push_back(data);
}

static shared_register
compile_lambda(context& ctx, procedure_context& parent, lambda_syntax const& stx) {
  procedure_context proc{&parent, parent.module};
  symbol_bindings::scope args_scope;

  for (auto const& param : stx.parameters)
    args_scope.push_back(symbol_bindings::binding{param, proc.registers.allocate_local()});

  symbol_bindings::unique_scope us = proc.bindings.push_scope(std::move(args_scope));

  shared_register return_value = compile_body(ctx, proc, stx.body, true);
  if (return_value)
    proc.bytecode.push_back(instruction{opcode::ret, *return_value, {}, {}});

  ptr<procedure> p = ctx.store.make<procedure>(std::move(proc.bytecode),
                                               proc.registers.locals_used(),
                                               stx.parameters.size());
  operand p_reg = operand::static_(ctx.intern_static(p));

  if (!proc.bindings.free().empty()) {
    shared_register result = parent.registers.allocate_local();
    parent.bytecode.push_back(instruction{opcode::make_closure,
                                          p_reg,
                                          operand::immediate(proc.bindings.free().size()),
                                          *result});

    std::vector<ptr<symbol>> free_symbols;
    free_symbols.resize(proc.bindings.free().size());
    for (auto const& [sym, reg] : proc.bindings.free()) {
      assert(reg->value() < free_symbols.size());
      free_symbols[reg->value()] = assume<symbol>(sym);
    }

    std::vector<shared_register> closure;
    closure.reserve(free_symbols.size());
    for (ptr<symbol> const& sym : free_symbols)
      closure.push_back(compile_reference(parent, reference_syntax{sym}));

    emit_data(parent.bytecode, closure);

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
  if (auto* ref = std::get_if<reference_syntax>(&stx.target->value))
    if (std::optional<module::binding_tag> tag = find_tag(proc, ref->symbol)) {
      if (*tag == module::binding_tag::plus || *tag == module::binding_tag::minus
          || *tag == module::binding_tag::times || *tag == module::binding_tag::divide)
        return compile_arithmetic(ctx, proc, stx, *tag);
      else if (*tag == module::binding_tag::less_than || *tag == module::binding_tag::greater_than)
        return compile_relational(ctx, proc, stx, *tag);
    }

  std::vector<shared_register> arg_registers;
  for (auto const& arg : stx.arguments)
    arg_registers.push_back(compile_expression(ctx, proc, *arg, false));

  shared_register f = compile_expression(ctx, proc, *stx.target, false);
  shared_register result;
  if (!tail) {
    result = proc.registers.allocate_local();
    proc.bytecode.push_back({opcode::call, *f, operand::immediate(arg_registers.size()), *result});
  } else
    proc.bytecode.push_back({opcode::tail_call, *f, operand::immediate(arg_registers.size()), {}});

  emit_data(proc.bytecode, arg_registers);
  return result;
}

static shared_register
compile_reference(procedure_context& proc, reference_syntax const& stx) {
  if (shared_register r = proc.bindings.lookup(stx.symbol))
    return r;
  else {
    // Not found in the locals of this procedure. Could be found in outer
    // procedures, in which case it will be captured into the closure, or it
    // could be found in the containing module or one of its imports.

    for (procedure_context* parent = proc.parent; parent; parent = parent->parent)
      if (parent->bindings.lookup(stx.symbol)) {
        shared_register result = proc.registers.allocate_closure();
        proc.bindings.add_free(stx.symbol, result);
        return result;
      }

    std::optional<std::size_t> binding_index = find_global(proc.module, stx.symbol);
    if (!binding_index)
      throw std::runtime_error{fmt::format("Unbound identifier {}", stx.symbol->value())};

    module::binding const& binding = proc.module->bindings[*binding_index];
    if (!binding.index)
      throw std::runtime_error{fmt::format("Invalid use of {} as value", stx.symbol->value())};

    return shared_register{operand::global(*binding_index)};
  }
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

// Translate an expression and return the register where the result is stored.
static shared_register
compile_expression(context& ctx, procedure_context& proc, syntax const& stx, bool tail) {
  if (auto* lit = std::get_if<literal_syntax>(&stx.value))
    return shared_register{operand::static_(ctx.intern_static(lit->value))};
  else if (auto* ref = std::get_if<reference_syntax>(&stx.value))
    return compile_reference(proc, *ref);
  else if (auto* app = std::get_if<application_syntax>(&stx.value))
    return compile_application(ctx, proc, *app, tail);
  else if (auto* let = std::get_if<let_syntax>(&stx.value))
    return compile_let(ctx, proc, *let, tail);
  else if (auto* letrec = std::get_if<letrec_syntax>(&stx.value))
    return compile_letrec(ctx, proc, *letrec, tail);
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
  else {
    assert(!"Unexpected syntax");
    return {};
  }
}

static shared_register
compile_body(context& ctx, procedure_context& proc, body_syntax const& stx, bool tail) {
  shared_register result;
  for (auto expr = stx.expressions.begin(); expr != stx.expressions.end(); ++expr) {
    bool last = std::next(expr) == stx.expressions.end();
    result = compile_expression(ctx, proc, **expr, tail && last);
  }

  return result;
}

ptr<procedure>
compile_expression(context& ctx, generic_ptr const& datum, ptr<module> const& mod) {
  auto stx = analyse(ctx, datum);

  procedure_context proc{nullptr, mod};
  shared_register result = compile_expression(ctx, proc, *stx, true);
  if (result)
    proc.bytecode.push_back(instruction{opcode::ret, *result, {}, {}});
  return ctx.store.make<procedure>(std::move(proc.bytecode),
                                   proc.registers.locals_used(),
                                   0);
}

} // namespace game::scm
