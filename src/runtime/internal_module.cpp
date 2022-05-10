#include "internal_module.hpp"

#include "compiler/analyser.hpp"
#include "io/write.hpp"
#include "runtime/syntax.hpp"
#include "util/define_procedure.hpp"

#ifndef WIN32
#include <csignal>
#else
#include <intrin.h>
#endif

namespace insider {

void
export_numeric(context&, ptr<module_>);

void
export_vm(context&, ptr<module_>);

void
export_records(context&, ptr<module_>);

void
export_write(context&, ptr<module_>);

void
export_analyser(context&, ptr<module_>);

void
export_port(context&, ptr<module_>);

void
export_basic_types(context&, ptr<module_>);

void
export_read(context&, ptr<module_>);

void
export_source_code_provider(context&, ptr<module_>);

void
export_syntax(context&, ptr<module_>);

void
export_time(context&, ptr<module_>);

void
export_error(context&, ptr<module_>);

void
export_string(context&, ptr<module_>);

void
export_character(context&, ptr<module_>);

static void
set_car(context& ctx, ptr<pair> p, ptr<> new_car) {
  p->set_car(ctx.store, new_car);
}

static void
set_cdr(context& ctx, ptr<pair> p, ptr<> new_cdr) {
  p->set_cdr(ctx.store, new_cdr);
}

static ptr<>
eq(context& ctx, ptr<> x, ptr<> y) {
  return x == y ? ctx.constants->t : ctx.constants->f;
}

static bool
known_module(context& ctx, ptr<syntax> name) {
  return ctx.module_resolver().knows_module(ctx, parse_module_name(ctx, name));
}

static ptr<>
procedure_bytecode(context& ctx, ptr<procedure> f) {
  integer::value_type pc = f->entry_pc;
  std::vector<std::tuple<std::size_t, std::size_t, instruction>> instrs;

  while (pc < f->entry_pc + static_cast<integer::value_type>(f->bytecode_size)) {
    integer::value_type pos = pc;
    instruction instr = read_instruction(ctx.program, pc);

    instrs.emplace_back(pos, pc - pos, instr);
  }

  return make_list_from_vector(
    ctx, instrs,
    [&] (std::tuple<std::size_t, std::size_t, instruction> i) {
      return make_list(
        ctx,
        integer_to_ptr(static_cast<integer::value_type>(std::get<0>(i))),
        integer_to_ptr(static_cast<integer::value_type>(std::get<1>(i))),
        make<opaque_value<instruction>>(ctx, std::get<2>(i))
      );
    }
  );
}

static ptr<>
procedure_name(context& ctx, ptr<procedure> f) {
  if (f->name)
    return make<string>(ctx, *f->name);
  else
    return ctx.constants->f;
}

static integer
instruction_opcode(ptr<opaque_value<instruction>> i) {
  return integer{static_cast<integer::value_type>(i->value.opcode)};
}

static ptr<>
instruction_operands(context& ctx, ptr<opaque_value<instruction>> i) {
  instruction instr = i->value;
  return make_list_from_vector(ctx, instr.operands,
                               [&] (operand o) { return integer_to_ptr(o); });
}

static ptr<string>
top_level_name(context& ctx, operand op) {
  return make<string>(ctx, ctx.get_top_level_name(op));
}

static ptr<>
static_value(context& ctx, operand op) {
  return ctx.get_static_checked(op);
}

static ptr<>
top_level_value(context& ctx, operand op) {
  return ctx.get_top_level_checked(op);
}

static void
set_verbose_colleection(context& ctx, ptr<boolean> value) {
  ctx.store.verbose_collection = value->value();
}

static void
trap([[maybe_unused]] ptr<> value) {
#ifndef WIN32
  raise(SIGTRAP);
#else
  __debugbreak();
#endif
}

static ptr<>
features(context& ctx) {
  return ctx.features();
}

template <auto F>
static void
define_tagged_procedure(context& ctx, char const* name, ptr<module_> result,
                        special_top_level_tag tag) {
  operand index = define_procedure<F>(ctx, name, result, true);
  ctx.tag_top_level(index, tag);
}

ptr<module_>
make_internal_module(context& ctx) {
  auto result = make<module_>(ctx, ctx, module_name{"insider", "internal"});
  result->mark_active();

  export_numeric(ctx, result);
  export_vm(ctx, result);
  export_records(ctx, result);
  export_analyser(ctx, result);
  export_write(ctx, result);
  export_port(ctx, result);
  export_basic_types(ctx, result);
  export_read(ctx, result);
  export_source_code_provider(ctx, result);
  export_syntax(ctx, result);
  export_time(ctx, result);
  export_error(ctx, result);
  export_string(ctx, result);
  export_character(ctx, result);

  define_raw_procedure<append>(ctx, "append", result, true);
  define_tagged_procedure<cons>(ctx, "cons", result,
                                special_top_level_tag::cons);
  define_tagged_procedure<static_cast<ptr<> (*)(ptr<pair>)>(car)>(
    ctx, "car", result, special_top_level_tag::car
  );
  define_tagged_procedure<static_cast<ptr<> (*)(ptr<pair>)>(cdr)>(
    ctx, "cdr", result, special_top_level_tag::cdr
  );
  define_procedure<cadr>(ctx, "cadr", result, true);
  define_procedure<caddr>(ctx, "caddr", result, true);
  define_procedure<cadddr>(ctx, "cadddr", result, true);
  define_procedure<cddr>(ctx, "cddr", result, true);
  define_procedure<cdddr>(ctx, "cdddr", result, true);
  define_procedure<set_car>(ctx, "set-car!", result, true);
  define_procedure<set_cdr>(ctx, "set-cdr!", result, true);

  define_tagged_procedure<type>(ctx, "type", result,
                                special_top_level_tag::type);

  define_tagged_procedure<eq>(ctx, "eq?", result, special_top_level_tag::eq);
  define_procedure<eqv>(ctx, "eqv?", result, true);
  define_procedure<equal>(ctx, "equal?", result, true);

  define_procedure<features>(ctx, "features", result, true);
  define_procedure<known_module>(ctx, "known-module?", result, true);

  define_procedure<procedure_bytecode>(ctx, "procedure-bytecode", result, true);
  define_procedure<procedure_name>(ctx, "procedure-name", result, true);
  define_procedure<&closure::procedure>(ctx, "closure-procedure", result, true);

  define_procedure<instruction_opcode>(ctx, "instruction-opcode", result, true);
  define_procedure<instruction_operands>(ctx, "instruction-operands", result,
                                         true);
  define_top_level(ctx, "opcodes", result, true,
                   make_vector(ctx, opcode_value_to_info,
                               [&] (instruction_info const& info) {
                                 return ctx.intern(info.mnemonic);
                               }));

  define_procedure<top_level_name>(ctx, "top-level-name", result, true);
  define_procedure<static_value>(ctx, "static-value", result, true);
  define_procedure<top_level_value>(ctx, "top-level-value", result, true);

  define_procedure<set_verbose_colleection>(ctx, "set-verbose-collection!",
                                            result, true);

  define_procedure<trap>(ctx, "trap!", result, true);

  define_top_level(ctx, "<eof-object>", result, true, ctx.constants->eof);

  return result;
}

} // namespace insider
