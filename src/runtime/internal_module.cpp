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

void
export_module(context&, ptr<module_>);

void
export_parser_expander(context&, ptr<module_>);

void
export_ast(context&, ptr<module_>);

void
export_variable(context&, ptr<module_>);

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
procedure_bytecode(context& ctx, ptr<procedure_prototype> proto) {
  instruction_pointer ip = proto->code.get();
  std::vector<std::tuple<std::size_t, std::size_t, instruction>> instrs;

  while (ip != proto->code.get() + proto->code_size) {
    instruction_pointer old_ip = ip;
    instruction instr = read_instruction(ip);

    std::size_t pos = old_ip - proto->code.get();
    std::size_t size = ip - old_ip;

    instrs.emplace_back(pos, size, instr);
  }

  return make_list_from_range(
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

static std::string
procedure_name(ptr<procedure_prototype> f) {
  return f->name;
}

static ptr<vector>
procedure_constants(context& ctx, ptr<procedure_prototype> f) {
  return make_vector(ctx, f->constants);
}

static integer
instruction_opcode(ptr<opaque_value<instruction>> i) {
  return integer{static_cast<integer::value_type>(i->value.opcode)};
}

static ptr<>
instruction_operands(context& ctx, ptr<opaque_value<instruction>> i) {
  instruction instr = i->value;
  return make_list_from_range(ctx, instruction_operands_vector(instr),
                               [&] (operand o) { return integer_to_ptr(o); });
}

static ptr<string>
top_level_name(context& ctx, operand op) {
  return make<string>(ctx, ctx.get_top_level_name(op));
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
  export_module(ctx, result);
  export_parser_expander(ctx, result);
  export_ast(ctx, result);
  export_variable(ctx, result);

  define_raw_procedure<append>(ctx, "append", result);
  define_tagged_procedure<cons>(ctx, "cons", result,
                                special_top_level_tag::cons);
  define_tagged_procedure<static_cast<ptr<> (*)(ptr<pair>)>(car)>(
    ctx, "car", result, special_top_level_tag::car, true
  );
  define_tagged_procedure<static_cast<ptr<> (*)(ptr<pair>)>(cdr)>(
    ctx, "cdr", result, special_top_level_tag::cdr, true
  );
  define_constant_evaluable_procedure<cadr>(ctx, "cadr", result);
  define_constant_evaluable_procedure<caddr>(ctx, "caddr", result);
  define_constant_evaluable_procedure<cadddr>(ctx, "cadddr", result);
  define_constant_evaluable_procedure<cddr>(ctx, "cddr", result);
  define_constant_evaluable_procedure<cdddr>(ctx, "cdddr", result);
  define_procedure<set_car>(ctx, "set-car!", result);
  define_procedure<set_cdr>(ctx, "set-cdr!", result);

  define_tagged_procedure<type>(ctx, "type", result,
                                special_top_level_tag::type,
                                true);

  define_tagged_procedure<eq>(ctx, "eq?", result, special_top_level_tag::eq,
                              true);
  define_constant_evaluable_procedure<eqv>(ctx, "eqv?", result);
  define_constant_evaluable_procedure<equal>(ctx, "equal?", result);

  define_procedure<features>(ctx, "features", result);
  define_procedure<known_module>(ctx, "known-module?", result);

  define_procedure<procedure_bytecode>(ctx, "procedure-prototype-bytecode",
                                       result);
  define_procedure<procedure_name>(ctx, "procedure-prototype-name", result);
  define_procedure<procedure_constants>(ctx, "procedure-prototype-constants",
                                        result);
  define_procedure<&procedure::prototype_ptr>(ctx, "procedure-prototype",
                                              result);

  define_procedure<instruction_opcode>(ctx, "instruction-opcode", result);
  define_procedure<instruction_operands>(ctx, "instruction-operands", result);
  define_top_level(ctx, "opcodes", result, true,
                   make_vector(ctx, opcode_value_to_info,
                               [&] (instruction_info const& info) {
                                 return ctx.intern(info.mnemonic);
                               }));
  define_top_level(ctx, "immediate-bias", result, true,
                   integer_to_ptr(immediate_bias));

  define_procedure<top_level_name>(ctx, "top-level-name", result);
  define_procedure<top_level_value>(ctx, "top-level-value", result);

  define_procedure<set_verbose_colleection>(ctx, "set-verbose-collection!",
                                            result);

  define_procedure<trap>(ctx, "trap!", result);

  define_top_level(ctx, "<eof-object>", result, true, ctx.constants->eof);

  return result;
}

} // namespace insider
