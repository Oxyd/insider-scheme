#include "internal_module.hpp"

#include "analyser.hpp"
#include "define_procedure.hpp"
#include "write.hpp"

#ifndef WIN32
#include <csignal>
#else
#include <intrin.h>
#endif

namespace insider {

void
export_numeric(context&, module_&);

void
export_vm(context&, module_&);

void
export_records(context&, module_&);

void
export_write(context&, module_&);

void
export_analyser(context&, module_&);

void
export_port(context&, module_&);

void
export_basic_types(context&, module_&);

void
export_read(context&, module_&);

void
export_source_code_provider(context&, module_&);

void
export_syntax(context&, module_&);

void
export_time(context&, module_&);

void
export_error(context&, module_&);

void
export_string(context&, module_&);

void
export_character(context&, module_&);

module_
make_internal_module(context& ctx) {
  module_ result{ctx};
  result.mark_active();

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

  define_raw_procedure(ctx, "append", result, true, append);
  define_procedure(ctx, "cons", result, true, cons);
  define_procedure(ctx, "car", result, true, static_cast<ptr<> (*)(ptr<pair>)>(car));
  define_procedure(ctx, "cdr", result, true, static_cast<ptr<> (*)(ptr<pair>)>(cdr));
  define_procedure(ctx, "cadr", result, true, cadr);
  define_procedure(ctx, "caddr", result, true, caddr);
  define_procedure(ctx, "cadddr", result, true, cadddr);
  define_procedure(ctx, "cddr", result, true, cddr);
  define_procedure(ctx, "cdddr", result, true, cdddr);
  define_procedure(ctx, "set-car!", result, true,
                   [] (context& ctx, ptr<pair> p, ptr<> new_car) {
                     p->set_car(ctx.store, new_car);
                   });
  define_procedure(ctx, "set-cdr!", result, true,
                   [] (context& ctx, ptr<pair> p, ptr<> new_cdr) {
                     p->set_cdr(ctx.store, new_cdr);
                   });

  operand type_index = define_procedure(ctx, "type", result, true, type);
  ctx.tag_top_level(type_index, special_top_level_tag::type);

  define_procedure(
    ctx, "eq?", result, true,
    [] (context& ctx, ptr<> x, ptr<> y) {
      return x == y ? ctx.constants->t.get() : ctx.constants->f.get();
    }
  );

  define_procedure(ctx, "eqv?", result, true, eqv);
  define_procedure(ctx, "equal?", result, true, equal);

  define_procedure(ctx, "features", result, true,
                   [] (context& ctx) { return ctx.features(); });

  define_procedure(ctx, "known-module?", result, true,
                   [] (context& ctx, ptr<syntax> name) {
                     return ctx.knows_module(parse_module_name(ctx, name));
                   });

  define_procedure(
    ctx, "procedure-bytecode", result, true,
    [] (context& ctx, ptr<procedure> f) {
      integer::value_type pc = f->entry_pc;
      std::vector<std::tuple<std::size_t, std::size_t, instruction>> instrs;

      while (pc < f->entry_pc + static_cast<integer::value_type>(f->bytecode_size)) {
        integer::value_type pos = pc;
        instruction instr = read_instruction(ctx.program, pc);

        instrs.emplace_back(pos, pc - pos, instr);
      }

      return make_list_from_vector(ctx, instrs,
                                   [&] (std::tuple<std::size_t, std::size_t, instruction> i) {
                                     return make_list(ctx,
                                                      integer_to_ptr(std::get<0>(i)),
                                                      integer_to_ptr(std::get<1>(i)),
                                                      make<opaque_value<instruction>>(ctx, std::get<2>(i)));
                                   });
    }
  );

  define_procedure(
    ctx, "procedure-name", result, true,
    [] (context& ctx, ptr<procedure> f) -> ptr<> {
      if (f->name)
        return make<string>(ctx, *f->name);
      else
        return ctx.constants->f.get();
    }
  );

  define_procedure(ctx, "closure-procedure", result, true, &closure::procedure);

  define_procedure(
    ctx, "instruction-opcode", result, true,
    [] (ptr<opaque_value<instruction>> i) {
      return integer{static_cast<integer::value_type>(i->value.opcode)};
    }
  );

  define_procedure(
    ctx, "instruction-operands", result, true,
    [] (context& ctx, ptr<opaque_value<instruction>> i) {
      instruction instr = i->value;
      return make_list_from_vector(ctx, instr.operands,
                                   [&] (operand o) { return integer_to_ptr(o); });
    }
  );

  define_top_level(ctx, "opcodes", result, true,
                   make_vector(ctx, opcode_value_to_info,
                               [&] (instruction_info const& info) {
                                 return ctx.intern(info.mnemonic);
                               }));

  define_procedure(
    ctx, "top-level-name", result, true,
    [] (context& ctx, operand op) {
      return make<string>(ctx, ctx.get_top_level_name(op));
    }
  );

  define_procedure(
    ctx, "static-value", result, true,
    [] (context& ctx, operand op) {
      return ctx.get_static_checked(op);
    }
  );

  define_procedure(
    ctx, "top-level-value", result, true,
    [] (context& ctx, operand op) {
      return ctx.get_top_level_checked(op);
    }
  );

  define_procedure(
    ctx, "set-verbose-collection!", result, true,
    [] (context& ctx, ptr<boolean> value) {
      ctx.store.verbose_collection = value->value();
    }
  );

  define_procedure(ctx, "trap!", result, true,
                   [] ([[maybe_unused]] ptr<> value) {
#ifndef WIN32
                     raise(SIGTRAP);
#else
                     __debugbreak();
#endif
                   });

  define_top_level(ctx, "<eof-object>", result, true, ctx.constants->eof.get());

  return result;
}

} // namespace insider
