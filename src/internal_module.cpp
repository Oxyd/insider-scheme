#include "internal_module.hpp"

#include "analyser.hpp"
#include "define_procedure.hpp"
#include "write.hpp"

namespace insider {

void
export_numeric(context&, module&);

void
export_vm(context&, module&);

void
export_records(context&, module&);

void
export_write(context&, module&);

void
export_analyser(context&, module&);

void
export_port(context&, module&);

void
export_basic_types(context&, module&);

void
export_read(context&, module&);

void
export_source_code_provider(context&, module&);

void
export_syntax(context&, module&);

static ptr<symbol>
type(context& ctx, ptr<> o) {
  if (is_object_ptr(o))
    return ctx.intern(object_type(o).name);
  else
    return ctx.intern(integer_type_name);
}

module
make_internal_module(context& ctx) {
  module result{ctx};
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

  define_raw_procedure(ctx, "append", result, true, append);
  define_procedure(ctx, "list->vector", result, true, list_to_vector);
  define_procedure(ctx, "vector->list", result, true, vector_to_list);
  define_raw_procedure(ctx, "vector-append", result, true, vector_append);
  define_procedure(
    ctx, "vector-length", result, true,
    [] (ptr<vector> v) {
      return integer{static_cast<integer::value_type>(v->size())};
    }
  );
  define_procedure(ctx, "vector", result, true,
                   static_cast<ptr<vector> (*)(context&, std::vector<ptr<>> const&)>(make_vector));
  define_procedure(
    ctx, "make-vector", result, true,
    [] (context& ctx, std::size_t len) {
      return make<vector>(ctx, ctx, len);
    }
  );
  operand vector_ref_index = define_procedure(ctx, "vector-ref", result, true, &vector::ref);
  ctx.tag_top_level(vector_ref_index, special_top_level_tag::vector_ref);

  operand vector_set_index = define_procedure(
    ctx, "vector-set!", result, true,
    [] (context& ctx, ptr<vector> v, std::size_t i, ptr<> o) {
      v->set(ctx.store, i, o);
    }
  );
  ctx.tag_top_level(vector_set_index, special_top_level_tag::vector_set);

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

  define_raw_procedure(
    ctx, "make-string", result, true,
    [] (context& ctx, object_span args) {
      if (args.size() < 1)
        throw std::runtime_error{"make-string: Expected at least 1 argument"};
      if (args.size() > 2)
        throw std::runtime_error{"make-string: Expected at most 2 arguments"};

      integer::value_type length = expect<integer>(args[0]).value();
      if (length < 0)
        throw std::runtime_error{"make-string: Length cannot be negative"};

      auto result = make<string>(ctx, length);

      if (args.size() == 2) {
        char32_t fill = expect<char32_t>(args[1]);
        for (std::size_t i = 0; i < static_cast<std::size_t>(length); ++i)
          result->set(i, fill);
      }

      return result;
    }
  );

  define_procedure(
    ctx, "string-length", result, true,
    [] (ptr<string> s) {
      return integer{static_cast<integer::value_type>(s->length())};
    }
  );

  define_raw_procedure(ctx, "string-append", result, true,
                       [] (context& ctx, object_span args) {
                         std::string result;
                         for (ptr<> s : args)
                           result += expect<string>(s)->value();
                         return make<string>(ctx, result);
                       });

  define_procedure(
    ctx, "number->string", result, true,
    [] (context& ctx, ptr<> num) {
      if (!is_number(num))
        throw make_error("Not a number: {}", datum_to_string(ctx, num));
      return datum_to_string(ctx, num);
    }
  );

  define_procedure(
    ctx, "datum->string", result, true,
    [] (context& ctx, ptr<> datum) {
      return datum_to_string(ctx, datum);
    }
  );

  define_procedure(
    ctx, "symbol->string", result, true,
    [] (context& ctx, ptr<> datum) {
      return make<string>(ctx, expect<symbol>(datum)->value());
    }
  );

  define_procedure(ctx, "type", result, true, type);

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

  return result;
}

} // namespace insider
