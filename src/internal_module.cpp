#include "internal_module.hpp"

#include "converters.hpp"
#include "io.hpp"

namespace insider {

static symbol*
type(context& ctx, object* o) {
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

  define_procedure(
    ctx, "write-simple", result, true,
    [] (context& ctx, object* datum) {
      write_simple(ctx, datum, ctx.output_port.get());
    }
  );

  define_procedure(
    ctx, "display", result, true,
    [] (context& ctx, object* datum) {
      display(ctx, datum, ctx.output_port.get());
    }
  );

  define_procedure(
    ctx, "newline", result, true,
    [] (context& ctx) { ctx.output_port->write_char('\n'); }
  );

  define_raw_procedure(ctx, "append", result, true, append);
  define_procedure(ctx, "list->vector", result, true, list_to_vector);
  define_raw_procedure(ctx, "vector-append", result, true, vector_append);
  define_procedure(
    ctx, "vector-length", result, true,
    [] (vector* v) {
      return integer{v->size()};
    }
  );
  define_procedure(ctx, "vector", result, true,
                   static_cast<vector* (*)(context&, std::vector<object*> const&)>(make_vector));
  define_procedure(
    ctx, "make-vector", result, true,
    [] (context& ctx, std::size_t len) {
      return make<vector>(ctx, ctx, len);
    }
  );
  define_procedure(ctx, "vector-ref", result, true, &vector::ref);
  define_procedure(
    ctx, "vector-set!", result, true,
    [] (context& ctx, vector* v, std::size_t i, object* o) {
      v->set(ctx.store, i, o);
    }
  );

  define_procedure(ctx, "cons", result, true, cons);
  define_procedure(ctx, "car", result, true, static_cast<object* (*)(pair*)>(car));
  define_procedure(ctx, "cdr", result, true, static_cast<object* (*)(pair*)>(cdr));
  define_procedure(ctx, "cadr", result, true, cadr);
  define_procedure(ctx, "caddr", result, true, caddr);
  define_procedure(ctx, "cadddr", result, true, cadddr);
  define_procedure(ctx, "cddr", result, true, cddr);
  define_procedure(ctx, "cdddr", result, true, cdddr);

  define_raw_procedure(
    ctx, "make-string", result, true,
    [] (context& ctx, std::vector<object*> const& args) {
      if (args.size() < 1)
        throw error{"make-string: Expected at least 1 argument"};
      if (args.size() > 2)
        throw error{"make-string: Expected at most 2 arguments"};

      integer::value_type length = expect<integer>(args[0]).value();
      if (length < 0)
        throw error{"make-string: Length cannot be negative"};

      auto result = make<string>(ctx, length);

      if (args.size() == 2) {
        character* fill = expect<character>(args[1]);
        for (std::size_t i = 0; i < static_cast<std::size_t>(length); ++i)
          result->set(i, fill->value());
      }

      return result;
    }
  );

  define_procedure(
    ctx, "string-length", result, true,
    [] (string* s) {
      return integer{s->size()};
    }
  );

  define_raw_procedure(ctx, "string-append", result, true,
                       [] (context& ctx, std::vector<object*> const& args) {
                         std::string result;
                         for (object* s : args)
                           result += expect<string>(s)->value();
                         return make_string(ctx, result);
                       });

  define_procedure(
    ctx, "number->string", result, true,
    [] (context& ctx, object* num) {
      if (!is_number(num))
        throw error{"Not a number: {}", datum_to_string(ctx, num)};
      return datum_to_string(ctx, num);
    }
  );

  define_procedure(
    ctx, "datum->string", result, true,
    [] (context& ctx, object* datum) {
      return datum_to_string(ctx, datum);
    }
  );

  define_procedure(
    ctx, "symbol->string", result, true,
    [] (context& ctx, object* datum) {
      return make_string(ctx, expect<symbol>(datum)->value());
    }
  );

  define_procedure(
    ctx, "make-syntactic-closure", result, true,
    [] (context& ctx, environment* env, object* free, object* form) {
      return make<syntactic_closure>(ctx, env, form, free);
    }
  );

  define_procedure(ctx, "syntactic-closure-expression", result, true, &syntactic_closure::expression);
  define_procedure(ctx, "syntactic-closure-environment", result, true, &syntactic_closure::environment);

  define_procedure(ctx, "type", result, true, type);

  define_procedure(
    ctx, "eq?", result, true,
    [] (context& ctx, object* x, object* y) {
      return x == y ? ctx.constants->t.get() : ctx.constants->f.get();
    }
  );

  define_procedure(ctx, "eqv?", result, true, eqv);
  define_procedure(ctx, "equal?", result, true, equal);

  define_procedure(
    ctx, "procedure-bytecode", result, true,
    [] (context& ctx, procedure* f) {
      std::size_t pc = 0;
      std::vector<std::tuple<std::size_t, std::size_t, instruction>> instrs;
      while (pc < f->bytecode.size()) {
        std::size_t pos = pc;
        instruction instr = read_instruction(f->bytecode, pc);

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
    [] (context& ctx, procedure* f) -> object* {
      if (f->name)
        return make_string(ctx, *f->name);
      else
        return ctx.constants->f.get();
    }
  );

  define_procedure(ctx, "closure-procedure", result, true, &closure::procedure);

  define_procedure(
    ctx, "instruction-opcode", result, true,
    [] (opaque_value<instruction>* i) {
      return integer{static_cast<integer::storage_type>(i->value.opcode)};
    }
  );

  define_procedure(
    ctx, "instruction-operands", result, true,
    [] (context& ctx, opaque_value<instruction>* i) {
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
      return make_string(ctx, ctx.get_top_level_name(op));
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
    [] (context& ctx, boolean* value) {
      ctx.store.verbose_collection = value->value();
    }
  );

  return result;
}

} // namespace insider