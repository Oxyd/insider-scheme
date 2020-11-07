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

  define_lambda<void(context&, object*)>(
    ctx, result, "write-simple", true,
    [] (context& ctx, object* datum) {
      write_simple(ctx, datum, ctx.output_port.get());
    }
  );

  define_lambda<void(context&, object*)>(
    ctx, result, "display", true,
    [] (context& ctx, object* datum) {
      display(ctx, datum, ctx.output_port.get());
    }
  );

  define_lambda<void(context&)>(
    ctx, result, "newline", true,
    [] (context& ctx) { ctx.output_port->write_char('\n'); }
  );

  define_top_level(ctx, result, "append", make<native_procedure>(ctx, append, "append"), true);
  define_lambda<vector*(context&, object*)>(ctx, result, "list->vector", true, list_to_vector);
  define_top_level(ctx, result, "vector-append", make<native_procedure>(ctx, vector_append, "vector-append"), true);
  define_lambda<integer(vector*)>(
    ctx, result, "vector-length", true,
    [] (vector* v) {
      return integer{v->size()};
    }
  );
  define_raw_lambda(ctx, result, "vector", true,
                    static_cast<vector* (&)(context&, std::vector<object*> const&)>(make_vector));
  define_lambda<vector*(context&, std::size_t)>(
    ctx, result, "make-vector", true,
    [] (context& ctx, std::size_t len) {
      return make<vector>(ctx, ctx, len);
    }
  );
  define_lambda<&vector::ref>(ctx, result, "vector-ref", true);
  define_lambda<void(context&, vector*, std::size_t, object*)>(
    ctx, result, "vector-set!", true,
    [] (context& ctx, vector* v, std::size_t i, object* o) {
      v->set(ctx.store, i, o);
    }
  );

  define_lambda<cons>(ctx, result, "cons", true);
  define_lambda<object*(pair*)>(ctx, result, "car", true, static_cast<object* (*)(pair*)>(car));
  define_lambda<object*(pair*)>(ctx, result, "cdr", true, static_cast<object* (*)(pair*)>(cdr));
  define_lambda<object*(pair*)>(ctx, result, "cadr", true, cadr);
  define_lambda<object*(pair*)>(ctx, result, "caddr", true, caddr);
  define_lambda<object*(pair*)>(ctx, result, "cadddr", true, cadddr);
  define_lambda<object*(pair*)>(ctx, result, "cddr", true, cddr);
  define_lambda<object*(pair*)>(ctx, result, "cdddr", true, cdddr);

  define_raw_lambda(
    ctx, result, "make-string", true,
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

  define_lambda<integer(string*)>(
    ctx, result, "string-length", true,
    [] (string* s) {
      return integer{s->size()};
    }
  );

  define_top_level(ctx, result, "string-append",
                   make<native_procedure>(ctx,
                                          [] (context& ctx, std::vector<object*> const& args) {
                                            std::string result;
                                            for (object* s : args)
                                              result += expect<string>(s)->value();
                                            return make_string(ctx, result);
                                          },
                                          "string-append"),
                   true);

  define_lambda<string*(context&, object*)>(
    ctx, result, "number->string", true,
    [] (context& ctx, object* num) {
      if (!is_number(num))
        throw error{"Not a number: {}", datum_to_string(ctx, num)};
      return datum_to_string(ctx, num);
    }
  );

  define_lambda<string*(context&, object*)>(
    ctx, result, "datum->string", true,
    [] (context& ctx, object* datum) {
      return datum_to_string(ctx, datum);
    }
  );

  define_lambda<string*(context&, object*)>(
    ctx, result, "symbol->string", true,
    [] (context& ctx, object* datum) {
      return make_string(ctx, expect<symbol>(datum)->value());
    }
  );

  define_lambda<syntactic_closure*(context&, environment*, object*, object*)>(
    ctx, result, "make-syntactic-closure", true,
    [] (context& ctx, environment* env, object* free, object* form) {
      return make<syntactic_closure>(ctx, env, form, free);
    }
  );

  define_lambda<&syntactic_closure::expression>(ctx, result, "syntactic-closure-expression", true);
  define_lambda<&syntactic_closure::environment>(ctx, result, "syntactic-closure-environment", true);

  define_lambda<type>(ctx, result, "type", true);

  define_lambda<boolean*(context&, object*, object*)>(
    ctx, result, "eq?", true,
    [] (context& ctx, object* x, object* y) {
      return x == y ? ctx.constants->t.get() : ctx.constants->f.get();
    }
  );

  define_lambda<eqv>(ctx, result, "eqv?", true);
  define_lambda<equal>(ctx, result, "equal?", true);

  define_lambda<vector*(context&, procedure*)>(
    ctx, result, "procedure-bytecode", true,
    [] (context& ctx, procedure* f) {
      bytecode_decoder dec{f->bytecode};
      std::vector<std::tuple<std::size_t, std::size_t, instruction>> instrs;
      while (!dec.done()) {
        std::size_t pos = dec.position();
        instruction instr = read_instruction(dec);

        instrs.emplace_back(pos, dec.position() - pos, instr);
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

  define_lambda<object*(context&, procedure*)>(
    ctx, result, "procedure-name", true,
    [] (context& ctx, procedure* f) -> object* {
      if (f->name)
        return make_string(ctx, *f->name);
      else
        return ctx.constants->f.get();
    }
  );

  define_lambda<&closure::procedure>(ctx, result, "closure-procedure", true);

  define_lambda<integer(opaque_value<instruction>*)>(
    ctx, result, "instruction-opcode", true,
    [] (opaque_value<instruction>* i) {
      return integer{static_cast<integer::storage_type>(i->value.opcode)};
    }
  );

  define_lambda<object*(context&, opaque_value<instruction>*)>(
    ctx, result, "instruction-operands", true,
    [] (context& ctx, opaque_value<instruction>* i) {
      instruction instr = i->value;
      return make_list_from_vector(ctx, instr.operands,
                                   [&] (operand o) { return integer_to_ptr(o); });
    }
  );

  define_top_level(ctx, result, "opcodes",
                   make_vector(ctx, opcode_value_to_info,
                               [&] (instruction_info const& info) {
                                 return ctx.intern(info.mnemonic);
                               }),
                   true);

  define_lambda<string*(context&, operand)>(
    ctx, result, "top-level-name", true,
    [] (context& ctx, operand op) {
      return make_string(ctx, ctx.get_top_level_name(op));
    }
  );

  define_lambda<object*(context&, operand)>(
    ctx, result, "static-value", true,
    [] (context& ctx, operand op) {
      return ctx.get_static_checked(op);
    }
  );

  define_lambda<object*(context&, operand)>(
    ctx, result, "top-level-value", true,
    [] (context& ctx, operand op) {
      return ctx.get_top_level_checked(op);
    }
  );

  define_lambda<void(context&, boolean*)>(
    ctx, result, "set-verbose-collection!", true,
    [] (context& ctx, boolean* value) {
      ctx.store.verbose_collection = value->value();
    }
  );

  return result;
}

} // namespace insider
