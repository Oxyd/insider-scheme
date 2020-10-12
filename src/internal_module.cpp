#include "internal_module.hpp"

#include "converters.hpp"
#include "io.hpp"

namespace insider {

static ptr<symbol>
type(context& ctx, generic_ptr const& x) {
  return ctx.intern(object_type(x.get()).name);
}

module
make_internal_module(context& ctx) {
  module result{ctx};
  result.mark_active();

  export_numeric(ctx, result);

  define_lambda<void(context&, generic_ptr const&)>(
    ctx, result, "write-simple", true,
    [] (context& ctx, generic_ptr const& datum) {
      write_simple(ctx, datum, ctx.output_port);
    }
  );

  define_lambda<void(context&, generic_ptr const&)>(
    ctx, result, "display", true,
    [] (context& ctx, generic_ptr const& datum) {
      display(ctx, datum, ctx.output_port);
    }
  );

  define_lambda<void(context&)>(
    ctx, result, "newline", true,
    [] (context& ctx) { ctx.output_port->write_char('\n'); }
  );

  define_top_level(ctx, result, "append", make<native_procedure>(ctx, append, "append"), true);
  define_lambda<ptr<vector>(context&, generic_ptr const&)>(ctx, result, "list->vector", true, list_to_vector);
  define_top_level(ctx, result, "vector-append", make<native_procedure>(ctx, vector_append, "vector-append"), true);
  define_lambda<integer(ptr<vector> const&)>(
    ctx, result, "vector-length", true,
    [] (ptr<vector> const& v) {
      return integer{v->size()};
    }
  );
  define_raw_lambda(ctx, result, "vector", true,
                    static_cast<ptr<vector> (&)(context&, std::vector<generic_ptr> const&)>(make_vector));
  define_lambda<ptr<vector>(context&, std::size_t)>(
    ctx, result, "make-vector", true,
    [] (context& ctx, std::size_t len) {
      return make<vector>(ctx, ctx, len);
    }
  );
  define_lambda<vector_ref>(ctx, result, "vector-ref", true);
  define_lambda<vector_set>(ctx, result, "vector-set!", true);

  define_lambda<ptr<pair>(context&, generic_ptr const&, generic_ptr const&)>(
    ctx, result, "cons", true,
    [] (context& ctx, generic_ptr const& car, generic_ptr const& cdr) {
      return make<pair>(ctx, car, cdr);
    }
  );
  define_lambda<generic_ptr(ptr<pair> const&)>(ctx, result, "car", true, car);
  define_lambda<generic_ptr(ptr<pair> const&)>(ctx, result, "cdr", true, cdr);
  define_lambda<generic_ptr(ptr<pair> const&)>(ctx, result, "cadr", true, cadr);
  define_lambda<generic_ptr(ptr<pair> const&)>(ctx, result, "caddr", true, caddr);
  define_lambda<generic_ptr(ptr<pair> const&)>(ctx, result, "cadddr", true, cadddr);
  define_lambda<generic_ptr(ptr<pair> const&)>(ctx, result, "cddr", true, cddr);
  define_lambda<generic_ptr(ptr<pair> const&)>(ctx, result, "cdddr", true, cdddr);

  define_raw_lambda(
    ctx, result, "make-string", true,
    [] (context& ctx, std::vector<generic_ptr> const& args) {
      if (args.size() < 1)
        throw error{"make-string: Expected at least 1 argument"};
      if (args.size() > 2)
        throw error{"make-string: Expected at most 2 arguments"};

      integer::value_type length = expect<integer>(args[0]).value();
      if (length < 0)
        throw error{"make-string: Length cannot be negative"};

      auto result = make<string>(ctx, length);

      if (args.size() == 2) {
        ptr<character> fill = expect<character>(args[1]);
        for (std::size_t i = 0; i < static_cast<std::size_t>(length); ++i)
          result->set(i, fill->value());
      }

      return result;
    }
  );

  define_lambda<integer(ptr<string> const&)>(
    ctx, result, "string-length", true,
    [] (ptr<string> const& s) {
      return integer{s->size()};
    }
  );

  define_top_level(ctx, result, "string-append",
                   make<native_procedure>(ctx,
                                          [] (context& ctx, std::vector<generic_ptr> const& args) {
                                            std::string result;
                                            for (generic_ptr const& s : args)
                                              result += expect<string>(s)->value();
                                            return make_string(ctx, result);
                                          },
                                          "string-append"),
                   true);

  define_lambda<ptr<string>(context&, generic_ptr const&)>(
    ctx, result, "number->string", true,
    [] (context& ctx, generic_ptr const& num) {
      if (!is_number(num))
        throw error{"Not a number: {}", datum_to_string(ctx, num)};
      return datum_to_string(ctx, num);
    }
  );

  define_lambda<ptr<string>(context&, generic_ptr const&)>(
    ctx, result, "datum->string", true,
    [] (context& ctx, generic_ptr const& datum) {
      return datum_to_string(ctx, datum);
    }
  );

  define_lambda<ptr<string>(context&, generic_ptr const&)>(
    ctx, result, "symbol->string", true,
    [] (context& ctx, generic_ptr const& datum) {
      return make_string(ctx, expect<symbol>(datum)->value());
    }
  );

  define_lambda<ptr<syntactic_closure>(context&, ptr<environment> const&,
                                       generic_ptr const&, generic_ptr const&)>(
    ctx, result, "make-syntactic-closure", true,
    [] (context& ctx, ptr<environment> const& env, generic_ptr const& free, generic_ptr const& form) {
      return make<syntactic_closure>(ctx, env, form, free);
    }
  );

  define_lambda<syntactic_closure_expression>(ctx, result, "syntactic-closure-expression", true);
  define_lambda<syntactic_closure_environment>(ctx, result, "syntactic-closure-environment", true);

  define_lambda<type>(ctx, result, "type", true);

  define_lambda<ptr<boolean>(context&, generic_ptr const&, generic_ptr const&)>(
    ctx, result, "eq?", true,
    [] (context& ctx, generic_ptr const& x, generic_ptr const& y) {
      return x == y ? ctx.constants->t : ctx.constants->f;
    }
  );

  define_lambda<eqv>(ctx, result, "eqv?", true);
  define_lambda<equal>(ctx, result, "equal?", true);

  define_lambda<ptr<vector>(context&, ptr<procedure> const&)>(
    ctx, result, "procedure-bytecode", true,
    [] (context& ctx, ptr<procedure> const& f) {
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

  define_lambda<generic_ptr(context&, ptr<procedure> const&)>(
    ctx, result, "procedure-name", true,
    [] (context& ctx, ptr<procedure> const& f) -> generic_ptr {
      if (f->name)
        return make_string(ctx, *f->name);
      else
        return ctx.constants->f;
    }
  );

  define_lambda<integer(ptr<opaque_value<instruction>> const&)>(
    ctx, result, "instruction-opcode", true,
    [] (ptr<opaque_value<instruction>> const& i) {
      return integer{static_cast<integer::storage_type>(i->value.opcode)};
    }
  );

  define_lambda<generic_ptr(context&, ptr<opaque_value<instruction>> const&)>(
    ctx, result, "instruction-operands", true,
    [] (context& ctx, ptr<opaque_value<instruction>> const& i) {
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

  define_lambda<ptr<string>(context&, operand)>(
    ctx, result, "top-level-name", true,
    [] (context& ctx, operand op) {
      return make_string(ctx, ctx.get_top_level_name(op));
    }
  );

  define_lambda<generic_ptr(context&, operand)>(
    ctx, result, "static-value", true,
    [] (context& ctx, operand op) {
      return ctx.get_static(op);
    }
  );

  define_lambda<void(context&, ptr<boolean> const&)>(
    ctx, result, "set-verbose-collection!", true,
    [] (context& ctx, ptr<boolean> const& value) {
      ctx.store.verbose_collection = value->value();
    }
  );

  return result;
}

} // namespace insider
