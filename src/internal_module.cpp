#include "internal_module.hpp"

#include "converters.hpp"
#include "io.hpp"

#ifdef INSIDER_VM_PROFILER
#include "vm.hpp"
#endif

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
  define_procedure(ctx, "vector->list", result, true, vector_to_list);
  define_raw_procedure(ctx, "vector-append", result, true, vector_append);
  define_procedure(
    ctx, "vector-length", result, true,
    [] (vector* v) {
      return integer{static_cast<integer::value_type>(v->size())};
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
  operand vector_ref_index = define_procedure(ctx, "vector-ref", result, true, &vector::ref);
  ctx.tag_top_level(vector_ref_index, special_top_level_tag::vector_ref);

  operand vector_set_index = define_procedure(
    ctx, "vector-set!", result, true,
    [] (context& ctx, vector* v, std::size_t i, object* o) {
      v->set(ctx.store, i, o);
    }
  );
  ctx.tag_top_level(vector_set_index, special_top_level_tag::vector_set);

  define_procedure(ctx, "cons", result, true, cons);
  define_procedure(ctx, "car", result, true, static_cast<object* (*)(pair*)>(car));
  define_procedure(ctx, "cdr", result, true, static_cast<object* (*)(pair*)>(cdr));
  define_procedure(ctx, "cadr", result, true, cadr);
  define_procedure(ctx, "caddr", result, true, caddr);
  define_procedure(ctx, "cadddr", result, true, cadddr);
  define_procedure(ctx, "cddr", result, true, cddr);
  define_procedure(ctx, "cdddr", result, true, cdddr);
  define_procedure(ctx, "set-car!", result, true,
                   [] (context& ctx, pair* p, object* new_car) {
                     p->set_car(ctx.store, new_car);
                   });
  define_procedure(ctx, "set-cdr!", result, true,
                   [] (context& ctx, pair* p, object* new_cdr) {
                     p->set_cdr(ctx.store, new_cdr);
                   });

  define_raw_procedure(
    ctx, "make-string", result, true,
    [] (context& ctx, object_span args) {
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
      return integer{static_cast<integer::value_type>(s->size())};
    }
  );

  define_raw_procedure(ctx, "string-append", result, true,
                       [] (context& ctx, object_span args) {
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
      return make<syntactic_closure>(ctx, env, datum_to_syntax(ctx, source_location::unknown, form), free);
    }
  );

  define_procedure(ctx, "syntactic-closure-expression", result, true, &syntactic_closure::expression);
  define_procedure(ctx, "syntactic-closure-environment", result, true, &syntactic_closure::environment);

  define_procedure(ctx, "type", result, true, type);
  define_raw_procedure(ctx, "error", result, true,
                       [] (context& ctx, object_span args) -> object* {
                         if (args.size() < 1)
                           throw error{"Expected at least 1 argument"};

                         std::string msg = expect<string>(args[0])->value();
                         for (std::size_t i = 1; i < args.size(); ++i)
                           msg += " " + datum_to_string(ctx, args[i]);

                         throw error{msg};
                       });

  define_procedure(
    ctx, "eq?", result, true,
    [] (context& ctx, object* x, object* y) {
      return x == y ? ctx.constants->t.get() : ctx.constants->f.get();
    }
  );

  define_procedure(ctx, "eqv?", result, true, eqv);
  define_procedure(ctx, "equal?", result, true, equal);

  define_procedure(ctx, "datum->syntax", result, true,
                   [] (context& ctx, syntax* s, object* datum) {
                     return datum_to_syntax(ctx, s->location(), datum);
                   });

  // define_procedure(ctx, "free-identifier=?", result, true,
  //                  [] (context& ctx, object* x, object* y) {
  //                    if (!is_identifier(x) || !is_identifier(y))
  //                      throw error{"Expected two identifiers"};

  //                    auto lookup_binding = [&] (object* id) -> std::optional<environment::value_type> {
  //                      tracked_ptr<environment> env = ctx.current_usage_environment;
  //                      if (auto sc = match<syntactic_closure>(id)) {
  //                        env = syntactic_closure_to_environment(ctx, sc, env);
  //                        id = sc->expression();
  //                      }

  //                      if (env)
  //                        return lookup(env, id);
  //                      else
  //                        return std::nullopt;
  //                    };

  //                    auto x_binding = lookup_binding(x);
  //                    auto y_binding = lookup_binding(y);

  //                    if (x_binding && y_binding)
  //                      return *x_binding == *y_binding;
  //                    else if (!x_binding && !y_binding)
  //                      return identifier_name(x) == identifier_name(y);
  //                    else
  //                      return false;
  //                  });

  define_procedure(
    ctx, "procedure-bytecode", result, true,
    [] (context& ctx, procedure* f) {
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
      return integer{static_cast<integer::value_type>(i->value.opcode)};
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

#ifdef INSIDER_VM_PROFILER
  define_procedure(
    ctx, "instruction-counts", result, true,
    [] (context& ctx) {
      return make_vector(ctx, ctx.instruction_counts,
                         [&] (std::size_t count) { return to_scheme(ctx, static_cast<integer::value_type>(count)); });
    }
  );

  define_procedure(
    ctx, "reset-instruction-counts!", result, true,
    [] (context& ctx) {
      ctx.instruction_counts = std::vector<std::size_t>(instructions.size());
    }
  );

  define_procedure(
    ctx, "instruction-times", result, true,
    [] (context& ctx) {
      return make_vector(ctx, ctx.instruction_times,
                         [&] (std::chrono::high_resolution_clock::duration time) {
                           return to_scheme(ctx,
                                            std::chrono::duration_cast<std::chrono::microseconds>(time).count() / 1.0e6);
                         });
    }
  );

  define_procedure(
    ctx, "reset-instruction-times!", result, true,
    [] (context& ctx) {
      ctx.instruction_times = std::vector<std::chrono::high_resolution_clock::duration>(instructions.size());
    }
  );
#endif

  return result;
}

} // namespace insider
