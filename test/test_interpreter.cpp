#include "scheme_fixture.hpp"

#include "compiler/variable.hpp"
#include "runtime/symbol.hpp"
#include "runtime/syntax.hpp"
#include "util/define_procedure.hpp"

using namespace insider;

template <typename T, typename... Args>
operand
make_static(context& ctx, Args&&... args) {
  if constexpr (std::is_same_v<T, integer>)
    return ctx.intern_static(integer_to_ptr(integer{args...}));
  else
    return ctx.intern_static(make<T>(ctx, std::forward<Args>(args)...));
}

static ptr<closure>
make_closure(context& ctx, bytecode const& bc, unsigned locals_size,
               unsigned min_args, bool has_rest = false) {
  return make_closure_from_bytecode(ctx, bc, locals_size, min_args, has_rest,
                                    "<test procedure>");
}

static operand
make_static_closure(context& ctx, bytecode const& bc,
                    unsigned locals_size, unsigned min_args) {
  return ctx.intern_static(make_closure(ctx, bc, locals_size, min_args));
}

static operand
make_static_procedure(context& ctx, bytecode const& bc,
                      unsigned locals_size, unsigned min_args) {
  return ctx.intern_static(
    make_procedure_from_bytecode(ctx, bc, locals_size, min_args, false,
                                 "<test procedure>")
  );
}

static bytecode
make_bytecode(std::vector<instruction> const& instr) {
  bytecode bc;
  for (instruction const& i : instr)
    encode_instruction(bc, i);

  return bc;
}

struct interpreter : scheme_fixture { };

TEST_F(interpreter, exec_arithmetic) {
  // 2 * (3 + 6). The input constants are stored in statics, result is stored in
  // local register 0.
  auto two = make_static<integer>(ctx, 2);
  auto three = make_static<integer>(ctx, 3);
  auto six = make_static<integer>(ctx, 6);

  auto proc = make_closure(
    ctx,
    make_bytecode({instruction{opcode::load_static, two,   operand{2}},
                   instruction{opcode::load_static, three, operand{3}},
                   instruction{opcode::load_static, six,   operand{4}},
                   instruction{opcode::add,         operand{3}, operand{4}, operand{1}},
                   instruction{opcode::multiply,    operand{2}, operand{1}, operand{0}},
                   instruction{opcode::ret,         operand{0}}}),
    5,
    0
  );
  auto result = call_with_continuation_barrier(ctx, proc, {});
  EXPECT_EQ(assume<integer>(result.get()).value(), 18);
}

TEST_F(interpreter, can_push_and_pop) {
  auto one = make_static<integer>(ctx, 1);
  auto two = make_static<integer>(ctx, 2);
  auto proc = make_closure(
    ctx,
    make_bytecode({
      instruction{opcode::load_static, one, operand{0}},
      instruction{opcode::load_static, two, operand{1}},
      instruction{opcode::push, operand{0}},
      instruction{opcode::push, operand{1}},
      instruction{opcode::pop, operand{0}},
      instruction{opcode::ret, operand{0}}
    }),
    2, 0
  );
  auto result = call_with_continuation_barrier(ctx, proc, {});
  EXPECT_EQ(expect<integer>(result.get()).value(), 2);
}

static operand
nth_argument(std::size_t n, std::size_t num_args) {
  return -static_cast<operand>(call_stack::stack_frame_header_size)
         - static_cast<operand>(num_args)
         + static_cast<operand>(n);
}

TEST_F(interpreter, can_access_pushed_arguments_from_callee) {
  auto one = make_static<integer>(ctx, 1);
  auto two = make_static<integer>(ctx, 2);
  operand add = make_static_closure(
    ctx,
    make_bytecode({
      instruction{opcode::add, nth_argument(0, 2), nth_argument(1, 2),
                  operand{0}},
      instruction{opcode::ret, operand{0}}
    }),
    1, 2
  );
  auto f = make_closure(
    ctx,
    make_bytecode({
      instruction{opcode::load_static, one, operand{0}},
      instruction{opcode::load_static, two, operand{1}},
      instruction{opcode::push, operand{0}},
      instruction{opcode::push, operand{1}},
      instruction{opcode::call_static, add, operand{2}},
      instruction{opcode::pop, operand{0}},
      instruction{opcode::ret, operand{0}}
    }),
    2, 0
  );
  auto result = call_with_continuation_barrier(ctx, f, {});
  EXPECT_EQ(expect<integer>(result.get()).value(), 3);
}

TEST_F(interpreter, exec_calls) {
  // f(x, y) = 2 * x + y
  // Evaluate: 3 * f(5, 7) + f(2, f(3, 4))

  auto two = make_static<integer>(ctx, 2);

  auto f = make_static_closure(
    ctx,
    make_bytecode({instruction{opcode::load_static, two, operand{3}},
                   instruction{opcode::multiply, operand{3}, nth_argument(0, 2),
                               operand{2}},
                   instruction{opcode::add, operand{2}, nth_argument(1, 2),
                               operand{2}},
                   instruction{opcode::ret, operand{2}}}),
    4, 2
  );

  auto three = make_static<integer>(ctx, 3);
  auto five = make_static<integer>(ctx, 5);
  auto seven = make_static<integer>(ctx, 7);
  auto four = make_static<integer>(ctx, 4);

  auto global = make_closure(
    ctx,
    make_bytecode({instruction{opcode::load_static, five,  operand{3}},
                   instruction{opcode::load_static, seven, operand{4}},
                   instruction{opcode::load_static, f,     operand{5}},
                   instruction{opcode::push, operand{3}},
                   instruction{opcode::push, operand{4}},
                   instruction{opcode::call, operand{5}, operand{2}},
                   instruction{opcode::pop, operand{0}},
                   instruction{opcode::load_static, three, operand{6}},
                   instruction{opcode::multiply, operand{6}, operand{0}, operand{0}},
                   instruction{opcode::load_static, four,  operand{7}},
                   instruction{opcode::load_static, two,   operand{8}},
                   instruction{opcode::push, operand{8}},
                   instruction{opcode::push, operand{6}},
                   instruction{opcode::push, operand{7}},
                   instruction{opcode::call, operand{5}, operand{2}},
                   instruction{opcode::call, operand{5}, operand{2}},
                   instruction{opcode::pop, operand{1}},
                   instruction{opcode::add, operand{0}, operand{1}, operand{0}},
                   instruction{opcode::ret, operand{0}}}),
    9,
    0
  );
  auto result = call_with_continuation_barrier(ctx, global, {});

  auto native_f = [] (int x, int y) { return 2 * x + y; };
  EXPECT_EQ(assume<integer>(result.get()).value(),
            3 * native_f(5, 7) + native_f(2, native_f(3, 4)));
}

TEST_F(interpreter, exec_tail_calls) {
  // f(x) = g(x)
  // g(x) = 2 * x
  auto two = make_static<integer>(ctx, 2);
  auto g = make_static_closure(
    ctx,
    make_bytecode({instruction{opcode::load_static, two, operand{2}},
                   instruction{opcode::multiply, operand{2}, nth_argument(0, 1),
                               operand{1}},
                   instruction{opcode::ret, operand{1}}}),
    3, 1
  );
  auto f = make_static_closure(
    ctx,
    make_bytecode({instruction{opcode::load_static, g, operand{1}},
                   instruction{opcode::push, nth_argument(0, 1)},
                   instruction{opcode::tail_call, operand{1}, operand{1}}}),
    2, 1
  );
  auto six = make_static<integer>(ctx, 6);
  auto global = make_closure(
    ctx,
    make_bytecode({instruction{opcode::load_static, f, operand{1}},
                   instruction{opcode::load_static, six, operand{2}},
                   instruction{opcode::push, operand{2}},
                   instruction{opcode::call, operand{1}, operand{1}},
                   instruction{opcode::pop, operand{0}},
                   instruction{opcode::ret, operand{0}}}),
    3, 0
  );
  auto result = call_with_continuation_barrier(ctx, global, {}).get();
  EXPECT_EQ(assume<integer>(result).value(), 12);
}

TEST_F(interpreter, exec_loop) {
  // sum = 0
  // i = 0
  // while i < 10
  //   sum += i
  //   i += 1

  auto zero = make_static<integer>(ctx, 0);
  auto ten = make_static<integer>(ctx, 10);
  auto one = make_static<integer>(ctx, 1);
  auto global = make_closure(
    ctx,
    make_bytecode({instruction{opcode::load_static, zero,       operand{3}},
                   instruction{opcode::load_static, ten,        operand{4}},
                   instruction{opcode::load_static, one,        operand{5}},
                   instruction{opcode::set,         operand{3}, operand{0}},
                   instruction{opcode::set,         operand{3}, operand{1}},
                   instruction{opcode::less,        operand{1}, operand{4}, operand{2}},
                   instruction{opcode::jump_unless, operand{2}, operand{10}},
                   instruction{opcode::add,         operand{0}, operand{1}, operand{0}},
                   instruction{opcode::add,         operand{1}, operand{5}, operand{1}},
                   instruction{opcode::jump_back,   operand{17}},
                   instruction{opcode::ret,         operand{0}}}),
    6,
    0
  );
  auto result = call_with_continuation_barrier(ctx, global, {}).get();
  EXPECT_EQ(assume<integer>(result).value(), 45);
}

TEST_F(interpreter, exec_native_call) {
  auto native = [] (context&, ptr<native_procedure>, object_span args) {
    return integer_to_ptr(integer{2 * expect<integer>(args[0]).value()
                                  + 3 * expect<integer>(args[1]).value()
                                  + 5 * expect<integer>(args[2]).value()});
  };
  auto native_static = make_static<native_procedure>(ctx, native);
  auto ten = make_static<integer>(ctx, 10);
  auto twenty = make_static<integer>(ctx, 20);
  auto thirty = make_static<integer>(ctx, 30);
  auto global = make_closure(
    ctx,
    make_bytecode({instruction{opcode::load_static, ten,           operand{1}},
                   instruction{opcode::load_static, twenty,        operand{2}},
                   instruction{opcode::load_static, thirty,        operand{3}},
                   instruction{opcode::load_static, native_static, operand{4}},
                   instruction{opcode::push, operand{1}},
                   instruction{opcode::push, operand{2}},
                   instruction{opcode::push, operand{3}},
                   instruction{opcode::call, operand{4}, operand{3}},
                   instruction{opcode::pop, operand{0}},
                   instruction{opcode::ret, operand{0}}}),
    5,
    0
  );
  auto result = call_with_continuation_barrier(ctx, global, {}).get();
  EXPECT_EQ(assume<integer>(result).value(),
            2 * 10 + 3 * 20 + 5 * 30);
}

TEST_F(interpreter, exec_closure_ref) {
  auto add = make_static_procedure(
    ctx,
    make_bytecode({instruction{opcode::add, nth_argument(0, 1), operand{0},
                               operand{0}},
                   instruction{opcode::ret, operand{0}}}),
    2, 1
  );
  auto three = make_static<integer>(ctx, 3);
  auto five = make_static<integer>(ctx, 5);
  auto global = make_closure(
    ctx,
    make_bytecode({instruction{opcode::load_static, add,   operand{2}},
                   instruction{opcode::load_static, three, operand{3}},
                   instruction{opcode::load_static, five,  operand{4}},
                   instruction{opcode::push, operand{3}},
                   instruction{opcode::make_closure, operand{2}, operand{1},
                               operand{1}},
                   instruction{opcode::push, operand{4}},
                   instruction{opcode::call, operand{1}, operand{1}},
                   instruction{opcode::pop, operand{0}},
                   instruction{opcode::ret, operand{0}}}),
    5, 0
  );
  auto result = call_with_continuation_barrier(ctx, global, {}).get();
  EXPECT_EQ(assume<integer>(result).value(), 5 + 3);
}

TEST_F(interpreter, exec_cons) {
  auto one = make_static<integer>(ctx, 1);
  auto two = make_static<integer>(ctx, 2);
  auto three = make_static<integer>(ctx, 3);
  auto global = make_closure(
    ctx,
    make_bytecode({instruction{opcode::load_static, ctx.statics.null, operand{1}},
                   instruction{opcode::load_static, one,              operand{2}},
                   instruction{opcode::load_static, two,              operand{3}},
                   instruction{opcode::load_static, three,            operand{4}},
                   instruction{opcode::cons,        operand{4},       operand{1}, operand{0}},
                   instruction{opcode::cons,        operand{3},       operand{0}, operand{0}},
                   instruction{opcode::cons,        operand{2},       operand{0}, operand{0}},
                   instruction{opcode::ret,         operand{0}}}),
    5, 0
  );
  auto result = call_with_continuation_barrier(ctx, global, {});
  EXPECT_TRUE(equal(result.get(), read("(1 2 3)")));
}

TEST_F(interpreter, exec_car_cdr) {
  auto p = track(ctx, cons(ctx, integer_to_ptr(1), integer_to_ptr(2)));

  auto first = make_closure(
    ctx,
    make_bytecode({instruction{opcode::car, nth_argument(0, 1), operand{0}},
                   instruction{opcode::ret, operand{0}}}),
    1, 1
  );
  auto result1 = call_with_continuation_barrier(ctx, first,
                                                {p.get()});
  EXPECT_EQ(expect<integer>(result1).value(), 1);

  auto second = make_closure(
    ctx,
    make_bytecode({instruction{opcode::cdr, nth_argument(0, 1), operand{0}},
                   instruction{opcode::ret, operand{0}}}),
    1, 1
  );
  auto result2 = call_with_continuation_barrier(ctx, second,
                                                {p.get()});
  EXPECT_EQ(expect<integer>(result2).value(), 2);
}

TEST_F(interpreter, test_eq) {
  auto are_eq = track(
    ctx,
    make_closure(
      ctx,
      make_bytecode({instruction{opcode::eq,
                                 nth_argument(0, 2), nth_argument(1, 2),
                                 operand{0}},
                     instruction{opcode::ret, operand{0}}}),
      1, 2
    )
  );

  auto result1 = call_with_continuation_barrier(
    ctx, are_eq.get(), {ctx.intern("foo"), ctx.intern("bar")}
  );
  EXPECT_EQ(result1.get(), ctx.constants->f);

  auto result2 = call_with_continuation_barrier(
    ctx, are_eq.get(), {ctx.intern("foo"), ctx.intern("foo")}
  );
  EXPECT_EQ(result2.get(), ctx.constants->t);
}

TEST_F(interpreter, exec_make_vector) {
  auto one = make_static<integer>(ctx, 1);
  auto two = make_static<integer>(ctx, 2);
  auto three = make_static<integer>(ctx, 3);
  auto global = make_closure(
    ctx,
    make_bytecode({instruction{opcode::load_static, one,   operand{1}},
                   instruction{opcode::load_static, two,   operand{2}},
                   instruction{opcode::load_static, three, operand{3}},
                   instruction{opcode::push, operand{1}},
                   instruction{opcode::push, operand{2}},
                   instruction{opcode::push, operand{3}},
                   instruction{opcode::make_vector, operand{3}, operand{0}},
                   instruction{opcode::ret,         operand{0}}}),
    4, 0
  );

  auto result = call_with_continuation_barrier(ctx, global, {});
  EXPECT_TRUE(equal(result.get(), read("#(1 2 3)")));
}

TEST_F(interpreter, exec_load_dynamic_top_level) {
  operand index = ctx.add_top_level(ctx.intern("foo"), "top-level");
  auto var = make<top_level_variable>(ctx, "top-level", index);
  auto top_level_scope = make<scope>(ctx, ctx, "top-level-scope");
  auto id = make<syntax>(ctx, ctx.intern("top-level"),
                         scope_set{top_level_scope});
  define(ctx.store, id, var);
  operand id_index = ctx.intern_static(id);

  auto f = make_closure(
    ctx,
    make_bytecode({
      instruction{opcode::load_dynamic_top_level, id_index, operand{0}},
      instruction{opcode::ret, operand{0}}
    }),
    1, 0
  );
  auto result = call_with_continuation_barrier(ctx, f, {});
  EXPECT_EQ(expect<symbol>(result)->value(), "foo");
}

static integer
apply_and_double(context& ctx, ptr<closure> f, ptr<> arg) {
  return 2 * expect<integer>(call_with_continuation_barrier(ctx, f,
                                                            {arg})).value();
}

TEST_F(interpreter, scheme_to_native_to_scheme) {
  define_procedure<apply_and_double>(ctx, "apply-and-double",
                                     ctx.internal_module());
  ptr<> result1 = eval_module(
    R"(
      (import (insider internal))

      (define add-3
        (lambda (x)
          (+ x 3)))

      (apply-and-double add-3 5)
   )"
  );
  EXPECT_EQ(expect<integer>(result1).value(), 2 * (5 + 3));

  ptr<> result2 = eval_module(
    R"(
      (import (insider internal))

      (define add-3
        (lambda (x)
          (+ x 3)))

      (let ((result (apply-and-double add-3 5)))
        result)
   )"
  );
  EXPECT_EQ(expect<integer>(result2).value(), 2 * (5 + 3));
}

TEST_F(interpreter, call_variadic_scheme_procedure_from_native) {
  ptr<> f = eval("(lambda args args)");
  ptr<> result = call_with_continuation_barrier(
    ctx, f, {to_scheme(ctx, 0), to_scheme(ctx, 1), to_scheme(ctx, 2)}
  ).get();
  EXPECT_TRUE(equal(result, read("(0 1 2)")));
}

static ptr<>
do_recursion(context& ctx, int i, int accum, ptr<> recurse, ptr<> base) {
  if (i == 0)
    return tail_call(ctx, base, {to_scheme(ctx, accum)});
  else
    return tail_call(ctx, recurse, {to_scheme(ctx, i - 1), to_scheme(ctx, accum + i), recurse, base});
}

TEST_F(interpreter, native_tail_calls) {
  define_procedure<do_recursion>(ctx, "f", ctx.internal_module());

  ptr<> result1 = eval_module(
    R"(
      (import (insider internal))

      (f 10 0 f (lambda (x) (* 2 x)))
   )"
  );
  EXPECT_EQ(expect<integer>(result1).value(), 2 * 55);

  ptr<> result2 = eval_module(
    R"(
      (import (insider internal))

      (let ((result (f 10 0 f (lambda (x) (* 2 x)))))
        result)
   )"
  );
  EXPECT_EQ(expect<integer>(result2).value(), 2 * 55);
}

static bool
test_args(int i, int j, int k) {
  return i < j && j < k;
}

TEST_F(interpreter, native_argument_order) {
  define_procedure<test_args>(ctx, "test-args", ctx.internal_module());
  ptr<> result = eval("(test-args 1 2 3)");
  EXPECT_EQ(result, ctx.constants->t);
}

static int
one(int) { return 1; }

static int
two(int) { return 2; }

static int
three(int) { return 3; }

TEST_F(interpreter, nested_native_calls) {
  define_procedure<one>(ctx, "one", ctx.internal_module());
  define_procedure<two>(ctx, "two", ctx.internal_module());
  define_procedure<three>(ctx, "three", ctx.internal_module());
  define_procedure<test_args>(ctx, "test-args", ctx.internal_module());
  ptr<> result = eval("(test-args (one 5) (two 2) (three 1))");
  EXPECT_EQ(result, ctx.constants->t);
}

TEST_F(interpreter, eval_simple_expression) {
  ptr<> result = eval("(eval '(* 7 3) (environment '(insider internal)))");
  EXPECT_EQ(expect<integer>(result).value(), 21);
}

TEST_F(interpreter, cant_define_in_immutable_environment) {
  EXPECT_THROW(eval("(eval '(define foo 1) (environment '(insider internal)))"),
               std::runtime_error);
}

static ptr<>
return_true(context& ctx) {
  return ctx.constants->t;
}

TEST_F(interpreter, eval_tail_call_to_native) {
  define_procedure<return_true>(ctx, "return-t", ctx.internal_module());
  ptr<> result = eval("(eval '(return-t) (environment '(insider internal)))");
  EXPECT_EQ(result, ctx.constants->t);
}

struct repl_fixture : interpreter {
  tracked_ptr<module_> m
    = make_interactive_module(
        ctx,
        import_modules(module_name{"insider", "internal"})
      );
};

TEST_F(repl_fixture, eval_simple_expression_in_interactive_module) {
  ptr<> result = insider::eval(ctx, m, "(* 7 3)").get();
  EXPECT_EQ(expect<integer>(result).value(), 21);
}

TEST_F(repl_fixture, define_top_level_in_interactive_module) {
  insider::eval(ctx, m, "(define x 7)");
  insider::eval(ctx, m, "(define y 3)");
  ptr<> result = insider::eval(ctx, m, "(* x y)").get();
  EXPECT_EQ(expect<integer>(result).value(), 21);
}

TEST_F(repl_fixture, reference_variable_before_definition) {
  insider::eval(ctx, m, "(define f (lambda () (* a 2)))");
  EXPECT_THROW(insider::eval(ctx, m, "(f)"), unbound_variable_error);
  insider::eval(ctx, m, "(define a 4)");
  ptr<> result = insider::eval(ctx, m, "(f)").get();
  EXPECT_EQ(expect<integer>(result).value(), 8);
}

TEST_F(repl_fixture, reference_to_top_level_variable_doesnt_bind_to_local) {
  insider::eval(ctx, m, "(define f (lambda () (* a 2)))");
  EXPECT_THROW(insider::eval(ctx, m, "(let ((a 4)) (f))"),
               unbound_variable_error);
}

TEST_F(repl_fixture, reference_procedure_before_definition) {
  insider::eval(ctx, m, "(define f (lambda () (a 2)))");
  insider::eval(ctx, m, "(define a (lambda (x) (* 2 x)))");
  ptr<> result = insider::eval(ctx, m, "(f)").get();
  EXPECT_EQ(expect<integer>(result).value(), 4);
}

TEST_F(repl_fixture, define_syntax_in_repl) {
  insider::eval(ctx, m, "(define-syntax a (lambda (stx) #'4))");
  ptr<> result = insider::eval(ctx, m, "(a)").get();
  EXPECT_EQ(expect<integer>(result).value(), 4);
}

TEST_F(repl_fixture, reference_to_unknown_variable_doesnt_bind_to_syntax) {
  insider::eval(ctx, m, "(define f (lambda () (a 2)))");
  insider::eval(
    ctx, m,
    R"(
      (define-syntax a
        (lambda (stx)
          (let ((name (cadr (syntax->list stx))))
            #`(* 2 #,name))))
    )"
  );
  EXPECT_THROW(insider::eval(ctx, m, "(f)"), unbound_variable_error);
}

TEST_F(repl_fixture, redefine_variable_in_repl) {
  insider::eval(ctx, m, "(define x 1)");
  insider::eval(ctx, m, "(define x 2)");
  ptr<> result = insider::eval(ctx, m, "x").get();
  EXPECT_EQ(expect<integer>(result).value(), 2);
}

TEST_F(repl_fixture, redefine_syntax_in_repl) {
  insider::eval(ctx, m, "(define-syntax a (lambda (stx) #'4))");
  insider::eval(ctx, m, "(define f (lambda () (a)))");
  ptr<> result1 = insider::eval(ctx, m, "(f)").get();
  EXPECT_EQ(expect<integer>(result1).value(), 4);

  // f still uses the definition that was visible when it was defined.
  insider::eval(ctx, m, "(define-syntax a (lambda (stx) #'8))");
  ptr<> result2 = insider::eval(ctx, m, "(f)").get();
  EXPECT_EQ(expect<integer>(result2).value(), 4);

  // The new definition of f will use the new definition of a.
  insider::eval(ctx, m, "(define f (lambda () (a)))");
  ptr<> result3 = insider::eval(ctx, m, "(f)").get();
  EXPECT_EQ(expect<integer>(result3).value(), 8);
}

TEST_F(repl_fixture, eval_sets_current_module_parameter) {
  ptr<> result
    = insider::eval(
      ctx, m,
      R"(
        (let-syntax ((s (lambda (stx)
                          (datum->syntax
                           #f
                           (find-parameter-value
                            current-expand-module-tag)))))
          (s))
      )").get();
  EXPECT_EQ(result, m.get());
}

TEST_F(interpreter, scheme_eval_sets_current_module_parameter) {
  ptr<> result = eval(
    R"(
      (let-syntax ((s (lambda (stx)
                        (datum->syntax
                         #f
                         (find-parameter-value
                          current-expand-module-tag)))))
        (s))
    )"
  );
  EXPECT_TRUE(is<module_>(result));
}

TEST_F(repl_fixture, dynamic_import_performs_imports) {
  define_top_level(ctx, "m", m.get(), true, m.get());
  // Not yet imported
  EXPECT_THROW(insider::eval(ctx, m, "var"), unbound_variable_error);

  add_source_file(
    "foo.scm",
    R"(
      (library (foo))
      (import (insider internal))
      (export var)
      (define var 13)
    )"
  );

  insider::eval(ctx, m, "(dynamic-import m '(foo))");
  ptr<> result = insider::eval(ctx, m, "var").get();
  EXPECT_EQ(expect<integer>(result).value(), 13);
}

TEST_F(interpreter, repl_define_using_macro) {
  add_source_file(
    "foo.scm",
    R"(
      (library (foo))
      (import (insider internal))
      (export def)
      (define-syntax def
        (lambda (stx)
          (let ((name (cadr (syntax->list stx))))
            #`(define #,name 0))))
    )"
  );

  tracked_ptr<module_> m
    = make_interactive_module(
      ctx,
      import_modules(module_name{"foo"})
    );
  insider::eval(ctx, m, "(def x)");
  ptr<> result = insider::eval(ctx, m, "x").get();
  EXPECT_EQ(expect<integer>(result).value(), 0);
}

TEST_F(interpreter, meta_eval_simple_expression) {
  ptr<> result = eval("(meta (+ 2 3))");
  EXPECT_EQ(expect<integer>(result).value(), 5);
}

TEST_F(interpreter, meta_eval_definition) {
  ptr<> result = eval_module(R"(
    (import (insider internal))
    (meta (define foo 12))
    (* foo 2)
  )");
  EXPECT_EQ(expect<integer>(result).value(), 24);
}

TEST_F(interpreter, meta_definitions_are_visible_in_transformers) {
  ptr<> result = eval_module(R"(
    (import (insider internal))

    (meta
      (define second
        (lambda (stx)
          (cadr (syntax->list stx)))))

    (meta
      (define third
        (lambda (stx)
          (caddr (syntax->list stx)))))

    (define-syntax backward
      (lambda (stx)
        #`(#,(third stx) #,(second stx))))

    (backward 2 -)
  )");
  EXPECT_EQ(expect<integer>(result).value(), -2);
}

TEST_F(interpreter, meta_syntax_definitions_are_visible_in_transformers) {
  ptr<> result = eval_module(R"(
    (import (insider internal))

    (meta
      (define-syntax if*
        (lambda (stx)
          (let ((exprs (syntax->list stx)))
            #`(if . #,(cdr exprs))))))

    (define-syntax introduce
      (lambda (stx)
        (let ((exprs (syntax->list stx)))
          (let ((name (cadr exprs))
                (value (if* (eq? (cddr exprs) '())
                            #'0
                            (caddr exprs))))
            #`(define #,name #,value)))))

    (introduce foo)
    (introduce bar 2)
    (+ foo bar)
  )");
  EXPECT_EQ(expect<integer>(result).value(), 2);
}

TEST_F(interpreter, right_hand_side_of_meta_is_macro_expanded) {
  ptr<> result = eval_module(R"(
    (import (insider internal))

    (meta
      (define-syntax make-def
        (lambda (stx)
          (let ((exprs (syntax->list stx)))
            (let ((name (cadr exprs))
                  (value (caddr exprs)))
              #`(define #,name #,value))))))

    (meta (make-def foo 2))
    foo
  )");
  EXPECT_EQ(expect<integer>(result).value(), 2);
}

TEST_F(interpreter, internal_meta_definition_is_visible_in_transformer) {
  ptr<> result = eval(R"(
    (let ()
      (meta (define x 2))
      (define-syntax foo
        (lambda (stx)
          (datum->syntax stx x)))
      (foo))
  )");
  EXPECT_EQ(expect<integer>(result).value(), 2);
}

TEST_F(interpreter, can_get_expand_time_parameter_through_meta) {
  ptr<> result_without_meta
    = eval("(find-parameter-value current-expand-module-tag)");
  EXPECT_EQ(result_without_meta, ctx.constants->f);

  ptr<> result_from_meta
    = eval("(meta (find-parameter-value current-expand-module-tag))");
  EXPECT_TRUE(is<module_>(result_from_meta));
}

static unsigned meta_eval_count = 0;

static void
increase_meta_eval_count() {
  ++meta_eval_count;
}

TEST_F(interpreter, top_level_meta_is_evaluated_only_once) {
  define_procedure<increase_meta_eval_count>(ctx, "increase-count!",
                                             ctx.internal_module());
  eval_module("(import (insider internal)) (meta (increase-count!))");
  EXPECT_EQ(meta_eval_count, 1u);
}

static char const* introduce_macro_def = R"(
  (define-syntax introduce
    (lambda (stx)
      (let ((exprs (syntax->list stx)))
        (let ((name (cadr exprs))
              (body (cddr exprs)))
          #`(let ((#,name 0)) #,@body)))))
)";

TEST_F(repl_fixture, reference_and_set_variable_introduced_by_macro) {
  insider::eval(ctx, m, introduce_macro_def);
  ptr<> result = insider::eval(ctx, m, "(introduce x (set! x 1) x)").get();
  EXPECT_EQ(expect<integer>(result).value(), 1);
}

TEST_F(repl_fixture, set_variable_introduced_by_macro_in_meta) {
  insider::eval(ctx, m, introduce_macro_def);
  ptr<> result
    = insider::eval(ctx, m, "(meta (introduce x (set! x 1) x))").get();
  EXPECT_EQ(expect<integer>(result).value(), 1);
}

TEST_F(interpreter, meta_in_transformer_output_is_evaluted_in_subexpression) {
  ptr<> result = eval_module(R"(
    (import (insider internal))

    (define-syntax foo
      (lambda (stx)
        #'(meta (find-parameter-value current-expand-module-tag))))

    (define identity
      (lambda (x)
        x))

    (identity (foo))
  )");
  EXPECT_TRUE(is<module_>(result));
}

TEST_F(interpreter, tail_call_from_variadic_procedure) {
  auto result = eval_module(R"(
    (import (insider internal))

    (define g
      (lambda (x y)
        y))

    (define f
      (lambda (x . y)
        (g 0 x)))

    (f 1)
  )");
  EXPECT_EQ(expect<integer>(result).value(), 1);
}

TEST_F(interpreter, uncaught_exception_generates_backtrace) {
  EXPECT_TRUE(ctx.error_backtrace.empty());

  try {
    eval_module(R"(
      (import (insider internal))

      (define foo
        (lambda ()
          (raise #f)
          #void))

      (foo)
    )");
  } catch (...) {
    EXPECT_FALSE(ctx.error_backtrace.empty());
    return;
  }

  FAIL();
}

static void
check_stacktrace(context& ctx) {
  auto trace = stacktrace(ctx);
  ASSERT_EQ(trace.size(), 4);

  EXPECT_EQ(trace[0].name, "check-stacktrace!");
  EXPECT_EQ(trace[0].kind, stacktrace_record::kind::native);

  EXPECT_EQ(trace[1].name, "one");
  EXPECT_EQ(trace[1].kind, stacktrace_record::kind::scheme);

  EXPECT_EQ(trace[2].name, "two");
  EXPECT_EQ(trace[2].kind, stacktrace_record::kind::scheme);

  EXPECT_EQ(trace[3].name, "three");
  EXPECT_EQ(trace[3].kind, stacktrace_record::kind::scheme);
}

TEST_F(interpreter, stack_trace_includes_all_procedures) {
  define_procedure<check_stacktrace>(ctx, "check-stacktrace!",
                                     ctx.internal_module());

  eval_module(
    R"(
      (import (insider internal))

      (define one (lambda () (check-stacktrace!) #void))
      (define two (lambda () (one) #void))
      (define three (lambda () (two) #void))
      (three)
    )",
    no_optimisations
  );
}
