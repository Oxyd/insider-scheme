#include "scheme_fixture.hpp"

#include "compiler/variable.hpp"
#include "runtime/symbol.hpp"
#include "runtime/syntax.hpp"
#include "util/define_procedure.hpp"
#include "vm/stacktrace.hpp"
#include "vm/vm.hpp"
#include <gtest/gtest.h>
#include <stdexcept>

using namespace insider;

static ptr<procedure>
make_procedure(context& ctx, mutable_bytecode const& bc,
               std::vector<ptr<>> constants,
               unsigned locals_size,
               unsigned leading_args, bool has_rest = false) {
  return make_procedure(
    ctx, bc,
    procedure_prototype::meta{
      .locals_size = locals_size,
      .num_required_args = leading_args,
      .num_leading_args = leading_args,
      .has_rest = has_rest,
      .parameter_names = std::make_shared<ptr<keyword>[]>(leading_args),
      .name = std::make_shared<std::string>("<test procedure>"),
      .debug_info = std::make_shared<debug_info_map>()
    },
    std::move(constants)
  );
}

static mutable_bytecode
make_bytecode(std::vector<instruction> const& instr) {
  mutable_bytecode bc;
  for (instruction const& i : instr)
    encode_instruction(bc, i);

  return bc;
}

struct interpreter : scheme_fixture { };

TEST_F(interpreter, exec_arithmetic) {
  // 2 * (3 + 6). The input constants are stored in the constants vector, result
  // is stored in local register 0.

  auto proc = make_procedure(
    ctx,
    make_bytecode({{opcode::load_constant, operand{0}, operand{2}},
                   {opcode::load_constant, operand{1}, operand{3}},
                   {opcode::load_constant, operand{2}, operand{4}},
                   {opcode::add,         operand{3}, operand{4}, operand{1}},
                   {opcode::multiply,    operand{2}, operand{1}, operand{1}},
                   {opcode::ret,         operand{1}}}),
    {
      integer_to_ptr(2),
      integer_to_ptr(3),
      integer_to_ptr(6)
    },
    5,
    0
  );
  auto result = call_root(ctx, proc, {});
  EXPECT_EQ(assume<integer>(result).value(), 18);
}

TEST_F(interpreter, can_access_arguments_from_callee) {
  ptr<procedure> add = make_procedure(
    ctx,
    make_bytecode({
      {opcode::add, operand{1}, operand{2}, operand{1}},
      {opcode::ret, operand{1}}
    }),
    {},
    3, 2
  );
  auto f = make_procedure(
    ctx,
    make_bytecode({
      {opcode::load_constant, operand{2}, operand{1}},
      {opcode::load_constant, operand{0}, operand{2}},
      {opcode::load_constant, operand{1}, operand{3}},
      {opcode::call, operand{1}, operand{2}, operand{1}},
      {opcode::ret, operand{1}}
    }),
    {
      integer_to_ptr(1),
      integer_to_ptr(2),
      add
    },
    4, 0
  );
  auto result = call_root(ctx, f, {});
  EXPECT_EQ(expect<integer>(result).value(), 3);
}

TEST_F(interpreter, exec_calls) {
  // f(x, y) = 2 * x + y
  // Evaluate: 3 * f(5, 7) + f(2, f(3, 4))

  ptr<procedure> f = make_procedure(
    ctx,
    make_bytecode({{opcode::load_constant, operand{0}, operand{4}},
                   {opcode::multiply, operand{4}, operand{1}, operand{3}},
                   {opcode::add, operand{3}, operand{2}, operand{3}},
                   {opcode::ret, operand{3}}}),
    {
      integer_to_ptr(2)
    },
    5, 2
  );

  auto global = make_procedure(
    ctx,
    make_bytecode({{opcode::load_constant, operand{5}, operand{1}},
                   {opcode::load_constant, operand{2}, operand{2}},
                   {opcode::load_constant, operand{3}, operand{3}},
                   {opcode::call, operand{1}, operand{2}, operand{2}},
                   {opcode::load_constant, operand{1}, operand{3}},
                   {opcode::multiply, operand{3}, operand{2}, operand{2}},
                   {opcode::load_constant, operand{4}, operand{5}},
                   {opcode::set, operand{3}, operand{4}},
                   {opcode::set, operand{1}, operand{3}},
                   {opcode::call, operand{3}, operand{2}, operand{5}},
                   {opcode::load_constant, operand{0}, operand{4}},
                   {opcode::call, operand{3}, operand{2}, operand{1}},
                   {opcode::add, operand{1}, operand{2}, operand{1}},
                   {opcode::ret, operand{1}}}),
    {
      integer_to_ptr(2),  // 0
      integer_to_ptr(3),  // 1
      integer_to_ptr(5),  // 2
      integer_to_ptr(7),  // 3
      integer_to_ptr(4),  // 4
      f                   // 5
    },
    6,
    0
  );
  auto result = call_root(ctx, global, {});

  auto native_f = [] (int x, int y) { return 2 * x + y; };
  EXPECT_EQ(assume<integer>(result).value(),
            3 * native_f(5, 7) + native_f(2, native_f(3, 4)));
}

TEST_F(interpreter, exec_tail_calls) {
  // f(x) = g(x)
  // g(x) = 2 * x
  ptr<procedure> g = make_procedure(
    ctx,
    make_bytecode({{opcode::load_constant, operand{0}, operand{2}},
                   {opcode::multiply, operand{2}, operand{1}, operand{1}},
                   {opcode::ret, operand{1}}}),
    {
      integer_to_ptr(2)
    },
    3, 1
  );
  ptr<procedure> f = make_procedure(
    ctx,
    make_bytecode({{opcode::load_constant, operand{0}, operand{2}},
                   {opcode::set, operand{1}, operand{3}},
                   {opcode::tail_call, operand{2}, operand{1}}}),
    {
      g
    },
    4, 1
  );
  auto global = make_procedure(
    ctx,
    make_bytecode({{opcode::load_constant, operand{0}, operand{1}},
                   {opcode::load_constant, operand{1}, operand{2}},
                   {opcode::call, operand{1}, operand{1}, operand{1}},
                   {opcode::ret, operand{1}}}),
    {
      f,
      integer_to_ptr(6)
    },
    3, 0
  );
  auto result = call_root(ctx, global, {});
  EXPECT_EQ(assume<integer>(result).value(), 12);
}

TEST_F(interpreter, exec_loop) {
  // sum = 0
  // i = 0
  // while i < 10
  //   sum += i
  //   i += 1

  auto global = make_procedure(
    ctx,
    make_bytecode(
      {
        {opcode::load_constant, operand{0}, operand{4}},             // 0
        {opcode::load_constant, operand{1}, operand{5}},             // 3
        {opcode::load_constant, operand{2}, operand{6}},             // 6
        {opcode::set, operand{4}, operand{1}},                       // 9
        {opcode::set, operand{4}, operand{2}},                       // 12
        {opcode::less, operand{2}, operand{5}, operand{3}},          // 15
        {opcode::jump_unless, operand{3}, immediate_to_operand(10)}, // 19
        {opcode::add, operand{1}, operand{2}, operand{1}},           // 22
        {opcode::add, operand{2}, operand{6}, operand{2}},           // 26
        {opcode::jump, immediate_to_operand(-17)},                   // 30
        {opcode::ret, operand{1}}                                    // 32
      }
    ),
    {
      integer_to_ptr(0),
      integer_to_ptr(10),
      integer_to_ptr(1)
    },
    7,
    0
  );
  auto result = call_root(ctx, global, {});
  EXPECT_EQ(assume<integer>(result).value(), 45);
}

TEST_F(interpreter, exec_native_call) {
  auto native = [] (vm&, ptr<native_procedure>, object_span args) {
    return integer_to_ptr(integer{2 * expect<integer>(args[0]).value()
                                  + 3 * expect<integer>(args[1]).value()
                                  + 5 * expect<integer>(args[2]).value()});
  };
  auto global = make_procedure(
    ctx,
    make_bytecode({{opcode::load_constant, operand{0}, operand{1}},
                   {opcode::load_constant, operand{1}, operand{2}},
                   {opcode::load_constant, operand{2}, operand{3}},
                   {opcode::load_constant, operand{3}, operand{4}},
                   {opcode::call, operand{1}, operand{3}, operand{1}},
                   {opcode::ret, operand{1}}}),
    {
      make<native_procedure>(ctx, native),
      integer_to_ptr(10),
      integer_to_ptr(20),
      integer_to_ptr(30)
    },
    6,
    0
  );
  auto result = call_root(ctx, global, {});
  EXPECT_EQ(assume<integer>(result).value(),
            2 * 10 + 3 * 20 + 5 * 30);
}

TEST_F(interpreter, exec_closure_ref) {
  ptr<procedure_prototype> add = make_procedure_prototype(
    ctx,
    make_bytecode({{opcode::add, operand{1}, operand{2}, operand{1}},
                   {opcode::ret, operand{1}}}),
    procedure_prototype::meta{
      .locals_size = 3,
      .num_required_args = 1,
      .num_leading_args = 1,
      .has_rest = false,
      .parameter_names = std::make_shared<ptr<keyword>[]>(1),
      .name = std::make_shared<std::string>("add"),
      .debug_info = std::make_shared<debug_info_map>()
    },
    {}
  );
  auto global = make_procedure(
    ctx,
    make_bytecode({{opcode::load_constant, operand{0}, operand{2}},
                   {opcode::load_constant, operand{1}, operand{3}},
                   {opcode::load_constant, operand{2}, operand{4}},
                   {opcode::make_closure, operand{2}, operand{1}, operand{3}},
                   {opcode::call, operand{3}, operand{1}, operand{1}},
                   {opcode::ret, operand{1}}}),
    {
      add,
      integer_to_ptr(3),
      integer_to_ptr(5)
    },
    5, 0
  );
  auto result = call_root(ctx, global, {});
  EXPECT_EQ(assume<integer>(result).value(), 5 + 3);
}

TEST_F(interpreter, exec_cons) {
  auto global = make_procedure(
    ctx,
    make_bytecode(
      {{opcode::load_null,     operand{1}},
       {opcode::load_constant, operand{0}, operand{2}},
       {opcode::load_constant, operand{1}, operand{3}},
       {opcode::load_constant, operand{2}, operand{4}},
       {opcode::cons,          operand{4}, operand{1}, operand{1}},
       {opcode::cons,          operand{3}, operand{1}, operand{1}},
       {opcode::cons,          operand{2}, operand{1}, operand{1}},
       {opcode::ret,           operand{1}}}
    ),
    {
      integer_to_ptr(1),
      integer_to_ptr(2),
      integer_to_ptr(3)
    },
    5, 0
  );
  auto result = call_root(ctx, global, {});
  EXPECT_TRUE(equal(result, read("(1 2 3)")));
}

TEST_F(interpreter, exec_car_cdr) {
  auto p = track(ctx, cons(ctx, integer_to_ptr(1), integer_to_ptr(2)));

  auto first = make_procedure(
    ctx,
    make_bytecode({{opcode::car, operand{1}, operand{1}},
                   {opcode::ret, operand{1}}}),
    {},
    2, 1
  );
  auto result1 = call_root(ctx, first, {p.get()});
  EXPECT_EQ(expect<integer>(result1).value(), 1);

  auto second = make_procedure(
    ctx,
    make_bytecode({{opcode::cdr, operand{1}, operand{1}},
                   {opcode::ret, operand{1}}}),
    {},
    2, 1
  );
  auto result2 = call_root(ctx, second, {p.get()});
  EXPECT_EQ(expect<integer>(result2).value(), 2);
}

TEST_F(interpreter, test_eq) {
  auto are_eq = track(
    ctx,
    make_procedure(
      ctx,
      make_bytecode({{opcode::eq, operand{1}, operand{2}, operand{1}},
                     {opcode::ret, operand{1}}}),
      {},
      3, 2
    )
  );

  auto result1 = call_root(
    ctx, are_eq.get(), {ctx.intern("foo"), ctx.intern("bar")}
  );
  EXPECT_EQ(result1, ctx.constants->f);

  auto result2 = call_root(
    ctx, are_eq.get(), {ctx.intern("foo"), ctx.intern("foo")}
  );
  EXPECT_EQ(result2, ctx.constants->t);
}

TEST_F(interpreter, exec_load_dynamic_top_level) {
  operand index = ctx.add_top_level(ctx.intern("foo"), "top-level");
  auto var = make<top_level_variable>(ctx, "top-level", index);
  auto top_level_scope = make<scope>(ctx, ctx, "top-level-scope");
  auto id = make<syntax>(ctx, ctx.intern("top-level"),
                         scope_set{top_level_scope});
  define(ctx.store, id, var);

  auto f = make_procedure(
    ctx,
    make_bytecode({
      {opcode::load_dynamic_top_level, operand{0}, operand{1}},
      {opcode::ret, operand{1}}
    }),
    {
      id
    },
    2, 0
  );
  auto result = call_root(ctx, f, {});
  EXPECT_EQ(expect<symbol>(result)->value(), "foo");
}

TEST_F(interpreter, load_self_in_plain_procedure) {
  auto f = track(
    ctx,
    make_procedure(
      ctx,
      make_bytecode({{opcode::ret, operand{0}}}),
      {},
      1, 0
    )
  );
  auto result = call_root(ctx, f.get(), {});
  EXPECT_EQ(result, f.get());
}

TEST_F(interpreter, load_self_in_closure) {
  auto f = make_procedure_prototype(
    ctx,
    make_bytecode({{opcode::ret, operand{0}}}),
    procedure_prototype::meta{
      .locals_size = 2,
      .num_required_args = 0,
      .num_leading_args = 0,
      .has_rest = false,
      .parameter_names = std::make_shared<ptr<keyword>[]>(0),
      .name = std::make_shared<std::string>("f"),
      .debug_info = std::make_shared<debug_info_map>()
    },
    {}
  );
  auto global = make_procedure(
    ctx,
    make_bytecode({
      {opcode::load_constant, operand{0}, operand{1}},
      {opcode::load_constant, operand{1}, operand{2}},
      {opcode::make_closure, operand{1}, operand{1}, operand{1}},
      {opcode::call, operand{1}, operand{0}, operand{2}},
      {opcode::eq, operand{1}, operand{2}, operand{1}},
      {opcode::ret, operand{1}},
    }),
    {
      f,
      integer_to_ptr(0)
    },
    3, 0
  );
  auto result = call_root(ctx, global, {});
  EXPECT_EQ(result, ctx.constants->t);
}

static integer
apply_and_double(context& ctx, ptr<procedure> f, ptr<> arg) {
  return 2 * expect<integer>(call_root(ctx, f, {arg})).value();
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
  ptr<> result = call_root(
    ctx, f, {to_scheme(ctx, 0), to_scheme(ctx, 1), to_scheme(ctx, 2)}
  );
  EXPECT_TRUE(equal(result, read("(0 1 2)")));
}

TEST_F(interpreter, call_variadic_procedure_with_empty_tail_arg) {
  ptr<> result = eval_module(
    R"(
      (import (insider internal))

      (define foo
        (lambda (a . rest)
          rest))

      (foo 2)
    )",
    no_optimisations
  );
  EXPECT_EQ(result, ctx.constants->null);
}

TEST_F(interpreter, variadic_argument_is_a_list) {
  ptr<> result = eval_module(
    R"(
      (import (insider internal))

      (define foo
        (lambda x x))

      (define bar
        (lambda (a)
          (let ((r (foo 0)))
            r)))

      (bar 2)
    )",
    no_optimisations
  );
  EXPECT_TRUE(is_list(result));
}


TEST_F(interpreter, call_lambda_with_optional_param_supplied) {
  ptr<> result = eval(
    R"(
      (let ((f (lambda (a (b #f)) (+ a b))))
        (f 1 2))
    )"
  );
  EXPECT_EQ(expect<integer>(result).value(), 3);
}

TEST_F(interpreter, call_lambda_with_optional_param_not_supplied) {
  ptr<> result = eval(
    R"(
      (let ((f (lambda (a (b #default-value)) (cons a b))))
        (f 0))
    )"
  );
  auto p = expect<pair>(result);
  EXPECT_EQ(expect<integer>(car(p)).value(), 0);
  EXPECT_EQ(cdr(p), ctx.constants->default_value);
}

TEST_F(interpreter, call_lambda_with_optionals_and_tail) {
  ptr<> result1 = eval(
    R"(
      (let ((f (lambda (a (b #f) . rest)
                 (list a b rest))))
        (f 1 2 3 4))
    )"
  );
  EXPECT_TRUE(equal(result1, read("(1 2 (3 4))")));

  ptr<> result2 = eval(
    R"(
      (let ((f (lambda (a (b #f) . rest)
                 (list a b rest))))
        (f 1 2 3))
    )"
  );
  EXPECT_TRUE(equal(result2, read("(1 2 (3))")));

  ptr<> result3 = eval(
    R"(
      (let ((f (lambda (a (b #f) . rest)
                 (list a b rest))))
        (f 1 2))
    )"
  );
  EXPECT_TRUE(equal(result3, read("(1 2 ())")));
  
  ptr<> result4 = eval(
    R"(
      (let ((f (lambda (a (b #f) . rest)
                 (list a b rest))))
        (f 1))
    )"
  );
  EXPECT_TRUE(equal(result4, read("(1 #f ())")));
}

TEST_F(interpreter,
       call_lambda_with_optional_default_expr_depending_on_required_arg) {
  ptr<> result = eval(R"(
    (let ((f (lambda (a (b (+ a 1))) (list a b))))
      (f 1))
  )");
  EXPECT_TRUE(equal(result, read("(1 2)")));
}

TEST_F(interpreter,
       call_lambda_with_optional_default_expr_depending_on_optional_arg) {
  ptr<> result = eval(R"(
    (let ((f (lambda ((a 1) (b (+ a 1))) (list a b))))
      (f))
  )");
  EXPECT_TRUE(equal(result, read("(1 2)")));
}

TEST_F(interpreter, calling_lambda_with_not_enough_args_throws) {
  EXPECT_THROW(
    eval(
      R"(
        (let ((f (lambda (a b (c #f)) #t)))
          (f 0))
      )"
    ),
    std::runtime_error
  );
}

TEST_F(interpreter, call_keyword_args_in_original_order) {
  ptr<> result = eval(R"(
    (let ((f (lambda (#:one a #:two b) (list a b))))
      (f #:one 1 #:two 2))
  )");
  EXPECT_TRUE(equal(result, read("(1 2)")));
}

TEST_F(interpreter, call_keyword_args_in_non_original_order) {
  ptr<> result = eval(R"(
    (let ((f (lambda (#:one a #:two b #:three c) (list a b c))))
      (f #:three 3 #:one 1 #:two 2))
  )");
  EXPECT_TRUE(equal(result, read("(1 2 3)")));
}

TEST_F(interpreter, call_keyword_args_by_position) {
  ptr<> result = eval(R"(
    (let ((f (lambda (#:one a #:two b) (list a b))))
      (f 1 2))
  )");
  EXPECT_TRUE(equal(result, read("(1 2)")));
}

TEST_F(interpreter, mix_by_position_and_named_arguments) {
  ptr<> result = eval(R"(
    (let ((f (lambda (#:one a #:two b #:three c) (list a b c))))
      (f 3 #:two 2 #:one 1))
  )");
  EXPECT_TRUE(equal(result, read("(1 2 3)")));
}

TEST_F(interpreter, keyword_args_followed_by_tail) {
  ptr<> result = eval(R"(
    (let ((f (lambda (#:one a #:two b . rest) (cons a (cons b rest)))))
      (f #:two 2 #:one 1 3 4))
  )");
  EXPECT_TRUE(equal(result, read("(1 2 3 4)")));
}

TEST_F(interpreter, keyword_and_positional_args_followed_by_tail) {
  ptr<> result = eval(R"(
    (let ((f (lambda (a b #:three c #:four d . rest)
               (cons a (cons b (cons c (cons d rest)))))))
      (f 1 #:three 3 2 5 6 #:four 4 7 8))
  )");
  EXPECT_TRUE(equal(result, read("(1 2 3 4 5 6 7 8)")));
}

TEST_F(interpreter, supply_optional_keyword_arg) {
  ptr<> result = eval(R"(
    (let ((f (lambda (#:one a #:two (b #f)) (list a b))))
      (f #:two 2 #:one 1))
  )");
  EXPECT_TRUE(equal(result, read("(1 2)")));
}

TEST_F(interpreter, dont_supply_optional_keyword_arg) {
  ptr<> result = eval(R"(
    (let ((f (lambda (#:one a #:two (b #f)) (list a b))))
      (f #:one 1))
  )");
  EXPECT_TRUE(equal(result, read("(1 #f)")));
}

TEST_F(interpreter, optional_unnamed_and_named_args) {
  ptr<> result = eval(R"(
    (let ((f (lambda (a (b #f) #:three (c #f) #:four (d #f))
               (list a b c d))))
      (f #:three 3 1))
  )");
  EXPECT_TRUE(equal(result, read("(1 #f 3 #f)")));
}

TEST_F(interpreter, keyword_optional_and_tail) {
  ptr<> result1 = eval(R"(
    (let ((f (lambda (a (b #f) #:three (c #f) . rest)
               (cons a (cons b (cons c rest))))))
      (f #:three 3 1))
  )");
  EXPECT_TRUE(equal(result1, read("(1 #f 3)")));

  ptr<> result2 = eval(R"(
    (let ((f (lambda (a (b #f) #:three (c #f) . rest)
               (cons a (cons b (cons c rest))))))
      (f #:three 3 1 2 4 5))
  )");
  EXPECT_TRUE(equal(result2, read("(1 2 3 4 5)")));
}

TEST_F(interpreter, keyword_arg_followed_by_unnamed) {
  ptr<> result1 = eval(R"(
    (let ((f (lambda (#:one a b) (list a b))))
      (f #:one 1 2))
  )");
  EXPECT_TRUE(equal(result1, read("(1 2)")));

  ptr<> result2 = eval(R"(
    (let ((f (lambda (#:one a b) (list a b))))
      (f 2 #:one 1))
  )");
  EXPECT_TRUE(equal(result2, read("(1 2)")));
}

TEST_F(interpreter, required_keyword_not_supplied_throws) {
  EXPECT_THROW(
    eval(R"(
      (let ((f (lambda (#:one a #:two b) (list a b))))
        (f #:two 2))
    )"),
    std::runtime_error
  );
}

TEST_F(interpreter, required_positional_arg_not_supplied_throws) {
  EXPECT_THROW(
    eval(R"(
      (let ((f (lambda (a #:two b) (list a b))))
        (f #:two 2))
    )"),
    std::runtime_error
  );
}

static ptr<>
do_recursion(vm& state, int i, int accum, ptr<> recurse, ptr<> base) {
  context& ctx = state.ctx;
  if (i == 0)
    return tail_call(state, base, {to_scheme(ctx, accum)});
  else
    return tail_call(state, recurse, {to_scheme(ctx, i - 1),
                                      to_scheme(ctx, accum + i), recurse, base});
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
  ptr<> result = insider::eval(ctx, m, "(* 7 3)");
  EXPECT_EQ(expect<integer>(result).value(), 21);
}

TEST_F(repl_fixture, define_top_level_in_interactive_module) {
  insider::eval(ctx, m, "(define x 7)");
  insider::eval(ctx, m, "(define y 3)");
  ptr<> result = insider::eval(ctx, m, "(* x y)");
  EXPECT_EQ(expect<integer>(result).value(), 21);
}

TEST_F(repl_fixture, reference_variable_before_definition) {
  insider::eval(ctx, m, "(define f (lambda () (* a 2)))");
  EXPECT_THROW(insider::eval(ctx, m, "(f)"), unbound_variable_error);
  insider::eval(ctx, m, "(define a 4)");
  ptr<> result = insider::eval(ctx, m, "(f)");
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
  ptr<> result = insider::eval(ctx, m, "(f)");
  EXPECT_EQ(expect<integer>(result).value(), 4);
}

TEST_F(repl_fixture, define_syntax_in_repl) {
  insider::eval(ctx, m, "(define-syntax a (lambda (stx) #'4))");
  ptr<> result = insider::eval(ctx, m, "(a)");
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
  ptr<> result = insider::eval(ctx, m, "x");
  EXPECT_EQ(expect<integer>(result).value(), 2);
}

TEST_F(repl_fixture, redefine_syntax_in_repl) {
  insider::eval(ctx, m, "(define-syntax a (lambda (stx) #'4))");
  insider::eval(ctx, m, "(define f (lambda () (a)))");
  ptr<> result1 = insider::eval(ctx, m, "(f)");
  EXPECT_EQ(expect<integer>(result1).value(), 4);

  // f still uses the definition that was visible when it was defined.
  insider::eval(ctx, m, "(define-syntax a (lambda (stx) #'8))");
  ptr<> result2 = insider::eval(ctx, m, "(f)");
  EXPECT_EQ(expect<integer>(result2).value(), 4);

  // The new definition of f will use the new definition of a.
  insider::eval(ctx, m, "(define f (lambda () (a)))");
  ptr<> result3 = insider::eval(ctx, m, "(f)");
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
      )");
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
  ptr<> result = insider::eval(ctx, m, "var");
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
  ptr<> result = insider::eval(ctx, m, "x");
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
  ptr<> result = insider::eval(ctx, m, "(introduce x (set! x 1) x)");
  EXPECT_EQ(expect<integer>(result).value(), 1);
}

TEST_F(repl_fixture, set_variable_introduced_by_macro_in_meta) {
  insider::eval(ctx, m, introduce_macro_def);
  ptr<> result
    = insider::eval(ctx, m, "(meta (introduce x (set! x 1) x))");
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
  EXPECT_TRUE(ctx.last_exception_stacktrace.empty());

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
    EXPECT_FALSE(ctx.last_exception_stacktrace.empty());
    return;
  }

  FAIL();
}

static void
always_throw() {
  throw std::runtime_error{"C++ exception"};
}

TEST_F(interpreter, calling_native_directly_from_native_does_not_pollute_stack) {
  define_procedure<always_throw>(ctx, "always-throw", ctx.internal_module());
  ptr<> f = eval("always-throw");

  vm state{ctx};
  ASSERT_TRUE(state.stack.empty());
  ASSERT_THROW(call_with_continuation_barrier(state, f, {}), std::runtime_error);
  EXPECT_TRUE(state.stack.empty());
}

TEST_F(interpreter, mutating_immutable_top_level_throws) {
  auto var = define_top_level(ctx, "var", ctx.internal_module(), true,
                              integer_to_ptr(0));
  EXPECT_THROW(ctx.set_top_level(var, integer_to_ptr(1)), std::runtime_error);
}

TEST_F(interpreter, mutating_mutable_top_level_doesnt_throw) {
  auto var = define_top_level_mutable(ctx, "var", ctx.internal_module(), true,
                                      integer_to_ptr(0));
  EXPECT_NO_THROW(ctx.set_top_level(var, integer_to_ptr(1)));
}

static void
check_stacktrace(vm& state) {
  auto trace = make_stacktrace(state);
  ASSERT_EQ(trace.size(), 5);

  EXPECT_EQ(trace[0].name, "check-stacktrace!");
  EXPECT_EQ(trace[0].kind, stacktrace_record::kind::native);

  EXPECT_EQ(trace[1].name, "one");
  EXPECT_EQ(trace[1].kind, stacktrace_record::kind::scheme);

  EXPECT_EQ(trace[2].name, "two");
  EXPECT_EQ(trace[2].kind, stacktrace_record::kind::scheme);

  EXPECT_EQ(trace[3].name, "three");
  EXPECT_EQ(trace[3].kind, stacktrace_record::kind::scheme);

  // The toplevel.
  EXPECT_EQ(trace[4].kind, stacktrace_record::kind::scheme);
}

TEST_F(interpreter, stack_trace_can_reconstruct_inlined_procedures) {
  define_procedure<check_stacktrace>(ctx, "check-stacktrace!",
                                     ctx.internal_module());

  eval_module(
    R"(
      (import (insider internal))

      (define one (lambda () (check-stacktrace!) #void))
      (define two (lambda () (one) #void))
      (define three (lambda () (two) #void))
      (three)
    )"
  );
}
