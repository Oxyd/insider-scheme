#include "scheme_fixture.hpp"

#include "converters.hpp"
#include "numeric.hpp"

using namespace insider;

struct compiler : scheme_fixture { };

TEST_F(compiler, compile_arithmetic) {
  ptr<> result = eval(
    "(+ 2 3 (* 5 9) (- 9 8) (/ 8 2))"
  );
  EXPECT_EQ(expect<integer>(result).value(),
            2 + 3 + (5 * 9) + (9 - 8) + (8 / 2));
}

TEST_F(compiler, compile_let) {
  ptr<> result = eval(
    R"(
      (let ((a 2)
            (b 5))
        (let ((sum (+ a b))
              (product (* a b)))
          (- sum product)))
    )"
  );

  int a = 2;
  int b = 5;
  int sum = a + b;
  int product = a * b;
  EXPECT_EQ(expect<integer>(result).value(), sum - product);

  EXPECT_THROW(compile_expression(ctx, read_syntax(ctx, "(let ((a 2)))"), ctx.internal_module),
               std::runtime_error);
  EXPECT_THROW(compile_expression(ctx, read_syntax(ctx, "(let foo)"), ctx.internal_module),
               std::runtime_error);
}

TEST_F(compiler, let_shadowing) {
  ptr<> result = eval(
    R"(
      (let ((a 2))
        (let ((a 5))
          a))
    )"
  );
  EXPECT_EQ(expect<integer>(result).value(), 5);
}

TEST_F(compiler, letrec) {
  auto result1 = eval(R"(
    (letrec* ((f (lambda (n accum)
                   (if (= n 0)
                       accum
                       (f (- n 1) (* n accum)))))
              (factorial (lambda (n) (f n 1))))
      (factorial 5))
  )");
  EXPECT_EQ(expect<integer>(result1).value(), 120);
}

TEST_F(compiler, core_shadowing) {
  auto result1 = eval("(let ((let 'let)) let)");
  EXPECT_EQ(expect<symbol>(result1)->value(), "let");

  auto result2 = eval("(let ((unquote 'x)) `(1 ,2 3))");
  EXPECT_TRUE(equal(ctx, result2, read("(1 (unquote 2) 3)")));
}

TEST_F(compiler, compile_lambda) {
  ptr<> result1 = eval(
    R"(
      (let ((twice (lambda (x) (* 2 x))))
        (twice 4))
    )"
  );
  EXPECT_EQ(expect<integer>(result1).value(), 8);

  ptr<> result2 = eval(
    R"(
      (let ((sum (lambda (a b c d) (+ a b c d))))
        (sum 1 2 3 4))
    )"
  );
  EXPECT_EQ(expect<integer>(result2).value(), 1 + 2 + 3 + 4);

  ptr<> result3 = eval(
    R"(
      (let ((call-with-sum (lambda (f a b) (f (+ a b))))
            (f (lambda (x) (* 2 x))))
        (call-with-sum f 3 4))
    )"
  );
  EXPECT_EQ(expect<integer>(result3).value(), 2 * (3 + 4));

  ptr<> result4 = eval(
    R"(
      (let ((list (lambda args args)))
        (list 1 2 3))
   )"
  );
  EXPECT_TRUE(equal(ctx, result4, make_list(ctx, integer_to_ptr(integer{1}), integer_to_ptr(integer{2}), integer_to_ptr(integer{3}))));

  ptr<> result5 = eval(
    R"(
      (let ((increment (lambda (value . rest)
                         (let ((addend (if (eq? rest '()) 1 (car rest))))
                           (+ value addend)))))
        (cons (increment 2) (increment 7 3)))
   )"
  );
  EXPECT_EQ(expect<integer>(car(expect<pair>(result5))).value(), 3);
  EXPECT_EQ(expect<integer>(cdr(expect<pair>(result5))).value(), 10);

  ptr<> result6 = eval(
    R"(
      (let ((const (lambda () 2)))
        (const))
   )"
  );
  EXPECT_EQ(expect<integer>(result6).value(), 2);
}

TEST_F(compiler, compile_if) {
  ptr<> result1 = eval("(if #t 2 3)");
  EXPECT_EQ(expect<integer>(result1).value(), 2);

  ptr<> result2 = eval("(if #f 2 3)");
  EXPECT_EQ(expect<integer>(result2).value(), 3);

  ptr<> result3 = eval("(if #t 2)");
  EXPECT_EQ(expect<integer>(result3).value(), 2);

  ptr<> result4 = eval("(if #f 2)");
  EXPECT_EQ(result4, ctx.constants->void_.get());

  ptr<> result5 = eval(
    R"(
      (let ((f (lambda (x) (* 2 x)))
            (x 4))
        (if (< x 5)
              (f x)
              0))
    )"
  );
  EXPECT_EQ(expect<integer>(result5).value(), 8);

  ptr<> result6 = eval(
    R"(
      (let ((f (lambda (x) (* 2 x)))
            (x 6))
        (if (< x 5)
              (f x)
              0))
    )"
  );
  EXPECT_EQ(expect<integer>(result6).value(), 0);;

  ptr<> result7 = eval(
    R"(
      (let ((f (lambda (x) (* 2 x)))
            (x 4))
        (if (< x 5)
              0
              (f x)))
    )"
  );
  EXPECT_EQ(expect<integer>(result7).value(), 0);

  ptr<> result8 = eval(
    R"(
      (let ((f (lambda (x) (* 2 x)))
            (x 6))
        (if (< x 5)
              0
              (f x)))
    )"
  );
  EXPECT_EQ(expect<integer>(result8).value(), 12);

  ptr<> result9 = eval(
    R"(
      (let ((f (lambda (x) (* 2 x)))
            (g (lambda (x) (+ 2 x)))
            (x 4))
        (if (< x 5)
              (f x)
              (g x)))
    )"
  );
  EXPECT_EQ(expect<integer>(result9).value(), 8);

  ptr<> result10 = eval(
    R"(
      (let ((f (lambda (x) (* 2 x)))
            (g (lambda (x) (+ 10 x)))
            (x 6))
        (if (< x 5)
              (f x)
              (g x)))
    )"
  );
  EXPECT_EQ(expect<integer>(result10).value(), 16);

  ptr<> result11 = eval(
    R"(
      (let ((loop #void))
        (set! loop (lambda (list result)
                     (if (eq? list '())
                         result
                         (loop (cdr list)
                               (if (> (car list) result)
                                   (car list)
                                   result)))))
        (loop '(12 11 14 15 3 8) 0))
    )"
  );
  EXPECT_EQ(expect<integer>(result11).value(), 15);
}

TEST_F(compiler, compile_closure) {
  ptr<> result1 = eval(
    R"(
      (let ((make-adder (lambda (x) (lambda (y) (+ x y)))))
        (let ((add-2 (make-adder 2)))
          (add-2 5)))
    )"
  );
  EXPECT_EQ(expect<integer>(result1).value(), 7);

  ptr<> result2 = eval(
    R"(
      (let ((x 7))
        (let ((f (lambda (y) (+ x y))))
          (f 3)))
    )"
  );
  EXPECT_EQ(expect<integer>(result2).value(), 10);
}

TEST_F(compiler, compile_set) {
  ptr<> result1 = eval(
    R"(
      (let ((x 2))
        (set! x 5)
        x)
    )"
  );
  EXPECT_EQ(expect<integer>(result1).value(), 5);

  ptr<> result2 = eval(
    R"(
      (let ((fact #void))
        (set! fact (lambda (n)
                     (if (= n 0)
                       1
                       (* n (fact (- n 1))))))
        (fact 5))
    )"
  );
  EXPECT_EQ(expect<integer>(result2).value(), 120);

  ptr<> result3 = eval(
    R"(
      (let ((f (lambda (x)
                 (set! x (* 2 x))
                 (lambda (y)
                   (+ x y)))))
        ((f 5) 3))
    )"
  );
  EXPECT_EQ(expect<integer>(result3).value(), 13);
}

TEST_F(compiler, compile_box) {
  ptr<> result = eval(
    R"(
      (let ((b1 (box 5))
            (b2 (box 7)))
        (box-set! b1 (+ (unbox b1) (unbox b2)))
        (unbox b1))
    )"
  );
  EXPECT_EQ(expect<integer>(result).value(), 12);
}

TEST_F(compiler, compile_sequence) {
  ptr<> result = eval(R"(
    (let ((a 0)
          (b 0))
      (if #t
          (begin
            (set! a 1)
            (set! b 2))
          'unpossible)
      (+ a b))
  )");
  EXPECT_EQ(expect<integer>(result).value(), 3);
}

TEST_F(compiler, compile_higher_order_arithmetic) {
  ptr<> result = eval(
    R"(
      (let ((f (lambda (op x y) (op x y))))
        (f + 2 3))
    )"
  );
  EXPECT_EQ(expect<integer>(result).value(), 5);
}

TEST_F(compiler, compile_module) {
  int sum = 0;
  define_top_level(
    ctx, "f", ctx.internal_module, true,
    make<native_procedure>(ctx,
                           [&] (context& ctx, object_span args) {
                             sum += expect<integer>(args[0]).value();
                             return ctx.constants->void_.get();
                           })
  );

  auto m = compile_main_module(ctx,
                               read_syntax_multiple(ctx,
                                                    "(import (insider internal))"
                                                    "(f 3)"
                                                    "(let ((x 2))"
                                                    "  (f x))"));
  call_with_continuation_barrier(ctx, m.top_level_procedure(), {});
  EXPECT_EQ(sum, 5);
}

TEST_F(compiler, compile_top_level_define) {
  auto result1 = eval_module(
    R"(
      (import (insider internal))
      (define f
        (lambda (x)
          (+ x 2)))
      (define var 7)
      (f var)
    )"
  );
  EXPECT_EQ(expect<integer>(result1).value(), 9);

  auto result2 = eval_module(
    R"(
      (import (insider internal))
      (define x 4)
      (define y 7)
      (set! x (+ y 2))
      x
    )"
  );
  EXPECT_EQ(expect<integer>(result2).value(), 9);

  auto result3 = eval_module(R"(
    (import (insider internal))

    (define a 1)
    (begin
      (define b 2)
      (define c 3)
      (begin
        (define d 4)
        (define e 5))
      (define f 6))
    (define g 7)
    (+ a b c d e f g)
  )");
  EXPECT_EQ(expect<integer>(result3).value(), 1 + 2 + 3 + 4 + 5 + 6 + 7);
}

TEST_F(compiler, compile_internal_define) {
  auto result1 = eval_module(
    R"(
      (import (insider internal))

      (define f
        (lambda (n)
          (define go
            (lambda (k accum)
              (if (= k 0)
                accum
                (go (- k 1) (+ accum k)))))
          (go n 0)))
      (f 5)
    )"
  );
  EXPECT_EQ(expect<integer>(result1).value(), 5 + 4 + 3 + 2 + 1);

  auto result2 = eval_module(R"(
    (import (insider internal))

    (define f
      (lambda (x y)
        (define sum (+ x y))
        (begin
          (define product (* x y))
          (define sum-of-squares (+ (* x x) (* y y))))
        (+ sum product sum-of-squares)))
    (f 4 5)
  )");
  EXPECT_EQ(expect<integer>(result2).value(), 4 + 5 + 4 * 5 + 4 * 4 + 5 * 5);
}

TEST_F(compiler, define_lambda) {
  define_procedure(
    ctx, "f", ctx.internal_module, true,
    [] (int a, int b) { return 2 * a + b; }
  );

  int x = 0;
  define_procedure<void(int)>(
    ctx, "g", ctx.internal_module, true,
    [&] (int a) { x += a; }
  );

  define_procedure(
    ctx, "to-string", ctx.internal_module, true,
    [] (int i) { return std::to_string(i); }
  );

  auto result1 = eval("(f 5 7)");
  EXPECT_EQ(expect<integer>(result1).value(), 2 * 5 + 7);

  eval("(g 9)");
  EXPECT_EQ(x, 9);

  auto result3 = eval("(to-string 3)");
  EXPECT_EQ(expect<string>(result3)->value(), "3");
}

TEST_F(compiler, quote) {
  auto result1 = eval("(quote (a b c))");
  EXPECT_TRUE(is_list(result1));
  EXPECT_EQ(list_length(result1), 3);

  auto result2 = eval("(quote 2)");
  EXPECT_EQ(expect<integer>(result2).value(), 2);

  auto result3 = eval("'3");
  EXPECT_EQ(expect<integer>(result3).value(), 3);

  auto result4 = eval("'(a b)");
  EXPECT_TRUE(is_list(result4));
  EXPECT_EQ(list_length(result4), 2);
  EXPECT_EQ(expect<symbol>(car(expect<pair>(result4)))->value(), "a");
  EXPECT_EQ(expect<symbol>(cadr(expect<pair>(result4)))->value(), "b");

  auto result5 = eval("''a");
  EXPECT_TRUE(is_list(result5));
  EXPECT_EQ(list_length(result5), 2);
  EXPECT_EQ(expect<symbol>(car(expect<pair>(result5)))->value(), "quote");
  EXPECT_EQ(expect<symbol>(cadr(expect<pair>(result5)))->value(), "a");
}

TEST_F(compiler, syntax) {
  auto result1 = eval("(syntax (a b c))");
  ASSERT_TRUE(is<syntax>(result1));

  auto result1_expr = assume<syntax>(result1)->expression();
  ASSERT_TRUE(is<pair>(result1_expr));
  ASSERT_TRUE(is<syntax>(car(assume<pair>(result1_expr))));

  auto car_stx = assume<syntax>(car(assume<pair>(result1_expr)));
  ASSERT_TRUE(is<symbol>(car_stx->expression()));
  EXPECT_EQ(assume<symbol>(car_stx->expression())->value(), "a");

  auto result2 = eval("#'(a b c)");
  ASSERT_TRUE(is<syntax>(result2));
  EXPECT_TRUE(equal(ctx, syntax_to_datum(ctx, assume<syntax>(result2)), read("(a b c)")));
}

TEST_F(compiler, quasiquote) {
  auto result1 = eval("`5");
  EXPECT_TRUE(equal(ctx, result1, read("5")));

  auto result2 = eval("`(1 2 5)");
  EXPECT_TRUE(equal(ctx, result2, read("(1 2 5)")));

  auto result3 = eval("(let ((a 7)) `(1 ,a 3))");
  EXPECT_TRUE(equal(ctx, result3, read("(1 7 3)")));

  auto result4 = eval("`(1 ,(+ 2 3) 3)");
  EXPECT_TRUE(equal(ctx, result4, read("(1 5 3)")));

  auto result5 = eval("(let ((name 'a)) `(list ,name ',name))");
  EXPECT_TRUE(equal(ctx, result5, read("(list a (quote a))")));

  auto result6 = eval("`#(1 2 5)");
  EXPECT_TRUE(equal(ctx, result6, read("#(1 2 5)")));

  auto result7 = eval("(let ((a 12)) `#(3 ,a 5 ,(* a 2) 9))");
  EXPECT_TRUE(equal(ctx, result7, read("#(3 12 5 24 9)")));

  auto result8 = eval("(let ((b '(b1 b2 b3))) `(a1 a2 ,@b c1 c2))");
  EXPECT_TRUE(equal(ctx, result8, read("(a1 a2 b1 b2 b3 c1 c2)")));

  auto result9 = eval("(let ((b '(b1 b2 b3))) `(a1 a2 ,b c1 c2))");
  EXPECT_TRUE(equal(ctx, result9, read("(a1 a2 (b1 b2 b3) c1 c2)")));

  auto result10 = eval("(let ((b '(b1 b2))) `(a1 a2 ,@b))");
  EXPECT_TRUE(equal(ctx, result10, read("(a1 a2 b1 b2)")));

  auto result11 = eval("``(a b ,c)");
  EXPECT_TRUE(equal(ctx, result11, read("(quasiquote (a b (unquote c)))")));

  auto result12 = eval("(let ((b '(b1 b2 b3))) `#(a1 a2 ,@b c1 c2))");
  EXPECT_TRUE(equal(ctx, result12, read("#(a1 a2 b1 b2 b3 c1 c2)")));

  auto result13 = eval("(let ((b '(b1 b2 b3))) `#(a1 a2 ,b c1 c2))");
  EXPECT_TRUE(equal(ctx, result13, read("#(a1 a2 (b1 b2 b3) c1 c2)")));

  auto result14 = eval("(let ((b '(a1 a2))) `#(,@b b1 b2 b3))");
  EXPECT_TRUE(equal(ctx, result14, read("#(a1 a2 b1 b2 b3)")));

  auto result15 = eval("(let ((b '(b1 b2))) `#(a1 a2 ,@b))");
  EXPECT_TRUE(equal(ctx, result15, read("#(a1 a2 b1 b2)")));

  auto result16 = eval("(let ((b '(b1 b2))) ``(a1 a2 ,b c1 c2 ,(d1 d2 ,b e1 e2)))");
  EXPECT_TRUE(equal(ctx, result16, read("(quasiquote (a1 a2 (unquote b) c1 c2 (unquote (d1 d2 (b1 b2) e1 e2))))")));

  auto result17 = eval("(let ((x '(x1 x2))) `(,@x . y))");
  EXPECT_TRUE(equal(ctx, result17, read("(x1 x2 . y)")));

  auto result18 = eval("(let ((x '(x1 x2))) `(a1 a2 ,@x . y))");
  EXPECT_TRUE(equal(ctx, result18, read("(a1 a2 x1 x2 . y)")));

  auto result19 = eval("(let ((x '(x1 x2))) `(,@x))");
  EXPECT_TRUE(equal(ctx, result19, read("(x1 x2)")));

  auto result20 = eval("(let ((x 2) (y 3)) `(,x . ,y))");
  EXPECT_TRUE(equal(ctx, result20, read("(2 . 3)")));

  auto result21 = eval("(let ((x 2)) `(a . `(b (,,x))))");
  EXPECT_TRUE(equal(ctx, result21, read("(a . `(b (,2)))")));
}

TEST_F(compiler, unbound_vars) {
  EXPECT_THROW(eval("foo"), error);
  EXPECT_THROW(eval_module("foo"), error);
  EXPECT_THROW(eval_module(R"((import (insider internal))
                              (define-syntax foo (lambda (stx) #'bar))
                              (foo))"),
               error);
  EXPECT_THROW(eval("(let-syntax ((foo (lambda (stx) #'bar))) (foo))"), error);
}

static bool
is_proper_syntax(ptr<> x) {
  if (!is<syntax>(x))
    return false;

  ptr<syntax> stx = assume<syntax>(x);
  if (syntax_is<pair>(stx)) {
    // cdr's don't have to be syntaxes, but all car's do.

    ptr<> elem = stx;
    while (true) {
      if (semisyntax_is<null_type>(elem))
        return true;

      if (!semisyntax_is<pair>(elem))
        return is_proper_syntax(elem);

      if (!is_proper_syntax(car(semisyntax_expect<pair>(elem))))
        return false;

      elem = cdr(semisyntax_assume<pair>(elem));
    }
  } else if (auto v = syntax_match<vector>(stx)) {
    for (std::size_t i = 0; i < v->size(); ++i)
      if (!is_proper_syntax(v->ref(i)))
        return false;
  }

  return true;
}

TEST_F(compiler, quasisyntax) {
#define EXPECT_SYNTAX_EQ(x, y)                                          \
  do {                                                                  \
    auto result = x;                                                    \
    EXPECT_TRUE(is_proper_syntax(result));                              \
    EXPECT_TRUE(equal(ctx, syntax_to_datum(ctx, expect<syntax>(result)), y)); \
  } while (false)

  EXPECT_SYNTAX_EQ(eval("#`(a b c)"), read("(a b c)"));
  EXPECT_SYNTAX_EQ(eval("#`(a #,(+ 2 3) c)"), read("(a 5 c)"));
  EXPECT_SYNTAX_EQ(eval("(let ((middle '(x y z))) #`(a b #,@middle c d))"),
                   read("(a b x y z c d)"));
  EXPECT_SYNTAX_EQ(eval("(let ((a 'x) (b 'y)) #`(#,a . #,b))"),
                   read("(x . y)"));
  EXPECT_SYNTAX_EQ(eval("#`#(a #,(+ 9 7) c)"), read("#(a 16 c)"));
  EXPECT_SYNTAX_EQ(eval("(let ((x '(a b c))) #`(#,@x))"), read("(a b c)"));
  EXPECT_SYNTAX_EQ(eval("(let ((middle '(x y z))) #`#(a b #,@middle c d))"),
                   read("#(a b x y z c d)"));

#undef EXPECT_SYNTAX_EQ
}

TEST_F(compiler, call_from_native) {
  auto f = expect<procedure>(eval("(lambda (x y) (+ (* 2 x) (* 3 y)))"));
  ptr<> result = call_with_continuation_barrier(ctx, f, {integer_to_ptr(integer{5}), integer_to_ptr(integer{4})}).get();
  EXPECT_EQ(expect<integer>(result).value(), 2 * 5 + 3 * 4);

  scheme_procedure<int(int, int)> g{track(ctx, eval("(lambda (x y) (+ (* 2 x) (* 3 y)))"))};
  EXPECT_EQ(g(ctx, 5, 4), 2 * 5 + 3 * 4);
}
