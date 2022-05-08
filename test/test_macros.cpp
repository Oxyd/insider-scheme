#include "scheme_fixture.hpp"

#include "runtime/symbol.hpp"

using namespace insider;

struct macros : scheme_fixture { };

TEST_F(macros, top_level_transformers) {
  auto result1 = eval_module(R"(
    (import (insider internal))
    (define-syntax num
      (lambda (x)
        #'4))
    (* (num) 2)
  )");
  EXPECT_EQ(expect<integer>(result1).value(), 8);

  auto result2 = eval_module(R"(
    (import (insider internal))
    (define-syntax when
      (lambda (stx)
        (let ((subforms (syntax->list stx)))
          (let ((test (cadr subforms))
                (body (cddr subforms)))
            #`(if #,test ((lambda () #,@body)) #f)))))

    (define value 4)
    (cons (when (< value 5) (* value 10))
          (when (> value 5) (* value 20)))
  )");
  auto result2p = expect<pair>(result2);
  EXPECT_EQ(expect<integer>(car(result2p)).value(), 40);
  EXPECT_EQ(cdr(result2p), ctx.constants->f);
}

TEST_F(macros, internal_transformers) {
  auto result1 = eval_module(R"(
    (import (insider internal))
    (define foo
      (lambda (x)
        (define-syntax double
          (lambda (stx)
            (let ((var (cadr (syntax->list stx))))
              #`(* 2 #,var))))
        (+ x (double x))))
    (foo 5)
  )");
  EXPECT_EQ(expect<integer>(result1).value(), 5 + 2 * 5);

  auto result2 = eval_module(R"(
    (import (insider internal))
    (define foo
      (lambda (x)
        (define-syntax def
          (lambda (stx)
            (let ((subexprs (syntax->list stx)))
              (let ((name (cadr subexprs))
                    (value (caddr subexprs)))
                #`(define #,name #,value)))))
        (def result (* 2 x))
        result))
    (foo 8)
  )");
  EXPECT_EQ(expect<integer>(result2).value(), 16);
}

TEST_F(macros, hygiene) {
  auto result1 = eval_module(R"(
    (import (insider internal))

    (define-syntax foo
      (lambda (stx)
        (let ((expr (cadr (syntax->list stx))))
          #`(let ((a 10))
              #,expr))))

    (let ((a 5))
      (foo a))
  )");
  EXPECT_EQ(expect<integer>(result1).value(), 5);

  auto result2 = eval_module(R"(
    (import (insider internal))

    (define foo
      (let ((x 1))
        (define-syntax bar
          (lambda (stx)
            #'x))
        (lambda (x)
          (bar))))
    (foo 2)
  )");
  EXPECT_EQ(expect<integer>(result2).value(), 1);
}

TEST_F(macros, transformers_producing_definitions) {
  auto result1 = eval_module(R"(
    (import (insider internal))

    (define-syntax define-double
      (lambda (stx)
        (let ((subexprs (syntax->list stx)))
          (let ((name (cadr subexprs))
                (value (caddr subexprs)))
            #`(define #,name (* 2 #,value))))))

    (define f
      (lambda (x y)
        (define sum (+ x y))
        (define-double result (* 3 sum))
        result))
    (f 4 5)
  )");
  EXPECT_EQ(expect<integer>(result1).value(), 2 * 3 * (4 + 5));

  auto result2 = eval_module(R"(
    (import (insider internal))

    (define-syntax define-two
      (lambda (stx)
        (let ((subexprs (syntax->list stx)))
          (let ((name-1 (cadr subexprs))
                (init-1 (caddr subexprs))
                (name-2 (cadddr subexprs))
                (init-2 (cadddr (cdr subexprs))))
            #`(begin
                (define #,name-1 #,init-1)
                (define #,name-2 #,init-2))))))

    (define f
      (lambda (x y)
        (define-two twice-x (* 2 x)
                    twice-y (* 2 y))
        (+ twice-x twice-y)))

    (f 4 5)
  )");
  EXPECT_EQ(expect<integer>(result2).value(), 2 * 4 + 2 * 5);

  auto result3 = eval_module(R"(
    (import (insider internal))

    (define-syntax define-two
      (lambda (stx)
        (let ((subexprs (syntax->list stx)))
          (let ((name-1 (cadr subexprs))
                (init-1 (caddr subexprs))
                (name-2 (cadddr subexprs))
                (init-2 (cadddr (cdr subexprs))))
            #`(begin
                (define #,name-1 #,init-1)
                (define #,name-2 #,init-2))))))

    (define-two a 7
                b 12)
    (+ a b)
  )");
  EXPECT_EQ(expect<integer>(result3).value(), 7 + 12);

  auto result4 = eval_module(R"(
    (import (insider internal))

    (define-syntax define-identity
      (lambda (stx)
        (let ((id (cadr (syntax->list stx))))
          #`(define #,id (lambda (x) x)))))

    (define-identity f)
    (f 5)
  )");
  EXPECT_EQ(expect<integer>(result4).value(), 5);
}

TEST_F(macros, let_syntax) {
  auto result1 = eval(R"(
    (let-syntax ((given-that (lambda (stx)
                               (let ((subexprs (syntax->list stx)))
                                 (let ((test (cadr subexprs))
                                       (body (cddr subexprs)))
                                   #`(if #,test (begin #,@body)))))))
      (let ((if #t))
        (given-that if (set! if 'now))
        if))
  )");
  EXPECT_EQ(expect<symbol>(result1)->value(), "now");

  auto result2 = eval(R"(
    (let ((x 'outer))
      (let-syntax ((m (lambda (stx) #'x)))
        (let ((x 'inner))
          (m))))
  )");
  EXPECT_EQ(expect<symbol>(result2)->value(), "outer");
}

TEST_F(macros, out_of_scope) {
  EXPECT_THROW(eval_module(R"(
                             (import (insider internal))
                             (define-syntax foo
                               (lambda (stx)
                                 (let ((bar 0))
                                   #'bar)))
                             (foo)
                           )"),
               std::runtime_error);
}

TEST_F(macros, recursive_syntax) {
  auto result1 = eval(R"(
    (letrec-syntax
        ((my-or (lambda (stx)
                  (let ((subexprs (cdr (syntax->list stx))))
                    (if (eq? subexprs '())
                        #'#f
                        (if (eq? (cdr subexprs) '())
                            (car subexprs)
                            #`(let ((temp #,(car subexprs)))
                                (if temp
                                    temp
                                    (my-or #,@(cdr subexprs))))))))))
      (let ((x #f)
            (y 7)
            (temp 8)
            (let (lambda (x) (< x 8)))
            (if (lambda (x) (> x 8))))
        (my-or x (let temp) (if y) y)))
  )");
  EXPECT_EQ(expect<integer>(result1).value(), 7);

  auto result2 = eval_module(R"(
    (import (insider internal))

    (define-syntax identity
      (lambda (stx)
        (let ((misc-id (cadr (syntax->list stx))))
          #`(lambda (x)
              (let ((#,misc-id 'other))
                x)))))

    (define f (identity x))
    (f 2)
  )");

  EXPECT_EQ(expect<integer>(result2).value(), 2);
}

TEST_F(macros, free_identifier_eq) {
  ptr<> result1 = eval("(free-identifier=? #'x #'x)");
  EXPECT_EQ(result1, ctx.constants->t);

  ptr<> result2 = eval("(free-identifier=? #'x #'y)");
  EXPECT_EQ(result2, ctx.constants->f);

  ptr<> result3 = eval_module(R"(
    (import (insider internal))

    (define-syntax aux
      (lambda (stx)
        #'#f))

    (define-syntax is-aux
      (lambda (stx)
        (if (free-identifier=? (cadr (syntax->list stx)) #'aux)
            #'#t
            #'#f)))

    (define-syntax test-for-aux
      (lambda (stx)
        #`(is-aux #,(cadr (syntax->list stx)))))

    (let ((list (lambda l l)))
      (list (is-aux aux) (is-aux is-aux) (is-aux not-aux) (test-for-aux aux) (test-for-aux not-aux)
            (let ((aux 5))
              (is-aux aux))))
  )");
  EXPECT_EQ(car(expect<pair>(result3)), ctx.constants->t);
  EXPECT_EQ(cadr(expect<pair>(result3)), ctx.constants->f);
  EXPECT_EQ(caddr(expect<pair>(result3)), ctx.constants->f);
  EXPECT_EQ(cadddr(expect<pair>(result3)), ctx.constants->t);
  EXPECT_EQ(cadddr(expect<pair>(cdr(expect<pair>(result3)))), ctx.constants->f);
  EXPECT_EQ(cadddr(expect<pair>(cdr(expect<pair>(cdr(expect<pair>(result3)))))), ctx.constants->f);
}

TEST_F(macros, bound_identifier_eq) {
  ptr<pair> result1 = expect<pair>(eval_module(R"(
    (import (insider internal))

    (define-syntax check
      (lambda (stx)
        (let ((elems (syntax->list stx)))
          (let ((x (cadr elems))
                (y (caddr elems)))
            (if (bound-identifier=? x y)
                #'#t
                #'#f)))))

    (let ((list (lambda l l)))
      (list (check a a)
            (check a b)
            (let-syntax ((check-a (lambda (stx)
                                    (let ((x (cadr (syntax->list stx))))
                                      #`(check a #,x)))))
              (check-a a))))
  )"));
  EXPECT_EQ(car(result1), ctx.constants->t);
  EXPECT_EQ(cadr(result1), ctx.constants->f);
  EXPECT_EQ(caddr(result1), ctx.constants->f);
}

TEST_F(macros, exported_transformer_producing_another_transformer) {
  add_source_file(
    "foo.scm",
    R"(
      (library (foo))
      (import (insider internal))
      (export make-transformer)

      (define-syntax make-transformer
        (lambda (stx)
          (let ((name (car (cdr (syntax->list stx)))))
            #`(define-syntax #,name
                (lambda (stx)
                  #'#t)))))
    )"
  );

  auto result = eval_module(R"(
    (import (insider internal) (foo))
    (make-transformer x)
    (x)
  )");
  EXPECT_EQ(result, ctx.constants->t);
}

TEST_F(macros, internal_definition_shadowing_macro_introduced_binding) {
  auto result = eval_module(R"(
    (import (insider internal))

    (define-syntax bind-to-0
      (lambda (stx)
        (let ((exprs (syntax->list stx)))
          (let ((name (car (cdr exprs)))
                (body (cdr (cdr exprs))))
            #`(let ((#,name 0))
                #,@body)))))

    (bind-to-0 x
      (define x 1)
      x)
  )");
  EXPECT_EQ(expect<integer>(result).value(), 1);
}

TEST_F(macros, transformer_referring_to_name_produced_by_another_transformer) {
  auto result = eval(R"(
    (let-syntax ((jabberwocky
                   (lambda (stx)
                     (let ((name (cadr (syntax->list stx))))
                       #`(begin
                           (define march-hare 42)
                           (define-syntax #,name
                             (lambda (stx)
                               #'march-hare)))))))
      (jabberwocky mad-hatter)
      (mad-hatter))
  )");
  EXPECT_EQ(expect<integer>(result).value(), 42);
}

TEST_F(macros, transformers_preserve_shared_structure) {
  auto result = expect<pair>(eval(R"(
    (let-syntax ((identity
                    (lambda (stx)
                      (car (cdr (syntax-expression stx))))))
      (identity '(#0=(1 2) #0#)))
  )"));
  EXPECT_EQ(car(result), cadr(result));
}
