#include "scheme_fixture.hpp"

#include "define_procedure.hpp"

#include <memory>

using namespace insider;

struct modules : scheme_fixture { };

TEST_F(modules, module_activation) {
  std::vector<int> trace;
  define_procedure<void(int)>(
    ctx, "leave-mark", ctx.internal_module, true,
    [&] (int value) { trace.push_back(value); }
  );

  add_source_file(
    "foo.scm",
    R"(
      (library (foo))
      (import (insider internal))

      (leave-mark 1)
    )"
  );
  EXPECT_TRUE(trace.empty());

  add_source_file(
    "bar.scm",
    R"(
      (library (bar))
      (import (insider internal))
      (import (foo))

      (leave-mark 2)
    )"
  );
  EXPECT_TRUE(trace.empty());

  eval_module(R"(
    (import (insider internal))
    (import (bar))

    (leave-mark 3)
  )");
  EXPECT_EQ(trace, (std::vector{1, 2, 3}));
}

TEST_F(modules, module_variable_export) {
  add_source_file(
    "foo.scm",
    R"(
      (library (foo))
      (import (insider internal))
      (export foo)
      (export exported)

      (define foo
        (lambda (x)
          (* 2 x)))

      (define exported 2)
      (define not-exported 3)
    )"
  );

  auto result = eval_module(R"(
    (import (foo))
    (foo 3)
  )");
  EXPECT_EQ(expect<integer>(result).value(), 6);

  EXPECT_EQ(expect<integer>(eval_module("(import (foo)) exported")).value(), 2);
  EXPECT_THROW(eval_module("(import (foo)) not-exported"), std::runtime_error);
  EXPECT_THROW(eval_module("(import (only (foo) not-exported)) 0"), std::runtime_error);
  EXPECT_THROW(eval_module("(import (except (foo) not-exported)) 0"), std::runtime_error);
}

TEST_F(modules, module_syntax_export) {
  add_source_file(
    "foo.scm",
    R"(
      (library (foo))
      (import (insider internal))
      (export double)

      (define-syntax double
        (lambda (stx)
          (let ((value (cadr (syntax->list stx))))
            #`(* 2 #,value))))
    )"
  );

  auto result1 = eval_module(R"(
    (import (foo))
    (import (insider internal))

    (double 3)
  )");
  EXPECT_EQ(expect<integer>(result1).value(), 6);

  add_source_file(
    "bar.scm",
    R"(
      (library (bar))
      (import (insider internal))
      (export get-var)

      (define var 7)
      (define-syntax get-var
        (lambda (stx)
          #'var))
    )"
  );
  auto result2 = eval_module(R"(
    (import (bar))
    (import (insider internal))

    (define var 3)
    (get-var)
  )");
  EXPECT_EQ(expect<integer>(result2).value(), 7);

  auto result3 = eval_module(R"(
    (import (bar))
    (get-var)
  )");
  EXPECT_EQ(expect<integer>(result3).value(), 7);
}

TEST_F(modules, import_specifiers) {
  add_source_file(
    "foo.scm",
    R"(
      (library (foo))
      (import (insider internal))
      (export a b c d e)
      (define a 1)
      (define b 2)
      (define c 3)
      (define d 4)
      (define e 5)
    )"
  );

  auto result1 = eval_module(R"(
    (import (insider internal)
            (only (foo) a b))
    (+ a b)
  )");
  EXPECT_EQ(expect<integer>(result1).value(), 1 + 2);

  EXPECT_THROW(eval_module("(import (insider internal) (only (foo) a b)) (+ a b c)"),
               std::runtime_error);

  auto result2 = eval_module(R"(
    (import (insider internal)
            (except (foo) a b))
    (+ c d e)
  )");
  EXPECT_EQ(expect<integer>(result2).value(), 3 + 4 + 5);

  EXPECT_THROW(eval_module("(import (insider internal) (except (foo) a b)) (+ a b c d e)"),
               std::runtime_error);

  auto result3 = eval_module(R"(
    (import (insider internal)
            (prefix (foo) foo:))
    (+ foo:a foo:b foo:c foo:d foo:e)
  )");
  EXPECT_EQ(expect<integer>(result3).value(), 1 + 2 + 3 + 4 + 5);

  auto result4 = eval_module(R"(
    (import (insider internal)
            (rename (foo)
                    (a first)
                    (b second)
                    (c third)
                    (d fourth)
                    (e fifth)))
    (+ first second third fourth fifth)
  )");
  EXPECT_EQ(expect<integer>(result4).value(), 1 + 2 + 3 + 4 + 5);

  auto result5 = eval_module(R"(
    (import (insider internal)
            (prefix (only (foo) a b) foo:))
    (+ foo:a foo:b)
  )");
  EXPECT_EQ(expect<integer>(result5).value(), 1 + 2);
}

TEST_F(modules, begin_for_syntax) {
  auto result1 = eval_module(R"(
    (import (insider internal))
    (begin-for-syntax
      (define x 21))
    (* x 2)
  )");
  EXPECT_EQ(expect<integer>(result1).value(), 42);

  auto result2 = eval_module(R"(
    (import (insider internal))

    (begin-for-syntax
      (define big?
        (lambda (x)
          (> x 10))))

    (define-syntax is-big?
      (lambda (stx)
        (if (big? (syntax->datum (cadr (syntax->list stx))))
            #''yes
            #''no)))

    (is-big? 12)
  )");
  EXPECT_EQ(expect<symbol>(result2)->value(), "yes");
}

TEST_F(modules, find_module_file) {
  add_source_file(
    "foo.scm",
    R"(
      (library (foo))
      (import (insider internal))
      (export value)
      (define value 4)
    )"
  );

  auto result = eval_module(R"(
    (import (foo))
    value
  )");
  EXPECT_EQ(expect<integer>(result).value(), 4);
}

TEST_F(modules, find_define_library_style_module) {
  add_source_file(
    "foo.scm",
    R"(
      (define-library (foo)
        (import (insider internal))
        (export value)
        (begin
          (define value 4)))
    )"
  );

  auto result = eval_module(R"(
    (import (foo))
    value
  )");
  EXPECT_EQ(expect<integer>(result).value(), 4);
}

TEST_F(modules, include_in_define_library) {
  add_source_file(
    "foo.sld",
    R"(
      (define-library (foo)
        (import (insider internal))
        (export value)
        (include "foo.scm"))
    )"
  );
  add_source_file("foo.scm",
                  "(define value 4)");

  auto result = eval_module(R"(
    (import (foo))
    value
  )");
  EXPECT_EQ(expect<integer>(result).value(), 4);
}

TEST_F(modules, include_multiple_files_in_one_directive_in_define_library) {
  add_source_file(
    "foo.sld",
    R"(
      (define-library (foo)
        (import (insider internal))
        (export one two)
        (include "one.scm" "two.scm"))
    )"
  );
  add_source_file("one.scm", "(define one 1)");
  add_source_file("two.scm", "(define two 2)");

  auto result = eval_module(R"(
    (import (insider internal) (foo))
    (+ one two)
  )");
  EXPECT_EQ(expect<integer>(result).value(), 3);
}

TEST_F(modules, mix_begin_and_include_in_define_library) {
  add_source_file(
    "foo.sld",
    R"(
      (define-library (foo)
        (import (insider internal))
        (export result)
        (begin
          (define result '(begin)))
        (include "include.scm"))
    )"
  );
  add_source_file("include.scm",
                  "(set! result (cons 'include result))");

  auto result = eval_module(R"(
    (import (insider internal) (foo))
    result
  )");
  EXPECT_TRUE(equal(ctx, result, read("(include begin)")));
}

TEST_F(modules, include_ci_in_define_library) {
  add_source_file(
    "foo.sld",
    R"(
      (define-library (foo)
        (import (insider internal))
        (export value)
        (include-ci "foo.scm"))
    )"
  );
  add_source_file("foo.scm",
                  "(DEFINE VALUE 4)");

  auto result = eval_module(R"(
    (import (foo))
    value
  )");
  EXPECT_EQ(expect<integer>(result).value(), 4);
}

TEST_F(modules, include_library_declarations) {
  add_source_file(
    "foo.sld",
    R"(
      (define-library (foo)
        (import (insider internal))
        (include-library-declarations "exports.scm")
        (begin
          (define value 4)))
    )"
  );
  add_source_file("exports.scm",
                  "(export value)");

  auto result = eval_module(R"(
    (import (foo))
    value
  )");
  EXPECT_EQ(expect<integer>(result).value(), 4);
}

TEST_F(modules, cond_expand_in_define_library_basic) {
  ctx.add_feature("one");
  add_source_file(
    "foo.sld",
    R"(
      (define-library (foo)
        (import (insider internal))
        (export value)
        (cond-expand
          (one (include "one.scm"))
          (two (include "two.scm"))))
    )"
  );

  add_source_file("one.scm", "(define value 4)");

  auto result = eval_module(R"(
    (import (foo))
    value
  )");
  EXPECT_EQ(expect<integer>(result).value(), 4);
}

TEST_F(modules, cond_expand_in_define_library_else) {
  add_source_file(
    "foo.sld",
    R"(
      (define-library (foo)
        (import (insider internal))
        (export value)
        (cond-expand
          (one (include "one.scm"))
          (else (include "two.scm"))))
    )"
  );

  add_source_file("two.scm", "(define value 6)");

  auto result = eval_module(R"(
    (import (foo))
    value
  )");
  EXPECT_EQ(expect<integer>(result).value(), 6);
}

TEST_F(modules, cond_expand_in_define_library_test_for_library) {
  add_source_file(
    "foo.sld",
    R"(
      (define-library (foo)
        (import (insider internal))
        (export first second)
        (cond-expand
          ((library (bar))
           (begin
             (define first #t)))
          (else
           (begin
             (define second #f))))
        (cond-expand
          ((library (baz))
           (begin
             (define second #t)))
          (else
           (begin
             (define second #f)))))
    )"
  );
  add_source_file(
    "bar.sld",
    R"(
      (define-library (bar))
    )"
  );

  auto result = eval_module(R"(
    (import (insider internal) (foo))
    (cons first second)
  )");
  EXPECT_TRUE(equal(ctx, result, read("(#t . #f)")));
}

TEST_F(modules, cond_expand_in_define_library_boolean_conditionals) {
  ctx.add_feature("one");
  add_source_file(
    "foo.sld",
    R"(
      (define-library (foo)
        (import (insider internal))
        (export first second third)

        (cond-expand
          ((not one)
           (begin (define first #f)))
          (else
           (begin (define first #t))))

        (cond-expand
          ((or one two)
           (begin (define second #t)))
          (else
           (begin (define second #f))))

        (cond-expand
          ((and one two)
           (begin (define third #f)))
          (else
           (begin (define third #t)))))
    )"
  );

  auto result = eval_module(R"(
    (import (insider internal) (foo))
    ((lambda l l) first second third)
  )");
  EXPECT_TRUE(equal(ctx, result, read("(#t #t #t)")));
}

TEST_F(modules, empty_module_body) {
  add_source_file(
    "reexporter.scm",
    R"(
      (library (reexporter))
      (import (insider internal))
      (export define lambda *)
    )"
  );

  auto result = eval_module(R"(
    (import (reexporter))
    (define f
      (lambda (x)
        (* 2 x)))
    (f 4)
  )");
  EXPECT_EQ(expect<integer>(result).value(), 8);
}
