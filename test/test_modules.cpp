#include "scheme_fixture.hpp"

#include "converters.hpp"
#include "source_code_provider.hpp"

#include <memory>

using namespace insider;

struct modules : scheme_fixture { };

TEST_F(modules, module_activation) {
  std::vector<int> trace;
  define_procedure<void(int)>(
    ctx, "leave-mark", ctx.internal_module, true,
    [&] (int value) { trace.push_back(value); }
  );

  add_library(R"(
    (library (foo))
    (import (insider internal))

    (leave-mark 1)
  )");
  EXPECT_TRUE(trace.empty());

  add_library(R"(
    (library (bar))
    (import (insider internal))
    (import (foo))

    (leave-mark 2)
  )");
  EXPECT_TRUE(trace.empty());

  eval_module(R"(
    (import (insider internal))
    (import (bar))

    (leave-mark 3)
  )");
  EXPECT_EQ(trace, (std::vector{1, 2, 3}));
}

TEST_F(modules, module_variable_export) {
  add_library(R"(
    (library (foo))
    (import (insider internal))
    (export foo)
    (export exported)

    (define foo
      (lambda (x)
        (* 2 x)))

    (define exported 2)
    (define not-exported 3)
  )");

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
  add_library(R"(
    (library (foo))
    (import (insider internal))
    (export double)

    (define-syntax double
      (lambda (stx)
        (let ((value (cadr (syntax->list stx))))
          #`(* 2 #,value))))
  )");

  auto result1 = eval_module(R"(
    (import (foo))
    (import (insider internal))

    (double 3)
  )");
  EXPECT_EQ(expect<integer>(result1).value(), 6);

  add_library(R"(
    (library (bar))
    (import (insider internal))
    (export get-var)

    (define var 7)
    (define-syntax get-var
      (lambda (stx)
        #'var))
  )");
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
  add_library(R"(
    (library (foo))
    (import (insider internal))
    (export a b c d e)
    (define a 1)
    (define b 2)
    (define c 3)
    (define d 4)
    (define e 5)
  )");

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
  auto provider = std::make_unique<virtual_filesystem_source_code_provider>();
  provider->add(
    "foo.scm",
    R"(
      (library (foo))
      (import (insider internal))
      (export value)
      (define value 4)
    )"
  );
  ctx.append_source_code_provider(std::move(provider));

  auto result = eval_module(R"(
    (import (foo))
    value
  )");
  EXPECT_EQ(expect<integer>(result).value(), 4);
}

TEST_F(modules, find_define_library_style_module) {
  auto provider = std::make_unique<virtual_filesystem_source_code_provider>();
  provider->add(
    "foo.scm",
    R"(
      (define-library (foo)
        (import (insider internal))
        (export value)
        (begin
          (define value 4)))
    )"
  );
  ctx.append_source_code_provider(std::move(provider));

  auto result = eval_module(R"(
    (import (foo))
    value
  )");
  EXPECT_EQ(expect<integer>(result).value(), 4);
}
