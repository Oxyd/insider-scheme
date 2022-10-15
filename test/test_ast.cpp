#include "scheme_fixture.hpp"

#include "compiler/analyser.hpp"
#include "compiler/ast.hpp"
#include "compiler/ast_transforms.hpp"
#include "compiler/expression.hpp"
#include "compiler/parser_expander.hpp"
#include "compiler/parsing_context.hpp"

using namespace insider;

static variable
find_variable(std::string const& name, expression);

static variable
find_variable_in_subexpressions(std::string const& name, auto expr) {
  std::vector<expression> subexprs;
  expr->visit_subexpressions([&] (expression e) { subexprs.push_back(e); });

  for (expression e : subexprs)
    if (variable v = find_variable(name, e))
      return v;
  return {};
}

static variable
find_variable(std::string const& name, ptr<local_reference_expression> ref) {
  if (ref->variable()->name() == name)
    return ref->variable();
  else
    return {};
}

static variable
find_variable(std::string const& name, ptr<top_level_reference_expression> ref) {
  if (ref->variable()->name() == name)
    return ref->variable();
  else
    return {};
}

static variable
find_variable(std::string const& name, ptr<let_expression> let) {
  for (auto const& dp : let->definitions())
    if (dp.variable()->name() == name)
      return dp.variable();

  return find_variable_in_subexpressions(name, let);
}

static variable
find_variable(std::string const& name, ptr<local_set_expression> set) {
  if (set->target()->name() == name)
    return set->target();
  else
    return find_variable_in_subexpressions(name, set);
}

static variable
find_variable(std::string const& name, ptr<top_level_set_expression> set) {
  if (set->target()->name() == name)
    return set->target();
  else
    return find_variable_in_subexpressions(name, set);
}

static variable
find_variable(std::string const& name, ptr<lambda_expression> lambda) {
  for (ptr<local_variable> v : lambda->parameters())
    if (v->name() == name)
      return v;

  return find_variable_in_subexpressions(name, lambda);
}

static variable
find_variable(std::string const& name, auto expr) {
  return find_variable_in_subexpressions(name, expr);
}

static variable
find_variable(std::string const& name, expression e) {
  return visit([&] (auto expr) { return find_variable(name, expr); },
               e);
}

struct ast : scheme_fixture {
  expression
  make_nested_call() {
    return make<application_expression>(
      ctx,
      make<local_reference_expression>(ctx, make<local_variable>(ctx, "foo")),
      make<application_expression>(
        ctx,
        make<local_reference_expression>(ctx, make<local_variable>(ctx, "bar")),
        make<literal_expression>(ctx, ctx.intern("baz"))
      )
    );
  }

  expression
  analyse(ptr<syntax> expr_stx, pass_list passes = all_passes) {
    auto m = make_tracked<module_>(ctx, ctx);
    import_all_exported(ctx, m, ctx.internal_module_tracked());

    insider::null_source_code_provider provider;
    return insider::analyse(ctx, expr_stx, m, std::move(passes),
                            {&provider, "<unit test expression>"});
  }

  expression
  analyse(std::string const& expr, pass_list passes = all_passes) {
    auto expr_stx = read_syntax(ctx, expr);
    if (expr_stx == ctx.constants->eof)
      throw std::runtime_error{"EOF"};
    else
      return analyse(assume<syntax>(expr_stx), std::move(passes));
  }

  expression
  analyse_module(std::string const& expr, pass_list passes = all_passes) {
    null_source_code_provider provider;
    module_specifier pm = read_module(ctx, read_syntax_multiple(ctx, expr),
                                      {&provider, "<unit test main module>"});
    auto mod = make_tracked<module_>(ctx, ctx, pm.name);
    perform_imports(ctx, mod, pm.imports);
    return insider::analyse_module(ctx, mod, pm, std::move(passes), true);
  }

  variable
  parse_and_get_variable(std::string const& variable_name,
                         std::string const& expr) {
    expression e = analyse(expr, {&analyse_variables});
    variable v = find_variable(variable_name, e);
    assert(v);
    return v;
  }

  ptr<local_variable>
  parse_and_get_local_variable(std::string const& variable_name,
                               std::string const& expr) {
    return assume<local_variable>(parse_and_get_variable(variable_name, expr));
  }

  variable
  parse_module_and_get_variable(std::string const& variable_name,
                                std::string const& expr) {
    expression e = analyse_module(expr, {&analyse_variables});
    variable v = find_variable(variable_name, e);
    assert(v);
    return v;
  }

  ptr<top_level_variable>
  parse_module_and_get_top_level_variable(std::string const& variable_name,
                                          std::string const& expr) {
    return assume<top_level_variable>(
      parse_module_and_get_variable(variable_name, expr)
    );
  }
};

struct counting_visitor {
  unsigned seen_applications = 0;
  unsigned seen_local_references = 0;
  unsigned seen_literals = 0;

  expression
  operator () (ptr<application_expression> e) {
    ++seen_applications;
    return e;
  }

  expression
  operator () (ptr<local_reference_expression> e) {
    ++seen_local_references;
    return e;
  }

  expression
  operator () (ptr<literal_expression> e) {
    ++seen_literals;
    return e;
  }

  expression
  operator () (auto e) {
    assert(false);
    return e;
  }
};

TEST_F(ast, map_ast_visits_all_nodes) {
  counting_visitor v;
  map_ast(ctx, make_nested_call(), v);
  EXPECT_EQ(v.seen_applications, 2u);
  EXPECT_EQ(v.seen_local_references, 2u);
  EXPECT_EQ(v.seen_literals, 1u);
}

struct wrap_local_reference_in_identity {
  context& ctx;

  explicit
  wrap_local_reference_in_identity(context& ctx)
    : ctx{ctx}
  { }

  expression
  operator () (ptr<local_reference_expression> ref) {
    return make<application_expression>(
      ctx,
      make<local_reference_expression>(ctx,
                                       make<local_variable>(ctx, "identity")),
      ref
    );
  };

  expression
  operator () (auto other) { return other; }
};

TEST_F(ast, map_ast_to_change_its_shape) {
  expression e = make_nested_call();
  EXPECT_TRUE(
    is<local_reference_expression>(expect<application_expression>(e)->target())
  );

  expression f = map_ast(ctx, e, wrap_local_reference_in_identity{ctx});
  EXPECT_FALSE(
    is<local_reference_expression>(expect<application_expression>(f)->target())
  );
  EXPECT_TRUE(
    is<application_expression>(expect<application_expression>(f)->target())
  );
}

static ptr<>
constant_value(auto var) {
  return expect<literal_expression>(var->constant_initialiser())->value();
}

TEST_F(ast, analyse_variables_recognises_constants) {
  ptr<local_variable> var = parse_and_get_local_variable("const", R"(
    (let ((const 2))
      (+ const 4))
  )");
  EXPECT_EQ(expect<integer>(constant_value(var)).value(), 2);
}

TEST_F(ast, set_variable_is_not_marked_as_constant) {
  ptr<local_variable> var = parse_and_get_local_variable("v", R"(
    (let ((v 2))
      v  ; Force non-eliminable
      (set! v 5)
      (+ v 4))
  )");
  EXPECT_TRUE(var->flags().is_set);
  EXPECT_FALSE(var->constant_initialiser());
}

TEST_F(ast, variable_with_non_constant_initialiser_is_not_constant) {
  ptr<local_variable> var = parse_and_get_local_variable("v", R"(
    (let ((v (read)))
      (+ v 2))
  )");
  EXPECT_FALSE(var->flags().is_set);
  EXPECT_FALSE(var->constant_initialiser());
}

TEST_F(ast, top_level_variable_is_recognised_as_constant) {
  ptr<top_level_variable> var = parse_module_and_get_top_level_variable(
    "top-level",
    R"(
      (import (insider internal))
      (define top-level 12)
      (define other 17)
    )"
  );
  EXPECT_FALSE(var->flags().is_set);
  EXPECT_EQ(expect<integer>(constant_value(var)).value(), 12);
}

TEST_F(ast, top_level_variable_is_not_constant_if_it_is_set) {
  ptr<top_level_variable> var = parse_module_and_get_top_level_variable(
    "top-level",
    R"(
      (import (insider internal))
      (define top-level 12)
      (define foo
        (lambda ()
          (set! top-level 24)))
    )"
  );
  EXPECT_TRUE(var->flags().is_set);
  EXPECT_FALSE(var->constant_initialiser());
}

TEST_F(ast, top_level_variable_is_not_constant_if_it_is_set_before_definition) {
  ptr<top_level_variable> var = parse_module_and_get_top_level_variable(
    "top-level",
    R"(
      (import (insider internal))
      (define foo
        (lambda ()
          (set! top-level 24)))
      (define top-level 12)
    )"
  );
  EXPECT_TRUE(var->flags().is_set);
  EXPECT_FALSE(var->constant_initialiser());
}

TEST_F(ast, meta_definitions_are_not_constants) {
  ptr<top_level_variable> var = parse_module_and_get_top_level_variable(
    "meta-var",
    R"(
      (import (insider internal))
      (meta (define meta-var 12))
      meta-var
    )"
  );
  EXPECT_FALSE(var->constant_initialiser());
}

TEST_F(ast, repl_definitions_are_not_constants) {
  tracked_ptr<module_> m
    = make_interactive_module(
        ctx,
        import_modules(module_name{"insider", "internal"})
      );
  null_source_code_provider provider;
  expression e
    = insider::analyse(ctx,
                       assume<syntax>(read_syntax(ctx, "(define foo 12)")),
                       m, {&analyse_variables},
                       {&provider, "<unit test main module>"});
  ptr<top_level_variable> v
    = expect<top_level_variable>(find_variable("foo", e));
  EXPECT_FALSE(v->constant_initialiser());
}

TEST_F(ast, top_level_lambda_definitions_are_recognised_as_constants) {
  ptr<top_level_variable> var = parse_module_and_get_top_level_variable(
    "foo",
    R"(
      (import (insider internal))
      (define foo (lambda () 0))
    )"
  );
  ASSERT_TRUE(var->constant_initialiser());
  EXPECT_TRUE(is<lambda_expression>(var->constant_initialiser()));
}

TEST_F(ast, local_lambda_definitions_are_recognised_as_constants) {
  ptr<local_variable> var = parse_and_get_local_variable(
    "foo",
    R"(
      (let ((foo (lambda () 0)))
        1)
    )"
  );
  ASSERT_TRUE(var->constant_initialiser());
  EXPECT_TRUE(is<lambda_expression>(var->constant_initialiser()));
}

template <typename T>
static void
for_each(expression e, auto&& f) {
  if (auto x = match<T>(e))
    f(x);

  visit(
    [&] (auto expr) {
      expr->visit_subexpressions([&] (auto subexpr) {
        for_each<T>(subexpr, f);
      });
    },
    e
  );
}

TEST_F(ast, constants_are_folded_into_expressions) {
  expression e = analyse(R"(
    (let ((const 2))
      (write const))
  )");
  for_each<application_expression>(
    e,
    [] (ptr<application_expression> app) {
      for (expression arg : app->arguments())
        EXPECT_TRUE(is<literal_expression>(arg));
    }
  );
}

TEST_F(ast, constant_only_lets_are_removed) {
  expression e = analyse(R"(
    (let ((const-1 2) (const-2 5))
      (+ const-1 const-2))
  )");
  for_each<let_expression>(
    e,
    [] (ptr<let_expression>) {
      FAIL();
    }
  );
}

TEST_F(ast, singleton_constants_are_propagated_correctly) {
  expression e = analyse(R"(
    (let ((const #t))
      const)
  )");
  expression subexpr = expect<sequence_expression>(e)->expressions()[0];
  EXPECT_EQ(expect<literal_expression>(subexpr)->value(), ctx.constants->t);
}

TEST_F(ast, constant_definitions_are_removed_from_lets) {
  expression e = analyse(R"(
    (let ((const 2) (non-const (read)))
      (+ const non-const))
  )");
  for_each<let_expression>(
    e,
    [] (ptr<let_expression> let) {
      ASSERT_EQ(let->definitions().size(), 1);
      EXPECT_EQ(let->definitions().front().variable()->name(), "non-const");
    }
  );
}

TEST_F(ast, top_level_constants_are_folded_into_expressions) {
  expression e = analyse_module(
    R"(
      (import (insider internal))
      (define foo 12)
      (define bar (* (read) foo))
    )",
    {&analyse_variables, &propagate_and_evaluate_constants}
  );
  for_each<top_level_set_expression>(
    e,
    [] (ptr<top_level_set_expression> set) {
      if (set->target()->name() == "bar") {
        auto app = assume<application_expression>(set->expression());
        ASSERT_EQ(app->arguments().size(), 2);
        EXPECT_TRUE(is<literal_expression>(app->arguments()[1]));
      }
    }
  );
}

TEST_F(ast, top_level_constants_are_folded_into_expressions_before_definition) {
  expression e = analyse_module(
    R"(
      (import (insider internal))
      (define bar (* (read) foo))
      (define foo 12)
    )",
    {&analyse_variables, &propagate_and_evaluate_constants}
  );
  for_each<top_level_set_expression>(
    e,
    [] (ptr<top_level_set_expression> set) {
      if (set->target()->name() == "bar") {
        auto app = assume<application_expression>(set->expression());
        ASSERT_EQ(app->arguments().size(), 2);
        EXPECT_TRUE(is<literal_expression>(app->arguments()[1]));
      }
    }
  );
}

TEST_F(ast, top_level_constants_are_propagated_across_modules) {
  add_source_file(
    "foo.scm",
    R"(
      (library (foo))
      (import (insider internal))
      (export top-level)
      (define top-level #t)
    )"
  );

  expression e = analyse_module(R"(
    (import (insider internal) (foo))
    (* top-level 2)
  )");
  for_each<application_expression>(
    e,
    [&] (ptr<application_expression> app) {
      ASSERT_EQ(app->arguments().size(), 2);
      auto arg = expect<literal_expression>(app->arguments()[0]);
      EXPECT_EQ(arg->value(), ctx.constants->t);
    }
  );
}


static std::string
target_name(ptr<top_level_reference_expression> ref) {
  return ref->variable()->name();
}

static std::string
target_name(ptr<local_reference_expression> ref) {
  return ref->variable()->name();
}

static std::string
target_name(auto) {
  assert(false);
  return {};
}

static void
assert_procedure_is_called(expression e, std::string const& name) {
  bool called = false;

  for_each<application_expression>(
    e,
    [&] (ptr<application_expression> app) {
      std::string target = visit([] (auto e) { return target_name(e); },
                                 app->target());
      if (target == name)
        called = true;
    }
  );

  EXPECT_TRUE(called);
}

static void
assert_procedure_not_called(expression e, std::string const& name) {
  for_each<application_expression>(
    e,
    [&] (ptr<application_expression> app) {
      std::string target = visit([] (auto e) { return target_name(e); },
                                 app->target());
      EXPECT_NE(target, name);
    }
  );
}

TEST_F(ast, top_level_procedure_calls_are_inlined) {
  expression e = analyse_module(
    R"(
      (import (insider internal))

      (define foo
        (lambda (x)
          (* 2 x)))

      (let ((y (read)))
        (foo y))
    )",
    {&analyse_variables, &inline_procedures}
  );
  assert_procedure_not_called(e, "foo");
}

TEST_F(ast, local_procedure_calls_are_inlined) {
  expression e = analyse(
    R"(
      (let ((f (lambda (x) (* 2 x))))
        (f 5))
    )",
    {&analyse_variables, &inline_procedures}
  );
  assert_procedure_not_called(e, "f");
}

TEST_F(ast, procedures_are_inlined_across_modules) {
  add_source_file(
    "foo.scm",
    R"(
      (library (foo))
      (import (insider internal))
      (export foo)
      (define foo
        (lambda (x)
          (* 2 x)))
    )"
  );
  expression e = analyse_module(
    R"(
      (import (foo))
      (foo 12)
    )",
    {&analyse_variables, &inline_procedures}
  );
  assert_procedure_not_called(e, "foo");
}

static expression
find_top_level_definition_for(expression root, std::string const& name) {
  expression result;

  for_each<top_level_set_expression>(
    root,
    [&] (ptr<top_level_set_expression> set) {
      if (set->target()->name() == name)
        result = set->expression();
    }
  );

  return result;
}

static expression
first_subexpression(ptr<let_expression> let) {
  if (auto seq = match<sequence_expression>(let->body()))
    return seq->expressions().front();
  else
    return let->body();
}

static expression
first_subexpression(ptr<lambda_expression> lambda) {
  if (auto seq = match<sequence_expression>(lambda->body()))
    return seq->expressions().front();
  else
    return lambda->body();
}

TEST_F(ast, recursive_procedures_are_not_inlined) {
  expression e = analyse_module(
    R"(
      (import (insider internal))
      (define foo
        (lambda ()
          (foo)))
    )",
    {&analyse_variables, &inline_procedures, &analyse_free_variables}
  );

  auto foo_var = find_variable("foo", e);
  auto foo_def
    = expect<lambda_expression>(find_top_level_definition_for(e, "foo"));
  auto app
    = expect<application_expression>(first_subexpression(foo_def));
  auto target = expect<top_level_reference_expression>(app->target());
  EXPECT_EQ(target->variable(), foo_var);
}

TEST_F(ast, outer_lambda_is_not_inlined_into_inner_lambda) {
  expression e = analyse_module(
    R"(
      (import (insider internal))

      (define foo
        (lambda ()
          (lambda ()
            (foo))))
    )",
    {&analyse_variables, &inline_procedures, &analyse_free_variables}
  );

  auto foo_def
    = expect<lambda_expression>(find_top_level_definition_for(e, "foo"));
  auto inner_lambda
    = expect<lambda_expression>(first_subexpression(foo_def));
  EXPECT_TRUE(
    is<application_expression>(first_subexpression(inner_lambda))
  );
}

static std::string const inlining_module = R"(
  (import (insider internal))

  (define foo
    (lambda (x)
      (+ x 2)))

  (define bar
    (lambda ()
      (foo 4)))
)";

TEST_F(ast, inlined_code_does_not_share_ast) {
  expression e = analyse_module(
    inlining_module,
    {&analyse_variables, &inline_procedures, &analyse_free_variables}
  );

  auto foo_def
    = expect<lambda_expression>(find_top_level_definition_for(e, "foo"));
  auto foo_body = first_subexpression(foo_def);

  auto bar_def
    = expect<lambda_expression>(find_top_level_definition_for(e, "bar"));
  auto bar_let = expect<let_expression>(first_subexpression(bar_def));
  auto bar_body = first_subexpression(bar_let);

  EXPECT_NE(foo_body, bar_body);
}

TEST_F(ast, inlined_code_does_not_share_lambda_variables) {
  expression e = analyse_module(
    inlining_module,
    {&analyse_variables, &inline_procedures, &analyse_free_variables}
  );

  auto foo_def
    = expect<lambda_expression>(find_top_level_definition_for(e, "foo"));
  auto foo_param = foo_def->parameters().front();

  auto bar_def
    = expect<lambda_expression>(find_top_level_definition_for(e, "bar"));
  auto bar_let = expect<let_expression>(first_subexpression(bar_def));
  auto bar_var = bar_let->definitions().front().variable();

  EXPECT_NE(foo_param, bar_var);
}

TEST_F(ast, inlined_code_does_not_share_internal_variables) {
  expression e = analyse_module(
    R"(
      (import (insider internal))

      (define foo
        (lambda ()
          (let ((x 2))
            x)))

      (define bar
        (lambda ()
          (foo)))
    )",
    {&analyse_variables, &inline_procedures, &analyse_free_variables}
  );

  auto foo_def
    = expect<lambda_expression>(find_top_level_definition_for(e, "foo"));
  auto foo_let = expect<let_expression>(first_subexpression(foo_def));
  auto foo_var = foo_let->definitions().front().variable();

  auto bar_def
    = expect<lambda_expression>(find_top_level_definition_for(e, "bar"));
  auto bar_outer_let
    = expect<let_expression>(first_subexpression(bar_def));
  auto bar_inner_let
    = expect<let_expression>(first_subexpression(bar_outer_let));
  auto bar_var = bar_inner_let->definitions().front().variable();

  EXPECT_NE(foo_var, bar_var);
}

TEST_F(ast, invalid_call_is_not_inlined) {
  expression e = analyse_module(
    R"(
      (import (insider internal))

      (define unary
        (lambda (x)
          x))

      (define wrong
        (lambda ()
          (unary 1 2 3)))
    )",
    {&analyse_variables, &inline_procedures, &analyse_free_variables}
  );

  assert_procedure_is_called(e, "unary");
}

TEST_F(ast, variadic_procedures_are_inlined) {
  expression e = analyse_module(
    R"(
      (import (insider internal))

      (define foo
        (lambda (x . rest)
          #f))

      (define bar
        (lambda ()
          (foo 1 2 3)))
    )",
    {&analyse_variables, &inline_procedures, &analyse_free_variables}
  );

  auto bar_def
    = expect<lambda_expression>(find_top_level_definition_for(e, "bar"));
  assert_procedure_not_called(bar_def, "foo");

  auto let = expect<let_expression>(first_subexpression(bar_def));
  ASSERT_EQ(let->definitions().size(), 2);
  EXPECT_EQ(
    expect<integer>(
      expect<literal_expression>(let->definitions()[0].expression())->value()
    ).value(),
    1
  );

  auto tail_expr
    = expect<application_expression>(let->definitions()[1].expression());
  auto list_ref = expect<top_level_reference_expression>(tail_expr->target());
  EXPECT_EQ(list_ref->variable()->name(), "list");
  ASSERT_EQ(tail_expr->arguments().size(), 2);
  EXPECT_EQ(
    expect<integer>(
      expect<literal_expression>(tail_expr->arguments()[0])->value()
    ).value(),
    2
  );
  EXPECT_EQ(
    expect<integer>(
      expect<literal_expression>(tail_expr->arguments()[1])->value()
    ).value(),
    3
  );
}

TEST_F(ast, call_of_variadic_procedure_with_too_few_args_is_not_inlined) {
  expression e = analyse_module(
    R"(
      (import (insider internal))

      (define foo
        (lambda (one two . rest)
          0))

      (define bar
        (lambda ()
          (foo 1)))
    )",
    {&analyse_variables, &inline_procedures, &analyse_free_variables}
  );

  auto bar_def
    = expect<lambda_expression>(find_top_level_definition_for(e, "bar"));
  assert_procedure_is_called(bar_def, "foo");
}

TEST_F(ast, inlined_call_of_mutative_procedure_across_modules_boxes_argument) {
  add_source_file(
    "foo.scm",
    R"(
      (library (foo))
      (import (insider internal))
      (export foo)

      (define foo
        (lambda (x)
          (set! x (* 2 x))))
    )"
  );

  expression e = analyse_module(
    R"(
      (import (insider internal) (foo))

      (foo 2)
    )",
    {&analyse_variables, &inline_procedures, &box_set_variables}
  );

  auto let = expect<let_expression>(e);
  ASSERT_EQ(let->definitions().size(), 1);
  auto init
    = expect<application_expression>(let->definitions().front().expression());
  EXPECT_EQ(
    expect<top_level_reference_expression>(init->target())->variable()->name(),
    "box"
  );

  auto var = let->definitions().front().variable();
  auto box_set
    = expect<application_expression>(first_subexpression(let));
  auto box_set_ref = expect<local_reference_expression>(box_set->arguments()[0]);
  EXPECT_EQ(box_set_ref->variable(), var);
}

TEST_F(ast, chain_of_top_level_procedures_is_inlined_completely) {
  expression e = analyse_module(
    R"(
      (import (insider internal))

      (define one (lambda () 5))
      (define two (lambda () (one)))
      (define three (lambda () (two)))
    )"
  );

  auto three_def
    = expect<lambda_expression>(find_top_level_definition_for(e, "three"));
  assert_procedure_not_called(three_def, "two");
  assert_procedure_not_called(three_def, "one");
}

TEST_F(ast, variables_are_not_boxed_twice) {
  add_source_file(
    "foo.scm",
    R"(
      (library (foo))
      (import (insider internal))
      (export foo)

      (define foo
        (lambda ()
          (let ((var #void))
            var ; Ensure it is not set!-eliminable
            (set! var 5)
            'hi)))
    )"
  );

  expression e = analyse_module(R"(
    (import (insider internal) (foo))

    (foo)
  )");

  auto let = expect<let_expression>(
    expect<sequence_expression>(e)->expressions().front()
  );
  ASSERT_EQ(let->definitions().size(), 1);
  auto var_def = let->definitions()[0].expression();
  auto var_op = expect<built_in_operation_expression>(var_def);
  EXPECT_EQ(var_op->operation(), opcode::box);

  ASSERT_EQ(var_op->operands().size(), 1);
  EXPECT_TRUE(is<literal_expression>(var_op->operands()[0]));
}

static expression
ignore_lets_and_sequences(expression e) {
  while (true) {
    if (auto let = match<let_expression>(e))
      e = first_subexpression(let);
    else if (auto seq = match<sequence_expression>(e)) {
      assert(seq->expressions().size() == 1);
      e = seq->expressions().front();
    } else
      return e;
  }
}

TEST_F(ast, inlined_applications_carry_debug_info) {
  using namespace std::literals;

  expression e = analyse_module(
    R"(
      (import (insider internal))

      (define one (lambda () (+ 2 3)))
      (define two (lambda () (one)))
      (define three (lambda () (two)))

      (three)
    )",
    {&analyse_variables, &inline_procedures, &analyse_free_variables}
  );

  auto three_def
    = expect<lambda_expression>(find_top_level_definition_for(e, "three"));
  auto three_app = expect<application_expression>(
    ignore_lets_and_sequences(three_def->body())
  );
  EXPECT_EQ(
    expect<top_level_reference_expression>(
      three_app->target()
    )->variable()->name(),
    "+"
  );

  ASSERT_TRUE(three_app->debug_info());
  EXPECT_EQ(three_app->debug_info()->inlined_call_chain,
            (std::vector{"one"s, "two"s}));

  auto top_level_app = expect<application_expression>(
    ignore_lets_and_sequences(
      expect<sequence_expression>(e)->expressions().back()
    )
  );
  EXPECT_EQ(
    expect<top_level_reference_expression>(
      top_level_app->target()
    )->variable()->name(),
    "+"
  );

  ASSERT_TRUE(top_level_app->debug_info());
  EXPECT_EQ(top_level_app->debug_info()->inlined_call_chain,
            (std::vector{"one"s, "two"s, "three"s}));
}

TEST_F(ast, call_with_all_arguments_literal_is_constant_evaluated) {
  expression e = analyse(
    "(car '(1 . 2))",
    {&analyse_variables, &propagate_and_evaluate_constants}
  );
  ASSERT_TRUE(is<literal_expression>(e));
  EXPECT_EQ(expect<integer>(expect<literal_expression>(e)->value()).value(), 1);
}

TEST_F(ast, invalid_constant_evaluable_call_is_not_compilation_error) {
  expression e = analyse(
    R"((car "not a pair"))",
    {&analyse_variables, &propagate_and_evaluate_constants}
  );
  EXPECT_TRUE(is<application_expression>(e));
}

TEST_F(ast, constant_calls_are_evaluated_recursively) {
  expression e = analyse(
    "(+ (car '(1 . 2)) (cdr '(1 . 2)))",
    {&analyse_variables, &propagate_and_evaluate_constants}
  );
  ASSERT_TRUE(is<literal_expression>(e));
  EXPECT_EQ(expect<integer>(expect<literal_expression>(e)->value()).value(), 3);
}

TEST_F(ast, constant_variables_are_used_in_evaluation_of_constant_calls) {
  expression e = analyse(
    R"(
      (let ((k 5))
        (+ 2 k))
    )",
    {&analyse_variables, &propagate_and_evaluate_constants}
  );
  e = ignore_lets_and_sequences(e);
  ASSERT_TRUE(is<literal_expression>(e));
  EXPECT_EQ(expect<integer>(expect<literal_expression>(e)->value()).value(), 7);
}

TEST_F(ast, let_variables_bound_to_constant_expressions_are_constants) {
  expression e = analyse(
    R"(
      (let ((k1 (+ 2 3)))
        (* 2 k1))
    )",
    {&analyse_variables, &propagate_and_evaluate_constants}
  );
  e = ignore_lets_and_sequences(e);
  ASSERT_TRUE(is<literal_expression>(e));
  EXPECT_EQ(expect<integer>(expect<literal_expression>(e)->value()).value(),
            10);
}

TEST_F(ast, let_initialiser_are_constant_if_they_only_use_other_constants) {
  expression e = analyse(
    R"(
      (let ((k1 (+ 2 3)))
        (let ((k2 (* k1 2)))
          (+ k2 1)))
    )",
    {&analyse_variables, &propagate_and_evaluate_constants}
  );
  e = ignore_lets_and_sequences(e);
  ASSERT_TRUE(is<literal_expression>(e));
  EXPECT_EQ(expect<integer>(expect<literal_expression>(e)->value()).value(),
            11);
}

TEST_F(ast, constant_ifs_are_removed) {
  expression e1 = analyse(
    R"(
      (if #f 1 2)
    )",
    {&analyse_variables, &propagate_and_evaluate_constants}
  );
  e1 = ignore_lets_and_sequences(e1);
  ASSERT_TRUE(is<literal_expression>(e1));
  EXPECT_EQ(expect<integer>(expect<literal_expression>(e1)->value()).value(), 2);

  expression e2 = analyse(
    R"(
      (if #t 1 2)
    )",
    {&analyse_variables, &propagate_and_evaluate_constants}
  );
  e2 = ignore_lets_and_sequences(e2);
  ASSERT_TRUE(is<literal_expression>(e2));
  EXPECT_EQ(expect<integer>(expect<literal_expression>(e2)->value()).value(), 1);
}

TEST_F(ast, constant_expressions_in_if_tests_are_evaluated) {
  expression e = analyse(
    R"(
      (let ((k (* 2 3)))
        (if (< k 10) 'yes 'no))
    )",
    {&analyse_variables, &propagate_and_evaluate_constants}
  );
  e = ignore_lets_and_sequences(e);
  ASSERT_TRUE(is<literal_expression>(e));
  EXPECT_EQ(expect<literal_expression>(e)->value(), ctx.intern("yes"));
}

TEST_F(ast, sequence_of_a_single_constant_expression_is_constant_expression) {
  expression e = analyse(
    R"(
      (let ((x (begin 2)))
        x)
    )",
    {&analyse_variables, &propagate_and_evaluate_constants}
  );
  e = ignore_lets_and_sequences(e);
  ASSERT_TRUE(is<literal_expression>(e));
  EXPECT_EQ(expect<integer>(expect<literal_expression>(e)->value()).value(), 2);
}

TEST_F(ast, unused_variable_is_not_read) {
  ptr<local_variable> var = parse_and_get_local_variable("v", R"(
    (let ((v 5))
      #t)
  )");
  EXPECT_FALSE(var->flags().is_read);
}

TEST_F(ast, local_variable_used_in_expression_is_marked_as_read) {
  ptr<local_variable> var = parse_and_get_local_variable("v", R"(
    (let ((v 5))
      (+ v 4))
  )");
  EXPECT_TRUE(var->flags().is_read);
}

TEST_F(ast, top_level_variable_used_in_expression_is_marked_as_read) {
  ptr<top_level_variable> var = parse_module_and_get_top_level_variable(
    "var",
    R"(
      (import (insider internal))
      (define var 4)
      (define foo (* 2 var))
    )"
  );
  EXPECT_TRUE(var->flags().is_read);
}

TEST_F(ast, local_variable_captured_by_lambda_is_marked_as_read) {
  ptr<local_variable> var = parse_and_get_local_variable("v", R"(
    (let ((v 2))
      (lambda () v))
  )");
  EXPECT_TRUE(var->flags().is_read);
}

TEST_F(ast, capture_of_variable_in_its_assignment_does_not_count_as_read) {
  ptr<local_variable> var = parse_and_get_local_variable("f", R"(
    (let ((f #void))
      (set! f (lambda () (f))))
  )");
  EXPECT_FALSE(var->flags().is_read);
}

TEST_F(ast, direct_use_of_variable_in_its_assignment_counts_as_read) {
  ptr<local_variable> var = parse_and_get_local_variable("x", R"(
    (let ((x 2))
      (set! x (* 2 x)))
  )");
  EXPECT_TRUE(var->flags().is_read);
}

TEST_F(ast, variable_that_is_only_set_is_set_eliminable) {
  ptr<local_variable> var = parse_and_get_local_variable("x", R"(
    (let ((x #void))
      (set! x 2))
  )");
  EXPECT_TRUE(var->flags().is_set_eliminable);
}

TEST_F(ast, variable_that_is_set_multiple_times_is_not_eliminable) {
  ptr<local_variable> var = parse_and_get_local_variable("x", R"(
    (let ((x #void))
      (set! x 2)
      (set! x 3))
  )");
  EXPECT_FALSE(var->flags().is_set_eliminable);
}

TEST_F(ast, variable_that_is_read_after_being_set_is_set_eliminable) {
  ptr<local_variable> var = parse_and_get_local_variable("x", R"(
    (let ((x #void))
      (set! x 2)
      x)
  )");
  EXPECT_TRUE(var->flags().is_set_eliminable);
}

TEST_F(ast, variable_that_is_never_set_is_not_set_eliminable) {
  ptr<local_variable> var = parse_and_get_local_variable("x", R"(
    (let ((x #void))
      x)
  )");
  EXPECT_FALSE(var->flags().is_set_eliminable);
}

TEST_F(ast, variable_that_is_set_after_read_is_not_set_eliminable) {
  ptr<local_variable> var = parse_and_get_local_variable("x", R"(
    (let ((x #void))
      (let ((f (lambda () x)))
        (set! x 2)
        f))
  )");
  EXPECT_FALSE(var->flags().is_set_eliminable);
}

TEST_F(
  ast,
  variable_that_is_set_in_lambda_that_refers_to_itself_is_not_set_eliminable
) {
  ptr<local_variable> var = parse_and_get_local_variable("f", R"(
    (let ((f #void))
      (set! f
        (lambda ()
          (set! f (lambda () #f))
          #t))
      (f))
  )");
  EXPECT_FALSE(var->flags().is_set_eliminable);
}

TEST_F(ast, variable_that_is_conditionally_set_is_not_set_eliminable) {
  ptr<local_variable> var = parse_and_get_local_variable("x", R"(
    (let ((x 0))
      (if #f (set! x 1))
      x)
  )");
  EXPECT_FALSE(var->flags().is_set_eliminable);
}

TEST_F(ast, variable_introduced_in_if_branch_can_be_eliminable) {
  ptr<local_variable> var = parse_and_get_local_variable("x", R"(
    (if #f
        (let ((x 0))
          (set! x 1)))
  )");
  EXPECT_TRUE(var->flags().is_set_eliminable);
}

TEST_F(ast, lambda_parameter_can_be_eliminable) {
  ptr<local_variable> var = parse_and_get_local_variable("x", R"(
    (lambda (x)
      (set! x 2)
      x)
  )");
  EXPECT_TRUE(var->flags().is_set_eliminable);
}

TEST_F(ast, variable_set_in_inner_lambda_is_not_eliminable) {
  ptr<local_variable> var = parse_and_get_local_variable("x", R"(
    (let ((x 0) (f #void))
      (set! f
        (lambda ()
          (set! x 1)))
      (if #f (f))
      x)
  )");
  EXPECT_FALSE(var->flags().is_set_eliminable);
}

TEST_F(ast, variable_that_is_set_after_a_lambda_definition_is_eliminable) {
  ptr<local_variable> var = parse_and_get_local_variable("x", R"(
    (let ((x 0) (f #void))
      (set! f (lambda () #f))
      (set! x 2)
      x)
  )");
  EXPECT_TRUE(var->flags().is_set_eliminable);
}

TEST_F(ast, variable_that_is_set_after_an_if_can_be_eliminable) {
  ptr<local_variable> var = parse_and_get_local_variable("x", R"(
    (let ((x 0))
      (if #f 0 1)
      (set! x 2)
      x)
  )");
  EXPECT_TRUE(var->flags().is_set_eliminable);
}

TEST_F(ast, set_eliminable_variable_is_not_boxed) {
  expression e = analyse(
    R"(
      (let ((x 0))
        (set! x 1)
        x)
    )",
    {&analyse_variables, box_set_variables}
  );

  auto let = expect<let_expression>(e);
  auto seq = expect<sequence_expression>(let->body());
  ASSERT_EQ(seq->expressions().size(), 2);
  EXPECT_TRUE(is<local_set_expression>(seq->expressions()[0]));
  EXPECT_TRUE(is<local_reference_expression>(seq->expressions()[1]));
}

TEST_F(ast, self_referential_local_lambda_uses_self_variable) {
  expression e = analyse(
    R"(
      (let ((f #void))
        (set! f
          (lambda ()
            f)))
    )",
    {&analyse_variables, &find_self_variables}
  );
  for_each<lambda_expression>(
    e,
    [] (ptr<lambda_expression> f) {
      EXPECT_EQ(expect<local_reference_expression>(
                  first_subexpression(f)
                )->variable(),
                f->self_variable());
    }
  );
}

TEST_F(ast, self_referential_top_level_procedure_uses_self_variable) {
  expression e = analyse_module(
    R"(
      (import (insider internal))

      (define f
        (lambda ()
          f))
    )",
    {&analyse_variables, &find_self_variables}
  );

  auto f = expect<lambda_expression>(find_top_level_definition_for(e, "f"));
  auto ref
    = expect<local_reference_expression>(ignore_lets_and_sequences(f->body()));
  EXPECT_EQ(ref->variable(), f->self_variable());
}

TEST_F(ast, self_variable_is_bound_within_the_lambda) {
  expression e = analyse(
    R"(
      (let ((f #void))
        (set! f
          (lambda ()
            f)))
    )",
    {&analyse_variables, &find_self_variables, &analyse_free_variables}
  );
  for_each<lambda_expression>(
    e,
    [] (ptr<lambda_expression> f) {
      EXPECT_TRUE(f->free_variables().empty());
    }
  );
}

TEST_F(ast, self_variable_in_inlined_procedure_is_consistent) {
  add_source_file(
    "foo.scm",
    R"(
      (library (foo))
      (import (insider internal))
      (export foo)

      (define foo
        (lambda ()
          (let ((f #void))
            (set! f (lambda () f)))))
    )"
  );

  expression e = analyse_module(
    R"(
      (import (insider internal) (foo))
      (foo)
    )"
  );
  for_each<lambda_expression>(
    e,
    [] (ptr<lambda_expression> f) {
      EXPECT_EQ(
        expect<local_reference_expression>(
          first_subexpression(f)
        )->variable(),
        f->self_variable()
      );
    }
  );
}

TEST_F(ast, simple_loop_lambda_has_one_self_reference) {
  expression e = analyse(
    R"(
      (let ((f #void))
        (set! f
          (lambda ()
            (f))))
    )",
    {&analyse_variables, &find_self_variables}
  );
  for_each<lambda_expression>(
    e,
    [] (ptr<lambda_expression> lambda) {
      EXPECT_EQ(lambda->num_self_references(), 1);
    }
  );
}

TEST_F(ast, references_in_inner_lambdas_count_as_self_references) {
  expression e = analyse(
    R"(
      (let ((f #void))
        (set! f
          (lambda ()
            (lambda ()
              (f)))))
    )",
    {&analyse_variables, &find_self_variables}
  );
  for_each<local_set_expression>(
    e,
    [] (ptr<local_set_expression> set) {
      auto lambda = expect<lambda_expression>(set->expression());
      EXPECT_EQ(lambda->num_self_references(), 1);
    }
  );
}

TEST_F(ast, set_eliminable_variables_are_constants) {
  expression e = analyse(
    R"(
      (let ((n #void))
        (set! n 2)
        (+ n (read)))
    )",
    {&analyse_variables, &propagate_and_evaluate_constants}
  );

  for_each<application_expression>(
    e,
    [] (ptr<application_expression> app) {
      auto target = assume<top_level_reference_expression>(app->target());
      if (target->variable()->name() == "+") {
        ASSERT_EQ(app->arguments().size(), 2);
        EXPECT_TRUE(is<literal_expression>(app->arguments()[0]));
      }
    }
  );
}

TEST_F(ast, set_eliminable_local_definitions_are_constants) {
  expression e = analyse(
    R"(
      (lambda ()
        (define n 2)
        (+ n (read)))
    )",
    {&analyse_variables, &propagate_and_evaluate_constants}
  );

  for_each<application_expression>(
    e,
    [] (ptr<application_expression> app) {
      auto target = assume<top_level_reference_expression>(app->target());
      if (target->variable()->name() == "+") {
        ASSERT_EQ(app->arguments().size(), 2);
        EXPECT_TRUE(is<literal_expression>(app->arguments()[0]));
      }
    }
  );
}

TEST_F(ast, procedures_bound_to_set_eliminable_variables_are_inlined) {
  expression e = analyse(
    R"(
      (let ((f #void))
        (set! f
          (lambda (x y)
            (+ x y)))
        (f 1 2))
    )",
    {&analyse_variables, &find_self_variables, &inline_procedures}
  );
  for_each<application_expression>(
    e,
    [] (ptr<application_expression> app) {
      auto target = expect<top_level_reference_expression>(app->target());
      EXPECT_EQ(target->variable()->name(), "+");
    }
  );
}

TEST_F(ast, self_referential_lambda_expressions_are_not_inlined) {
  expression e = analyse(
    R"(
      (let ((f #void))
        (set! f
          (lambda ()
            f))
        (f))
    )",
    {&analyse_variables, &find_self_variables, &inline_procedures}
  );

  auto let = expect<let_expression>(e);
  auto seq = expect<sequence_expression>(let->body());
  ASSERT_EQ(seq->expressions().size(), 2);

  auto app = expect<application_expression>(seq->expressions()[1]);
  auto target = expect<local_reference_expression>(app->target());
  EXPECT_EQ(target->variable()->name(), "f");
}

TEST_F(ast, set_eliminable_variables_are_retained_in_lets) {
  expression e = analyse(
    R"(
      (let ((x 0))
        (set! x 1)
        x)
    )",
    {&analyse_variables, &propagate_and_evaluate_constants}
  );
  auto let = expect<let_expression>(e);
  ASSERT_EQ(let->definitions().size(), 1);
  EXPECT_EQ(let->definitions()[0].variable()->name(), "x");
}

TEST_F(ast, non_set_eliminable_variable_does_not_have_constant_initialiser) {
  auto var = parse_and_get_local_variable(
    "x",
    R"(
      (let ((x #void))
        (set! x 0)
        (set! x 1))
    )"
  );
  EXPECT_FALSE(var->constant_initialiser());
}

TEST_F(ast, local_definition_of_set_variable_is_retained) {
  expression e = analyse(
    R"(
      (let ()
        (define x 0)
        (set! x 1)
        x)
    )",
    {&analyse_variables, &propagate_and_evaluate_constants}
  );

  auto seq = expect<sequence_expression>(e);
  ASSERT_EQ(seq->expressions().size(), 1);

  auto let = expect<let_expression>(seq->expressions()[0]);
  EXPECT_EQ(let->definitions().size(), 1);
}

TEST_F(ast, simple_local_loop_is_replaced_with_loop_expressions) {
  expression e = analyse(
    R"(
      (let ((f #void))
        (set! f (lambda () (f))))
    )",
    {&analyse_variables, &find_self_variables, &inline_procedures}
  );
  for_each<lambda_expression>(
    e,
    [] (ptr<lambda_expression> lambda) {
      expression fun_body = ignore_lets_and_sequences(lambda->body());
      ASSERT_TRUE(is<loop_body>(fun_body));

      expression body
        = ignore_lets_and_sequences(assume<loop_body>(fun_body)->body());
      EXPECT_TRUE(is<loop_continue>(body));
    }
  );
}

TEST_F(ast, self_call_with_parameters_is_replaced_with_loop_expression) {
  expression e = analyse(
    R"(
      (let ((f #void))
        (set! f
          (lambda (n)
            (f (+ n 1)))))
    )",
    {&analyse_variables, &find_self_variables, &inline_procedures}
  );

  for_each<loop_continue>(
    e,
    [] (ptr<loop_continue> cont) {
      ASSERT_EQ(cont->variables().size(), 1);
      EXPECT_EQ(expect<local_variable>(cont->variables()[0].variable())->name(),
                "n");

      auto expr
        = expect<application_expression>(cont->variables()[0].expression());
      auto tgt = expect<top_level_reference_expression>(expr->target());
      EXPECT_EQ(tgt->variable()->name(), "+");
    }
  );
}

TEST_F(ast, self_call_in_non_tail_position_is_not_replaced_with_loop) {
  expression e = analyse(
    R"(
      (let ((f #void))
        (set! f
          (lambda ()
            (f)
            0)))
    )",
    {&analyse_variables, &find_self_variables, &inline_procedures}
  );

  for_each<lambda_expression>(
    e,
    [] (ptr<lambda_expression> lambda) {
      auto body = expect<sequence_expression>(lambda->body());
      ASSERT_EQ(body->expressions().size(), 2);
      EXPECT_TRUE(is<application_expression>(body->expressions()[0]));
    }
  );
}

TEST_F(ast, self_call_with_wrong_arity_is_not_replaced_with_loop) {
  expression e = analyse(
    R"(
      (let ((f #void))
        (set! f
          (lambda (x)
            (f x x))))
    )",
    {&analyse_variables, &find_self_variables, &inline_procedures}
  );

  bool called = false;
  for_each<application_expression>(
    e,
    [&] (ptr<application_expression>) {
      called = true;
    }
  );
  EXPECT_TRUE(called);
}

TEST_F(ast, self_call_in_variadic_procedure_is_not_replaced_with_loop) {
  expression e = analyse(
    R"(
      (let ((f #void))
        (set! f
          (lambda args
            (f 0))))
    )",
    {&analyse_variables, &find_self_variables, &inline_procedures}
  );

  bool called = false;
  for_each<application_expression>(
    e,
    [&] (ptr<application_expression>) {
      called = true;
    }
  );
  EXPECT_TRUE(called);
}

TEST_F(ast, self_call_in_top_level_procedure_is_replaced_with_loop) {
  expression e = analyse_module(
    R"(
      (import (insider internal))

      (define f
        (lambda ()
          (f)))
    )",
    {&analyse_variables, &find_self_variables, &inline_procedures}
  );

  auto f_def = expect<lambda_expression>(find_top_level_definition_for(e, "f"));
  auto loop = expect<loop_body>(ignore_lets_and_sequences(f_def->body()));
  auto body = ignore_lets_and_sequences(loop->body());
  EXPECT_TRUE(is<loop_continue>(body));
}

TEST_F(ast, let_expressions_are_created_if_loop_variables_would_overwrite) {
  expression e = analyse(
    R"(
      (let ((f #void))
        (set! f
          (lambda (x y)
            (f y x))))
    )",
    {&analyse_variables, &find_self_variables, &inline_procedures}
  );

  auto set = expect<local_set_expression>(ignore_lets_and_sequences(e));
  auto lambda = expect<lambda_expression>(set->expression());
  auto loop = expect<loop_body>(ignore_lets_and_sequences(lambda->body()));
  auto seq = expect<sequence_expression>(loop->body());
  ASSERT_EQ(seq->expressions().size(), 1);
  auto let = expect<let_expression>(seq->expressions().front());
  ASSERT_EQ(let->definitions().size(), 1);
  auto ref
    = expect<local_reference_expression>(let->definitions()[0].expression());
  auto var = expect<local_variable>(ref->variable());
  EXPECT_EQ(var->name(), "x");
}

TEST_F(ast, loops_arent_self_references) {
  expression e = analyse(
    R"(
      (let ((f #void))
        (set! f (lambda () (f))))
    )",
    {&analyse_variables, &find_self_variables, &inline_procedures}
  );

  for_each<lambda_expression>(
    e,
    [] (ptr<lambda_expression> lambda) {
      EXPECT_EQ(lambda->num_self_references(), 0);
    }
  );
}

TEST_F(ast, loops_in_top_level_procedures_are_not_self_references) {
  expression e = analyse_module(
    R"(
      (import (insider internal))

      (define f
        (lambda ()
          (f)))
    )",
    {&analyse_variables, &find_self_variables, &inline_procedures}
  );

  for_each<lambda_expression>(
    e,
    [] (ptr<lambda_expression> lambda) {
      EXPECT_EQ(lambda->num_self_references(), 0);
    }
  );
}

TEST_F(ast, inlined_loop_uses_correct_variables) {
  expression e = analyse_module(
    R"(
      (import (insider internal))

      (define f
        (lambda (m n)
          (f (* m 2) (+ n 1))))

      (define g
        (lambda ()
          (f 1 0)))
    )",
    {&analyse_variables, &find_self_variables, &inline_procedures}
  );

  auto g_def = expect<lambda_expression>(find_top_level_definition_for(e, "g"));
  auto g_body = expect<sequence_expression>(g_def->body());
  ASSERT_EQ(g_body->expressions().size(), 1);
  auto let = expect<let_expression>(g_body->expressions().front());
  ASSERT_EQ(let->definitions().size(),2 );
  auto var_m = let->definitions()[0].variable();
  auto var_n = let->definitions()[1].variable();
  auto loop = expect<loop_body>(ignore_lets_and_sequences(let->body()));
  auto cont = expect<loop_continue>(ignore_lets_and_sequences(loop->body()));
  ASSERT_EQ(cont->variables().size(), 2);

  EXPECT_EQ(cont->variables()[0].variable(), var_m);
  EXPECT_EQ(cont->variables()[1].variable(), var_n);
}

TEST_F(ast, loop_variable_is_not_constant) {
  expression e = analyse(
    R"(
      (let ((f #void))
        (set! f
          (lambda (m n)
            (f m n)))
        (f 5 (read)))
    )",
    {&analyse_variables, &find_self_variables, &inline_procedures,
     &propagate_and_evaluate_constants}
  );

  auto outer_let = expect<let_expression>(e);
  auto seq = expect<sequence_expression>(outer_let->body());
  ASSERT_EQ(seq->expressions().size(), 2);
  auto inner_let = expect<let_expression>(seq->expressions()[1]);
  EXPECT_EQ(inner_let->definitions().size(), 2);
}

TEST_F(ast, unnecessary_procedure_definitions_are_removed) {
  expression e = analyse(
    R"(
      (let ((f #void) (y 5))
        (set! f (lambda (x) (* 2 x)))
        (set! y 10)
        (set! y 2)
        (f y))
    )",
    {&analyse_variables, &inline_procedures,
     &remove_unnecessary_procedure_definitions}
  );
  auto let = expect<let_expression>(e);
  auto body = expect<sequence_expression>(let->body());
  EXPECT_EQ(body->expressions().size(), 3);
  EXPECT_TRUE(is<let_expression>(body->expressions().back()));
}

TEST_F(ast, explicitly_set_loop_variable_is_boxed) {
  expression e = analyse(
    R"(
      (let ((f #void))
        (set! f
          (lambda (x y)
            (set! y (cons x y))
            (f (+ x 1) y))))
    )",
    {&analyse_variables, &find_self_variables, &inline_procedures,
     &box_set_variables}
  );
  for_each<loop_continue>(
    e,
    [] (ptr<loop_continue> cont) {
      ASSERT_EQ(cont->variables().size(), 2);
      auto y_expr = cont->variables()[1].expression();
      auto box = expect<application_expression>(y_expr);
      auto box_ref = expect<top_level_reference_expression>(box->target());
      EXPECT_EQ(box_ref->variable()->name(), "box");

      ASSERT_EQ(box->arguments().size(), 1);
      auto unbox = expect<application_expression>(box->arguments()[0]);
      auto unbox_ref = expect<top_level_reference_expression>(unbox->target());
      EXPECT_EQ(unbox_ref->variable()->name(), "unbox");
    }
  );
}

TEST_F(ast, call_to_special_procedure_is_replaced_with_built_operation) {
  expression e = analyse(
    "(eq? (read) (read))",
    {&analyse_variables, &inline_built_in_operations}
  );
  ASSERT_TRUE(is<built_in_operation_expression>(e));
  auto op = expect<built_in_operation_expression>(e);
  EXPECT_EQ(op->operation(), opcode::eq);
}

TEST_F(ast, special_procedure_is_not_replaced_when_called_with_unusual_arity) {
  expression e = analyse(
    "(eq? (read) (read) (read))",
    {&analyse_variables, &inline_built_in_operations}
  );
  EXPECT_TRUE(is<application_expression>(e));
}
