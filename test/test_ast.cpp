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
      (set! v 5)
      (+ v 4))
  )");
  EXPECT_TRUE(var->is_set());
  EXPECT_FALSE(var->constant_initialiser());
}

TEST_F(ast, variable_with_non_constant_initialiser_is_not_constant) {
  ptr<local_variable> var = parse_and_get_local_variable("v", R"(
    (let ((v (read)))
      (+ v 2))
  )");
  EXPECT_FALSE(var->is_set());
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
  EXPECT_FALSE(var->is_set());
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
  EXPECT_TRUE(var->is_set());
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
  EXPECT_TRUE(var->is_set());
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
  expression e = analyse_module(R"(
    (import (insider internal))
    (define foo 12)
    (define bar (* 2 foo))
  )");
  for_each<top_level_set_expression>(
    e,
    [] (ptr<top_level_set_expression> set) {
      if (set->target()->name() == "bar") {
        auto app = assume<application_expression>(set->expression());
        ASSERT_EQ(app->arguments().size(), 2);
        EXPECT_TRUE(is<literal_expression>(app->arguments()[0]));
        EXPECT_TRUE(is<literal_expression>(app->arguments()[1]));
      }
    }
  );
}

TEST_F(ast, top_level_constants_are_folded_into_expressions_before_definition) {
  expression e = analyse_module(R"(
    (import (insider internal))
    (define bar (* 2 foo))
    (define foo 12)
  )");
  for_each<top_level_set_expression>(
    e,
    [] (ptr<top_level_set_expression> set) {
      if (set->target()->name() == "bar") {
        auto app = assume<application_expression>(set->expression());
        ASSERT_EQ(app->arguments().size(), 2);
        EXPECT_TRUE(is<literal_expression>(app->arguments()[0]));
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
    = expect<application_expression>(foo_def->body()->expressions().front());
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
    = expect<lambda_expression>(foo_def->body()->expressions().front());
  EXPECT_TRUE(
    is<application_expression>(inner_lambda->body()->expressions().front())
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
  auto foo_body = foo_def->body()->expressions().front();

  auto bar_def
    = expect<lambda_expression>(find_top_level_definition_for(e, "bar"));
  auto bar_let = expect<let_expression>(bar_def->body()->expressions().front());
  auto bar_body = bar_let->body()->expressions().front();

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
  auto bar_let = expect<let_expression>(bar_def->body()->expressions().front());
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
  auto foo_let = expect<let_expression>(foo_def->body()->expressions().front());
  auto foo_var = foo_let->definitions().front().variable();

  auto bar_def
    = expect<lambda_expression>(find_top_level_definition_for(e, "bar"));
  auto bar_outer_let
    = expect<let_expression>(bar_def->body()->expressions().front());
  auto bar_inner_let
    = expect<let_expression>(bar_outer_let->body()->expressions().front());
  auto bar_var = bar_inner_let->definitions().front().variable();

  EXPECT_NE(foo_var, bar_var);
}
