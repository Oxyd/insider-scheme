#include "scheme_fixture.hpp"

#include "compiler/analyser.hpp"
#include "compiler/ast_transforms.hpp"
#include "compiler/expression.hpp"
#include "compiler/parser_expander.hpp"
#include "compiler/parsing_context.hpp"

using namespace insider;

struct ast : scheme_fixture {
  expression
  make_nested_call() {
    return make<application_expression>(
      ctx,
      make<local_reference_expression>(ctx, make<variable>(ctx, "foo")),
      make<application_expression>(
        ctx,
        make<local_reference_expression>(ctx, make<variable>(ctx, "bar")),
        make<literal_expression>(ctx, ctx.intern("baz"))
      )
    );
  }

  expression
  parse(ptr<syntax> expr_stx) {
    auto m = make_tracked<module_>(ctx, ctx);
    import_all_exported(ctx, m, ctx.internal_module_tracked());

    expr_stx = expr_stx->add_scope(ctx.store, m->scope());

    insider::null_source_code_provider provider;
    parsing_context pc{ctx, m.get(), {&provider, "<unit test expression>"}};

    return insider::parse(pc, expr_stx);
  }

  expression
  parse(std::string const& expr) {
    auto expr_stx = read_syntax(ctx, expr);
    if (expr_stx == ctx.constants->eof)
      throw std::runtime_error{"EOF"};
    else
      return parse(assume<syntax>(expr_stx));
  }

  expression
  analyse(ptr<syntax> expr_stx) {
    auto m = make_tracked<module_>(ctx, ctx);
    import_all_exported(ctx, m, ctx.internal_module_tracked());

    insider::null_source_code_provider provider;
    return insider::analyse(ctx, expr_stx, m,
                            {&provider, "<unit test expression>"});
  }

  expression
  analyse(std::string const& expr) {
    auto expr_stx = read_syntax(ctx, expr);
    if (expr_stx == ctx.constants->eof)
      throw std::runtime_error{"EOF"};
    else
      return analyse(assume<syntax>(expr_stx));
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
  EXPECT_EQ(v.seen_applications, 2);
  EXPECT_EQ(v.seen_local_references, 2);
  EXPECT_EQ(v.seen_literals, 1);
}

TEST_F(ast, map_ast_makes_a_deep_copy) {
  expression e = make_nested_call();
  expression copy = map_ast(ctx, e,
                            [] (auto expr) -> expression { return expr; });
  EXPECT_NE(e, copy);
  EXPECT_TRUE(is<application_expression>(copy));
  EXPECT_NE(assume<application_expression>(e)->target(),
            assume<application_expression>(copy)->target());
  EXPECT_TRUE(is<local_reference_expression>(
    assume<application_expression>(copy)->target()
  ));
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
      make<local_reference_expression>(ctx, make<variable>(ctx, "identity")),
      ref
    );
  };

  expression
  operator () (auto other) { return other; }
};

TEST_F(ast, map_ast_to_change_its_shape) {
  expression e = make_nested_call();
  expression f = map_ast(ctx, e, wrap_local_reference_in_identity{ctx});
  EXPECT_TRUE(
    is<local_reference_expression>(expect<application_expression>(e)->target())
  );
  EXPECT_FALSE(
    is<local_reference_expression>(expect<application_expression>(f)->target())
  );
  EXPECT_TRUE(
    is<application_expression>(expect<application_expression>(f)->target())
  );
}

static ptr<variable>
find_variable(std::string const& name, expression);

static ptr<variable>
find_variable_in_subexpressions(std::string const& name, auto expr) {
  std::vector<expression> subexprs;
  expr->visit_subexpressions([&] (expression e) { subexprs.push_back(e); });

  for (expression e : subexprs)
    if (ptr<variable> v = find_variable(name, e))
      return v;
  return {};
}

static ptr<variable>
find_variable(std::string const& name, ptr<local_reference_expression> ref) {
  if (ref->variable()->name == name)
    return ref->variable();
  else
    return {};
}

static ptr<variable>
find_variable(std::string const& name, ptr<top_level_reference_expression> ref) {
  if (ref->variable()->name == name)
    return ref->variable();
  else
    return {};
}

static ptr<variable>
find_variable(std::string const& name, ptr<let_expression> let) {
  for (auto const& dp : let->definitions())
    if (dp.variable()->name == name)
      return dp.variable();

  return find_variable_in_subexpressions(name, let);
}

static ptr<variable>
find_variable(std::string const& name, ptr<local_set_expression> set) {
  if (set->target()->name == name)
    return set->target();
  else
    return find_variable_in_subexpressions(name, set);
}

static ptr<variable>
find_variable(std::string const& name, ptr<top_level_set_expression> set) {
  if (set->target()->name == name)
    return set->target();
  else
    return find_variable_in_subexpressions(name, set);
}

static ptr<variable>
find_variable(std::string const& name, ptr<lambda_expression> lambda) {
  for (ptr<variable> v : lambda->parameters())
    if (v->name == name)
      return v;

  return find_variable_in_subexpressions(name, lambda);
}

static ptr<variable>
find_variable(std::string const& name, auto expr) {
  return find_variable_in_subexpressions(name, expr);
}

static ptr<variable>
find_variable(std::string const& name, expression e) {
  return visit([&] (auto expr) { return find_variable(name, expr); },
               e);
}

struct variable_analysis : ast {
  ptr<variable>
  parse_and_get_variable(std::string const& variable_name,
                         std::string const& expr) {
    expression e = parse(expr);
    analyse_variables(e);
    ptr<variable> v = find_variable(variable_name, e);
    assert(v);
    return v;
  }
};

TEST_F(variable_analysis, analyse_variables_recognises_constants) {
  ptr<variable> var = parse_and_get_variable("const", R"(
    (let ((const 2))
      (+ const 4))
  )");
  EXPECT_EQ(expect<integer>(var->constant_value).value(), 2);
}

TEST_F(variable_analysis, set_variable_is_not_marked_as_constant) {
  ptr<variable> var = parse_and_get_variable("v", R"(
    (let ((v 2))
      (set! v 5)
      (+ v 4))
  )");
  EXPECT_TRUE(var->is_set);
  EXPECT_FALSE(var->constant_value);
}

TEST_F(variable_analysis,
       variable_with_non_constant_initialiser_is_not_constant) {
  ptr<variable> var = parse_and_get_variable("v", R"(
    (let ((v (read)))
      (+ v 2))
  )");
  EXPECT_FALSE(var->is_set);
  EXPECT_FALSE(var->constant_value);
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

TEST_F(ast, constant_definitions_are_removed_from_lets) {
  expression e = analyse(R"(
    (let ((const 2) (non-const (read)))
      (+ const non-const))
  )");
  for_each<let_expression>(
    e,
    [] (ptr<let_expression> let) {
      ASSERT_EQ(let->definitions().size(), 1);
      EXPECT_EQ(let->definitions().front().variable()->name, "non-const");
    }
  );
}
