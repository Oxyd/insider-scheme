#include "scheme_fixture.hpp"

#include "compiler/expression.hpp"

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
