#include "scheme_fixture.hpp"

#include "syntax.hpp"

using namespace insider;

struct syntax_fixture : scheme_fixture { };

TEST_F(syntax_fixture, added_scope_does_not_immediately_propagate) {
  auto inner_syntax = make<syntax>(ctx, ctx.intern("foo"));
  auto outer_syntax = make<syntax>(ctx, make_list(ctx, inner_syntax));
  auto sc = make<scope>(ctx, "");

  EXPECT_TRUE(outer_syntax->scopes().empty());
  EXPECT_TRUE(inner_syntax->scopes().empty());

  auto outer_with_scope = outer_syntax->add_scope(ctx.store, sc);
  EXPECT_FALSE(outer_with_scope->scopes().empty());
  EXPECT_TRUE(outer_syntax->scopes().empty());
  EXPECT_TRUE(inner_syntax->scopes().empty());
}

TEST_F(syntax_fixture, update_and_get_expression_propagates_scopes) {
  auto inner_syntax = make<syntax>(ctx, ctx.intern("foo"));
  auto outer_syntax = make<syntax>(ctx, make_list(ctx, inner_syntax));
  auto sc = make<scope>(ctx, "");
  auto outer_with_scope = outer_syntax->add_scope(ctx.store, sc);
  auto inner_with_scope = expect<syntax>(car(expect<pair>(outer_with_scope->update_and_get_expression(ctx))));

  EXPECT_FALSE(outer_with_scope->scopes().empty());
  EXPECT_FALSE(inner_with_scope->scopes().empty());
}

TEST_F(syntax_fixture, scopes_propagate_through_multiple_levels) {
  auto inner_syntax = make<syntax>(ctx, ctx.intern("foo"));
  auto middle_syntax = make<syntax>(ctx, make_list(ctx, inner_syntax));
  auto outer_syntax = make<syntax>(ctx, make_list(ctx, middle_syntax));

  auto sc = make<scope>(ctx, "");
  auto outer_with_scope = outer_syntax->add_scope(ctx.store, sc);
  auto middle_with_scope = expect<syntax>(car(expect<pair>(outer_with_scope->update_and_get_expression(ctx))));
  auto inner_with_scope = expect<syntax>(car(expect<pair>(middle_with_scope->update_and_get_expression(ctx))));

  EXPECT_FALSE(inner_with_scope->scopes().empty());
}
