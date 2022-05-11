#include "scheme_fixture.hpp"

#include "runtime/syntax.hpp"

using namespace insider;

struct syntax_fixture : scheme_fixture { };

TEST_F(syntax_fixture, added_scope_does_not_immediately_propagate) {
  auto inner_syntax = make<syntax>(ctx, ctx.intern("foo"));
  auto outer_syntax = make<syntax>(ctx, make_list(ctx, inner_syntax));
  auto sc = make<scope>(ctx, ctx, "");

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
  auto sc = make<scope>(ctx, ctx, "");
  auto outer_with_scope = outer_syntax->add_scope(ctx.store, sc);
  auto inner_with_scope = expect<syntax>(car(expect<pair>(outer_with_scope->update_and_get_expression(ctx))));

  EXPECT_FALSE(outer_with_scope->scopes().empty());
  EXPECT_FALSE(inner_with_scope->scopes().empty());
}

TEST_F(syntax_fixture, scopes_propagate_through_multiple_levels) {
  auto inner_syntax = make<syntax>(ctx, ctx.intern("foo"));
  auto middle_syntax = make<syntax>(ctx, make_list(ctx, inner_syntax));
  auto outer_syntax = make<syntax>(ctx, make_list(ctx, middle_syntax));

  auto sc = make<scope>(ctx, ctx, "");
  auto outer_with_scope = outer_syntax->add_scope(ctx.store, sc);
  auto middle_with_scope = expect<syntax>(car(expect<pair>(outer_with_scope->update_and_get_expression(ctx))));
  auto inner_with_scope = expect<syntax>(car(expect<pair>(middle_with_scope->update_and_get_expression(ctx))));

  EXPECT_FALSE(inner_with_scope->scopes().empty());
}

TEST_F(syntax_fixture, datum_to_syntax_copies_location_and_scopes) {
  auto scp = make<scope>(ctx, ctx, "scope");
  auto src_stx = make<syntax>(
    ctx,
    ctx.intern("foo"),
    source_location{.file_name="foo", .line=10, .column=20},
    scope_set{scp}
  );

  auto datum = ctx.intern("bar");
  auto new_stx = datum_to_syntax(ctx, src_stx, datum);

  ASSERT_TRUE(new_stx->contains<symbol>());
  EXPECT_EQ(
    expect<symbol>(new_stx->update_and_get_expression(ctx))->value(),
    "bar"
  );
  ASSERT_EQ(new_stx->scopes().size(), 1);
  EXPECT_EQ(new_stx->scopes().back()->description(), "scope");

  EXPECT_EQ(new_stx->location(),
            (source_location{.file_name="foo", .line=10, .column=20}));
}

TEST_F(syntax_fixture, datum_to_syntax_with_no_source_syntax) {
  auto datum = ctx.intern("foo");
  auto stx = datum_to_syntax(ctx, {}, datum);

  EXPECT_TRUE(stx->scopes().empty());
  EXPECT_EQ(stx->location(), source_location{});
}