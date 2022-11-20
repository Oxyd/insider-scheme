#include "scheme_fixture.hpp"

#include "compiler/ast.hpp"
#include "compiler/parser_expander.hpp"

using namespace insider;

struct parser : scheme_fixture {
  expression
  parse(std::string const& expr) {
    auto m = make_tracked<module_>(ctx, ctx);
    import_all_exported(ctx, m, ctx.internal_module_tracked());

    null_source_code_provider provider;
    parsing_context pc{ctx, m.get(), {}, {&provider, "<unit test expression>"}};

    auto stx = expect<syntax>(read_syntax(ctx, expr));
    stx = stx->add_scope(ctx.store, m->scope());

    return insider::parse(pc, stx);
  }
};

TEST_F(parser, parse_nullary_lambda) {
  auto l = expect<lambda_expression>(parse("(lambda () 1)"));
  EXPECT_TRUE(l->parameters().empty());
}

TEST_F(parser, parse_lambda_with_required_positional_args) {
  auto l = expect<lambda_expression>(parse("(lambda (a b c) #t)"));
  ASSERT_EQ(l->parameters().size(), 3);
  EXPECT_EQ(l->parameters()[0].variable->name(), "a");
  EXPECT_EQ(l->parameters()[1].variable->name(), "b");
  EXPECT_EQ(l->parameters()[2].variable->name(), "c");
}

TEST_F(parser, parse_lambda_with_only_tail_arg) {
  auto l = expect<lambda_expression>(parse("(lambda l #t)"));
  EXPECT_TRUE(l->has_rest());
  ASSERT_EQ(l->parameters().size(), 1);
  EXPECT_EQ(l->parameters().front().variable->name(), "l");
}

TEST_F(parser, parse_lambda_with_positional_and_tail_args) {
  auto l = expect<lambda_expression>(parse("(lambda (a b . l) #t)"));
  EXPECT_TRUE(l->has_rest());
  ASSERT_EQ(l->parameters().size(), 3);
  EXPECT_EQ(l->parameters()[0].variable->name(), "a");
  EXPECT_EQ(l->parameters()[1].variable->name(), "b");
  EXPECT_EQ(l->parameters()[2].variable->name(), "l");
}

TEST_F(parser, parse_lambda_with_optional_param) {
  auto l = expect<lambda_expression>(parse("(lambda (a (b #:optional)) #t)"));
  ASSERT_EQ(l->parameters().size(), 2);
  EXPECT_EQ(l->parameters()[0].variable->name(), "a");
  EXPECT_EQ(l->parameters()[1].variable->name(), "b");
  EXPECT_FALSE(l->parameters()[0].optional);
  EXPECT_TRUE(l->parameters()[1].optional);
}

TEST_F(parser, invalid_optional_param_syntax_throws) {
  EXPECT_THROW(parse("(lambda (req (opt)) #t)"), syntax_error);
  EXPECT_THROW(parse("(lambda (req (opt #:optional #f)) #t)"), syntax_error);
  EXPECT_THROW(parse("(lambda (req (opt #:maybe)) #t)"), syntax_error);
}

TEST_F(parser, required_param_after_optional_throws) {
  EXPECT_THROW(parse("(lambda ((opt #:optional) req) #t)"), syntax_error);
}

TEST_F(parser, parse_lambda_with_multiple_optional_params) {
  auto l = expect<lambda_expression>(
    parse("(lambda ((a #:optional) (b #:optional)) #t)")
  );
  ASSERT_EQ(l->parameters().size(), 2);
  EXPECT_TRUE(l->parameters()[0].optional);
  EXPECT_TRUE(l->parameters()[1].optional);
}

TEST_F(parser, parse_lambda_with_optional_and_tail_params) {
  auto l = expect<lambda_expression>(
    parse("(lambda ((a #:optional) . tail) #t)")
  );
  ASSERT_EQ(l->parameters().size(), 2);
  EXPECT_TRUE(l->parameters()[0].optional);
  EXPECT_FALSE(l->parameters()[1].optional);
  EXPECT_TRUE(l->has_rest());
}

TEST_F(parser, parse_lambda_with_required_optional_and_tail_params) {
  auto l = expect<lambda_expression>(
    parse("(lambda (a (b #:optional) . tail) #t)")
  );
  ASSERT_EQ(l->parameters().size(), 3);
  EXPECT_FALSE(l->parameters()[0].optional);
  EXPECT_TRUE(l->parameters()[1].optional);
  EXPECT_FALSE(l->parameters()[2].optional);
  EXPECT_TRUE(l->has_rest());
}