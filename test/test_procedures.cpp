#include "scheme_fixture.hpp"

#include "basic_types.hpp"
#include "string.hpp"

using namespace insider;

struct procedures : scheme_fixture { };

TEST_F(procedures, type_predicates) {
  ptr<pair> p = make<pair>(ctx, ctx.constants->null.get(), ctx.constants->null.get());
  ptr<> x = p;
  ptr<> null = ctx.constants->null.get();

  EXPECT_TRUE(is<pair>(x));
  EXPECT_FALSE(is<pair>(null));
  EXPECT_TRUE(expect<pair>(x) == p);
  EXPECT_THROW(expect<pair>(null), error);

  if (match<pair>(x))
    SUCCEED();
  else
    ADD_FAILURE();

  if (match<pair>(null))
    ADD_FAILURE();
  else
    SUCCEED();
}

TEST_F(procedures, is_list) {
  // (1 . 2)
  ptr<pair> l1 = make<pair>(ctx, integer_to_ptr(integer{1}), integer_to_ptr(integer{2}));
  EXPECT_FALSE(is_list(l1));

  // (1 2)
  ptr<pair> l2 = make<pair>(ctx,
                            integer_to_ptr(integer{1}),
                            make<pair>(ctx,
                                       integer_to_ptr(integer{2}),
                                       ctx.constants->null.get()));
  EXPECT_TRUE(is_list(l2));

  // (0 1 2)
  ptr<pair> l3 = make<pair>(ctx, integer_to_ptr(integer{0}), l2);
  EXPECT_TRUE(is_list(l3));
}

TEST_F(procedures, make_list) {
  ptr<> empty = make_list(ctx);
  EXPECT_TRUE(empty == ctx.constants->null.get());

  ptr<> l = make_list(ctx,
                            integer_to_ptr(integer{1}),
                            integer_to_ptr(integer{2}),
                            integer_to_ptr(integer{3}));
  auto first = expect<pair>(l);
  EXPECT_EQ(expect<integer>(car(first)).value(), 1);

  auto second = expect<pair>(cdr(first));
  EXPECT_EQ(expect<integer>(car(second)).value(), 2);

  auto third = expect<pair>(cdr(second));
  EXPECT_EQ(expect<integer>(car(third)).value(), 3);

  EXPECT_EQ(cdr(third), ctx.constants->null.get());
}

TEST_F(procedures, equal) {
  EXPECT_TRUE(equal(ctx, read("1"), read("1")));
  EXPECT_FALSE(equal(ctx, read("1"), read("2")));
  EXPECT_FALSE(equal(ctx, read("1"), read("sym")));
  EXPECT_TRUE(equal(ctx, read("'(1 2)"), read("'(1 2)")));
  EXPECT_TRUE(equal(ctx, read("'(1 2)"), read("(quote (1 2))")));
  EXPECT_FALSE(equal(ctx, read("'(1 2)"), read("'(1 3)")));
  EXPECT_FALSE(equal(ctx, read("'(1 2)"), read("'(1 2 3)")));
  EXPECT_TRUE(equal(ctx, make<string>(ctx, "foo"), make<string>(ctx, "foo")));
  EXPECT_FALSE(equal(ctx, make<string>(ctx, "foo"), make<string>(ctx, "bar")));
}

TEST_F(procedures, equal_on_infinite_data_structures) {
  EXPECT_TRUE(equal(ctx, read("#1=(a b . #1#)"), read("#2=(a b a b . #2#)")));
  EXPECT_FALSE(equal(ctx, read("#1=(a b . #1#)"), read("#2=(a b a . #2#)")));
}

TEST_F(procedures, append) {
  auto r1 = eval("(append '(a1 a2 a3) '(b1 b2 b3) '(c1 c2) '(d1) '() '(f1 f2))");
  EXPECT_TRUE(equal(ctx, r1, read("(a1 a2 a3 b1 b2 b3 c1 c2 d1 f1 f2)")));

  auto r2 = eval("(append)");
  EXPECT_TRUE(equal(ctx, r2, read("()")));

  auto r3 = eval("(append '(a1 a2 a3))");
  EXPECT_TRUE(equal(ctx, r3, read("(a1 a2 a3)")));

  auto r4 = eval("(append '(a1 a2) 'tail)");
  EXPECT_EQ(cddr(expect<pair>(r4)), ctx.intern("tail"));

  auto r5 = eval("(append '() '() '() '())");
  EXPECT_TRUE(equal(ctx, r5, ctx.constants->null.get()));

  auto r6 = eval("(append '() '(a1 a2))");
  EXPECT_TRUE(equal(ctx, r6, read("(a1 a2)")));

  auto r7 = eval("(append '() '(a1 a2) '() '() '(b1 b2 b3))");
  EXPECT_TRUE(equal(ctx, r7, read("(a1 a2 b1 b2 b3)")));

  auto r8 = eval("(append '() '(a1 a2) '() '() '(b1 b2 b3) '())");
  EXPECT_TRUE(equal(ctx, r8, read("(a1 a2 b1 b2 b3)")));
}
