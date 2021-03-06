#include "scheme_fixture.hpp"

#include "runtime/basic_types.hpp"
#include "runtime/string.hpp"
#include "runtime/syntax.hpp"

using namespace insider;

struct procedures : scheme_fixture { };

TEST_F(procedures, type_predicates) {
  ptr<pair> p = make<pair>(ctx, ctx.constants->null, ctx.constants->null);
  ptr<> x = p;
  ptr<> null = ctx.constants->null;

  EXPECT_TRUE(is<pair>(x));
  EXPECT_FALSE(is<pair>(null));
  EXPECT_TRUE(expect<pair>(x) == p);
  EXPECT_THROW(expect<pair>(null), type_error);

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
                                       ctx.constants->null));
  EXPECT_TRUE(is_list(l2));

  // (0 1 2)
  ptr<pair> l3 = make<pair>(ctx, integer_to_ptr(integer{0}), l2);
  EXPECT_TRUE(is_list(l3));
}

TEST_F(procedures, make_list) {
  ptr<> empty = make_list(ctx);
  EXPECT_TRUE(empty == ctx.constants->null);

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

  EXPECT_EQ(cdr(third), ctx.constants->null);
}

TEST_F(procedures, eqv) {
  EXPECT_TRUE(eqv(ctx, ctx.constants->t, ctx.constants->t));
  EXPECT_TRUE(eqv(ctx, ctx.constants->f, ctx.constants->f));
  EXPECT_FALSE(eqv(ctx, ctx.constants->t, ctx.constants->f));

  EXPECT_TRUE(eqv(ctx, ctx.intern("foo"), ctx.intern("foo")));
  EXPECT_FALSE(eqv(ctx, ctx.intern("foo"), ctx.intern("bar")));

  EXPECT_TRUE(eqv(ctx, read("1"), read("1")));
  EXPECT_FALSE(eqv(ctx, read("1"), read("0")));

  EXPECT_TRUE(eqv(ctx, read("0.5"), read("0.5")));
  EXPECT_FALSE(eqv(ctx, read("0.5"), read("0.1")));
  EXPECT_FALSE(eqv(ctx, read("0.0"), read("-0.0")));

  EXPECT_TRUE(eqv(ctx, character_to_ptr(L'x'), character_to_ptr(L'x')));
  EXPECT_FALSE(eqv(ctx, character_to_ptr(L'x'), character_to_ptr(L'y')));

  EXPECT_TRUE(eqv(ctx, read("()"), read("()")));
  EXPECT_FALSE(eqv(ctx, read("(a)"), read("(a)")));
}

TEST_F(procedures, equal) {
  EXPECT_TRUE(equal(read("1"), read("1")));
  EXPECT_FALSE(equal(read("1"), read("2")));
  EXPECT_FALSE(equal(read("1"), read("sym")));
  EXPECT_TRUE(equal(read("'(1 2)"), read("'(1 2)")));
  EXPECT_TRUE(equal(read("'(1 2)"), read("(quote (1 2))")));
  EXPECT_FALSE(equal(read("'(1 2)"), read("'(1 3)")));
  EXPECT_FALSE(equal(read("'(1 2)"), read("'(1 2 3)")));
  EXPECT_TRUE(equal(make<string>(ctx, "foo"), make<string>(ctx, "foo")));
  EXPECT_FALSE(equal(make<string>(ctx, "foo"), make<string>(ctx, "bar")));
}

TEST_F(procedures, equal_on_infinite_data_structures) {
  EXPECT_TRUE(equal(read("#1=(a b . #1#)"), read("#2=(a b a b . #2#)")));
  EXPECT_FALSE(equal(read("#1=(a b . #1#)"), read("#2=(a b a . #2#)")));
}

TEST_F(procedures, append) {
  auto r1 = eval("(append '(a1 a2 a3) '(b1 b2 b3) '(c1 c2) '(d1) '() '(f1 f2))");
  EXPECT_TRUE(equal(r1, read("(a1 a2 a3 b1 b2 b3 c1 c2 d1 f1 f2)")));

  auto r2 = eval("(append)");
  EXPECT_TRUE(equal(r2, read("()")));

  auto r3 = eval("(append '(a1 a2 a3))");
  EXPECT_TRUE(equal(r3, read("(a1 a2 a3)")));

  auto r4 = eval("(append '(a1 a2) 'tail)");
  EXPECT_EQ(cddr(expect<pair>(r4)), ctx.intern("tail"));

  auto r5 = eval("(append '() '() '() '())");
  EXPECT_TRUE(equal(r5, ctx.constants->null));

  auto r6 = eval("(append '() '(a1 a2))");
  EXPECT_TRUE(equal(r6, read("(a1 a2)")));

  auto r7 = eval("(append '() '(a1 a2) '() '() '(b1 b2 b3))");
  EXPECT_TRUE(equal(r7, read("(a1 a2 b1 b2 b3)")));

  auto r8 = eval("(append '() '(a1 a2) '() '() '(b1 b2 b3) '())");
  EXPECT_TRUE(equal(r8, read("(a1 a2 b1 b2 b3)")));
}

TEST_F(procedures, syntax_to_datum_on_cyclic_input) {
  auto p1 = cons(ctx, make<syntax>(ctx, integer_to_ptr(1)), ctx.constants->null);
  p1->set_cdr(ctx.store, p1);
  auto stx = make<syntax>(ctx, p1);
  auto datum = syntax_to_datum(ctx, stx);
  EXPECT_TRUE(equal(datum, read("#0=(1 . #0#)")));
}

TEST_F(procedures, syntax_to_datum_preserves_sharing) {
  auto stx = assume<syntax>(read_syntax(ctx, "(#0=(1 2) #0#)"));
  auto datum = expect<pair>(syntax_to_datum(ctx, stx));
  EXPECT_EQ(car(datum), cadr(datum));
}

TEST_F(procedures, box) {
  ptr<> result = eval(
    R"(
      (let ((b1 (box 5))
            (b2 (box 7)))
        (box-set! b1 (+ (unbox b1) (unbox b2)))
        (unbox b1))
    )"
  );
  EXPECT_EQ(expect<integer>(result).value(), 12);
}
