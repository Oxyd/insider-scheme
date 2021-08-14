#include "scheme_fixture.hpp"

#include "string.hpp"

using namespace insider;

struct string_fixture : scheme_fixture { };

TEST_F(string_fixture, string_eqv) {
  auto s1 = make<string>(ctx, "foo");
  auto s2 = make<string>(ctx, "foo");
  EXPECT_FALSE(s1 == s2);
  EXPECT_TRUE(eqv(ctx, s1, s2));
  EXPECT_NE(object_hash(s1), object_hash(s2));
  EXPECT_EQ(hasheqv(s1), hasheqv(s2));
}

TEST_F(string_fixture, string_ref) {
  auto s1 = make<string>(ctx, "foo");
  EXPECT_EQ(s1->ref(0).value(), 'f');
  EXPECT_EQ(s1->ref(1).value(), 'o');
  EXPECT_EQ(s1->ref(2).value(), 'o');
}

TEST_F(string_fixture, string_set) {
  auto s = make<string>(ctx, "   ");
  EXPECT_EQ(s->ref(0).value(), ' ');
  s->set(0, 'd');
  EXPECT_EQ(s->ref(0).value(), 'd');
  EXPECT_EQ(s->ref(1).value(), ' ');
}