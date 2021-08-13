#include "scheme_fixture.hpp"

#include "string.hpp"

using namespace insider;

struct string_fixture : scheme_fixture { };

TEST_F(string_fixture, string_eqv) {
  auto s1 = make_string(ctx, "foo");
  auto s2 = make_string(ctx, "foo");
  EXPECT_FALSE(s1 == s2);
  EXPECT_TRUE(eqv(ctx, s1, s2));
  EXPECT_NE(object_hash(s1), object_hash(s2));
  EXPECT_EQ(hasheqv(s1), hasheqv(s2));
}
