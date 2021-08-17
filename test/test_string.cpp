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

TEST_F(string_fixture, string_set_latin) {
  auto s = make<string>(ctx, "   ");
  EXPECT_EQ(s->ref(0).value(), ' ');
  s->set(0, character{'d'});
  EXPECT_EQ(s->ref(0).value(), 'd');
  EXPECT_EQ(s->ref(1).value(), ' ');
}

TEST_F(string_fixture, string_set_multibyte_same_length) {
  auto s = make<string>(ctx, u8"aáa");
  EXPECT_EQ(s->ref(1).value(), U'á');
  s->set(1, character{U'ž'});
  EXPECT_EQ(s->ref(1).value(), U'ž');
  EXPECT_EQ(s->ref(2).value(), 'a');
}

TEST_F(string_fixture, string_set_multibyte_shorter) {
  auto s = make<string>(ctx, u8"aáa");
  EXPECT_EQ(s->ref(1).value(), U'á');
  s->set(1, character{U'a'});
  EXPECT_EQ(s->ref(1).value(), 'a');
  EXPECT_EQ(s->ref(2).value(), 'a');
}

TEST_F(string_fixture, string_set_multibyte_longer) {
  auto s = make<string>(ctx, "aaa");
  EXPECT_EQ(s->ref(1).value(), 'a');
  s->set(1, character{U'á'});
  EXPECT_EQ(s->ref(1).value(), U'á');
  EXPECT_EQ(s->ref(2).value(), 'a');
}

TEST_F(string_fixture, string_length) {
  EXPECT_EQ(make<string>(ctx, u8"aaa")->length(), 3);
  EXPECT_EQ(make<string>(ctx, u8"aáa")->length(), 3);
  EXPECT_EQ(make<string>(ctx, u8"ááá")->length(), 3);
  EXPECT_EQ(make<string>(ctx, u8"")->length(), 0);
  EXPECT_EQ(make<string>(ctx, u8"か")->length(), 1);
  EXPECT_EQ(make<string>(ctx, u8"áかa")->length(), 3);
}

TEST_F(string_fixture, string_equal) {
  EXPECT_TRUE(string_equal(make<string>(ctx, "aaa"), make<string>(ctx, "aaa")));
  EXPECT_FALSE(string_equal(make<string>(ctx, "aaa"), make<string>(ctx, "aa")));
  EXPECT_FALSE(string_equal(make<string>(ctx, "aaa"), make<string>(ctx, "aba")));
}
