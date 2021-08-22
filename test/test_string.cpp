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

TEST_F(string_fixture, string_upcase_simple) {
  EXPECT_EQ(upcase(ctx, make<string>(ctx, "aaa"))->value(), "AAA");
  EXPECT_EQ(upcase(ctx, make<string>(ctx, "aAa"))->value(), "AAA");
  EXPECT_EQ(upcase(ctx, make<string>(ctx, "000"))->value(), "000");
  EXPECT_EQ(upcase(ctx, make<string>(ctx, u8"ááá"))->value(), u8"ÁÁÁ");
}

TEST_F(string_fixture, string_upcase_complex) {
  EXPECT_EQ(upcase(ctx, make<string>(ctx, u8"eﬃcient"))->value(), "EFFICIENT");
  EXPECT_EQ(upcase(ctx, make<string>(ctx, u8"scheiße"))->value(), "SCHEISSE");
}

TEST_F(string_fixture, string_downcase_simple) {
  EXPECT_EQ(downcase(ctx, make<string>(ctx, "AAA"))->value(), "aaa");
  EXPECT_EQ(downcase(ctx, make<string>(ctx, "aAa"))->value(), "aaa");
  EXPECT_EQ(downcase(ctx, make<string>(ctx, "000"))->value(), "000");
  EXPECT_EQ(downcase(ctx, make<string>(ctx, u8"ÁÁÁ"))->value(), u8"ááá");
}

TEST_F(string_fixture, sigma) {
  EXPECT_EQ(upcase(ctx, make<string>(ctx, u8"Ὀδυσσεύς"))->value(), u8"ὈΔΥΣΣΕΎΣ");
  EXPECT_EQ(downcase(ctx, make<string>(ctx, u8"ὈΔΥΣΣΕΎΣ"))->value(), u8"ὀδυσσεύς");
  EXPECT_EQ(downcase(ctx, make<string>(ctx, u8"Σ"))->value(), u8"σ");
}
