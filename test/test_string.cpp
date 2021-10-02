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
  EXPECT_EQ(s1->ref(0), 'f');
  EXPECT_EQ(s1->ref(1), 'o');
  EXPECT_EQ(s1->ref(2), 'o');
}

TEST_F(string_fixture, string_set_latin) {
  auto s = make<string>(ctx, "   ");
  EXPECT_EQ(s->ref(0), ' ');
  s->set(0, 'd');
  EXPECT_EQ(s->ref(0), 'd');
  EXPECT_EQ(s->ref(1), ' ');
}

TEST_F(string_fixture, string_set_multibyte_same_length) {
  auto s = make<string>(ctx, "aáa");
  EXPECT_EQ(s->ref(1), U'á');
  s->set(1, U'ž');
  EXPECT_EQ(s->ref(1), U'ž');
  EXPECT_EQ(s->ref(2), 'a');
}

TEST_F(string_fixture, string_set_multibyte_shorter) {
  auto s = make<string>(ctx, "aáa");
  EXPECT_EQ(s->ref(1), U'á');
  s->set(1, U'a');
  EXPECT_EQ(s->ref(1), 'a');
  EXPECT_EQ(s->ref(2), 'a');
}

TEST_F(string_fixture, string_set_multibyte_longer) {
  auto s = make<string>(ctx, "aaa");
  EXPECT_EQ(s->ref(1), 'a');
  s->set(1, U'á');
  EXPECT_EQ(s->ref(1), U'á');
  EXPECT_EQ(s->ref(2), 'a');
}

TEST_F(string_fixture, string_length) {
  EXPECT_EQ(make<string>(ctx, "aaa")->length(), 3);
  EXPECT_EQ(make<string>(ctx, "aáa")->length(), 3);
  EXPECT_EQ(make<string>(ctx, "ááá")->length(), 3);
  EXPECT_EQ(make<string>(ctx, "")->length(), 0);
  EXPECT_EQ(make<string>(ctx, "か")->length(), 1);
  EXPECT_EQ(make<string>(ctx, "áかa")->length(), 3);
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
  EXPECT_EQ(upcase(ctx, make<string>(ctx, "ááá"))->value(), "ÁÁÁ");
}

TEST_F(string_fixture, string_upcase_complex) {
  EXPECT_EQ(upcase(ctx, make<string>(ctx, "eﬃcient"))->value(), "EFFICIENT");
  EXPECT_EQ(upcase(ctx, make<string>(ctx, "scheiße"))->value(), "SCHEISSE");
}

TEST_F(string_fixture, string_downcase_simple) {
  EXPECT_EQ(downcase(ctx, make<string>(ctx, "AAA"))->value(), "aaa");
  EXPECT_EQ(downcase(ctx, make<string>(ctx, "aAa"))->value(), "aaa");
  EXPECT_EQ(downcase(ctx, make<string>(ctx, "000"))->value(), "000");
  EXPECT_EQ(downcase(ctx, make<string>(ctx, "ÁÁÁ"))->value(), "ááá");
}

TEST_F(string_fixture, sigma) {
  EXPECT_EQ(upcase(ctx, make<string>(ctx, "Ὀδυσσεύς"))->value(), "ὈΔΥΣΣΕΎΣ");
  EXPECT_EQ(downcase(ctx, make<string>(ctx, "ὈΔΥΣΣΕΎΣ"))->value(), "ὀδυσσεύς");
  EXPECT_EQ(downcase(ctx, make<string>(ctx, "Σ"))->value(), "σ");
}

TEST_F(string_fixture, string_foldcase) {
  EXPECT_EQ(foldcase(ctx, make<string>(ctx, "scheiße"))->value(), "scheisse");
  EXPECT_EQ(foldcase(ctx, make<string>(ctx, "ꮜꮝꮞ"))->value(), "ᏌᏍᏎ");
}
