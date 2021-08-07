#include "scheme_fixture.hpp"

#include "character.hpp"
#include "code_point_properties.hpp"

using namespace insider;

struct character_fixture : scheme_fixture { };

TEST_F(character_fixture, is_numeric) {
  EXPECT_TRUE(is_numeric(character{'4'}));
  EXPECT_FALSE(is_numeric(character{'f'}));
  EXPECT_TRUE(is_numeric(character{1633})); // ARABIC-INDIC DIGIT ONE
  EXPECT_TRUE(is_numeric(character{123638})); // WANCHO DIGIT SIX
}

TEST_F(character_fixture, codepoint_hash) {
  for (std::size_t i = 0; i < code_points.size(); ++i)
    EXPECT_EQ(codepoint_hash(code_points[i].code_point), i);
}

TEST_F(character_fixture, digit_value) {
  EXPECT_EQ(expect<integer>(digit_value(ctx, character{'8'})).value(), 8);
  EXPECT_EQ(expect<integer>(digit_value(ctx, character{123638})).value(), 6);
  EXPECT_EQ(digit_value(ctx, character{'a'}), ctx.constants->f.get());
}

TEST_F(character_fixture, is_alphabetic) {
  EXPECT_TRUE(is_alphabetic(character{'a'}));
  EXPECT_FALSE(is_alphabetic(character{';'}));
  EXPECT_FALSE(is_alphabetic(character{' '}));
  EXPECT_TRUE(is_alphabetic(character{U'ř'}));
}

TEST_F(character_fixture, is_lower_or_upper_case) {
  EXPECT_TRUE(is_lower_case(character{'a'}));
  EXPECT_FALSE(is_upper_case(character{'a'}));
  EXPECT_FALSE(is_lower_case(character{'A'}));
  EXPECT_TRUE(is_upper_case(character{'A'}));
  EXPECT_FALSE(is_lower_case(character{'0'}));
  EXPECT_FALSE(is_upper_case(character{'0'}));

  EXPECT_TRUE(is_alphabetic(character{U'か'}));
  EXPECT_FALSE(is_lower_case(character{U'か'}));
  EXPECT_FALSE(is_upper_case(character{U'か'}));
}
