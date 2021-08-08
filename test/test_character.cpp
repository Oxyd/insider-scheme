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

TEST_F(character_fixture, is_whitespace) {
  EXPECT_TRUE(is_white_space(character{' '}));
  EXPECT_TRUE(is_white_space(character{'\t'}));
  EXPECT_TRUE(is_white_space(character{0x2029})); // PARAGRAPH SEPARATOR
  EXPECT_FALSE(is_white_space(character{'a'}));
}

TEST_F(character_fixture, upcase) {
  EXPECT_EQ(upcase(character{'a'}).value(), 'A');
  EXPECT_EQ(upcase(character{'A'}).value(), 'A');
  EXPECT_EQ(upcase(character{'5'}).value(), '5');
  EXPECT_EQ(upcase(character{U'か'}).value(), U'か');
}

TEST_F(character_fixture, downcase) {
  EXPECT_EQ(downcase(character{'A'}).value(), 'a');
  EXPECT_EQ(downcase(character{'a'}).value(), 'a');
  EXPECT_EQ(downcase(character{'5'}).value(), '5');
  EXPECT_EQ(downcase(character{U'か'}).value(), U'か');
}

TEST_F(character_fixture, foldcase) {
  EXPECT_EQ(foldcase(character{'a'}).value(), 'a');
  EXPECT_EQ(foldcase(character{'A'}).value(), 'a');
  EXPECT_EQ(foldcase(character{U'Á'}).value(), U'á');
  EXPECT_EQ(foldcase(character{U'ſ'}).value(), 's');
}

TEST_F(character_fixture, to_utf8) {
  EXPECT_EQ(to_utf8_copy(character{'a'}), std::vector<std::uint8_t>{'a'});
  EXPECT_EQ(to_utf8_copy(character{U'á'}), (std::vector<std::uint8_t>{0xc3, 0xa1}));
  EXPECT_EQ(to_utf8_copy(character{0x831}), (std::vector<std::uint8_t>{0xe0, 0xa0, 0xb1}));
  EXPECT_EQ(to_utf8_copy(character{0x10345}), (std::vector<std::uint8_t>{0xf0, 0x90, 0x8d, 0x85}));
}
