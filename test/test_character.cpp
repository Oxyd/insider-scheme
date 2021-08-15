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
  EXPECT_EQ(to_utf8_copy(character{'a'}), "a");
  EXPECT_EQ(to_utf8_copy(character{U'á'}), "\xc3\xa1");
  EXPECT_EQ(to_utf8_copy(character{0x831}), "\xe0\xa0\xb1");
  EXPECT_EQ(to_utf8_copy(character{0x10345}), "\xf0\x90\x8d\x85");
}

TEST_F(character_fixture, utf8_code_point_byte_length) {
  EXPECT_EQ(utf8_code_point_byte_length(static_cast<std::uint8_t>('a')), 1);
  EXPECT_EQ(utf8_code_point_byte_length(0xc3), 2);
  EXPECT_EQ(utf8_code_point_byte_length(0xe0), 3);
  EXPECT_EQ(utf8_code_point_byte_length(0xf0), 4);
  EXPECT_THROW(utf8_code_point_byte_length(0b1000'0000), std::runtime_error);
  EXPECT_THROW(utf8_code_point_byte_length(0b1111'1111), std::runtime_error);
}

TEST_F(character_fixture, from_utf8) {
  using namespace std::literals;

  from_utf8_result r1 = from_utf8(u8"a"sv);
  EXPECT_EQ(r1.code_point, U'a');
  EXPECT_EQ(r1.length, 1);

  from_utf8_result r2 = from_utf8(u8"á"sv);
  EXPECT_EQ(r2.code_point, U'á');
  EXPECT_EQ(r2.length, 2);

  from_utf8_result r3 = from_utf8(u8"࠱"sv);
  EXPECT_EQ(r3.code_point, U'࠱');
  EXPECT_EQ(r3.length, 3);

  from_utf8_result r4 = from_utf8(u8"𐍅"sv);
  EXPECT_EQ(r4.code_point, U'𐍅');
  EXPECT_EQ(r4.length, 4);
}
