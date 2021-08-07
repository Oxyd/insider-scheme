#include "scheme_fixture.hpp"

#include "character.hpp"

using namespace insider;

struct character_fixture : scheme_fixture { };

TEST_F(character_fixture, is_numeric) {
  EXPECT_TRUE(is_numeric(character{'4'}));
  EXPECT_FALSE(is_numeric(character{'f'}));
  EXPECT_TRUE(is_numeric(character{1633})); // ARABIC-INDIC DIGIT ONE
  EXPECT_TRUE(is_numeric(character{123638})); // WANCHO DIGIT SIX
}
