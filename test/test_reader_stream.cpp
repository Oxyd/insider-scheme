#include "scheme_fixture.hpp"

#include "reader_stream.hpp"

using namespace insider;

struct reader_stream_fixture : scheme_fixture {
  reader_stream
  make(std::string data) {
    return reader_stream{track(ctx, open_input_string(ctx, std::move(data)))};
  }
};

TEST_F(reader_stream_fixture, can_read_character_from_port) {
  auto s = make("abc");
  EXPECT_EQ(s.read(), U'a');
  EXPECT_EQ(s.read(), U'b');
  EXPECT_EQ(s.read(), U'c');
  EXPECT_FALSE(s.read());
}

TEST_F(reader_stream_fixture, location_updates_after_read) {
  auto s = make("abc");
  EXPECT_EQ(s.location().column, 1);
  s.read();
  EXPECT_EQ(s.location().column, 2);
  s.read();
  EXPECT_EQ(s.location().column, 3);
}

TEST_F(reader_stream_fixture, location_updates_after_reading_newline) {
  auto s = make("ab\ncd");
  EXPECT_EQ(s.location().line, 1);
  s.read();
  EXPECT_EQ(s.location().line, 1);
  s.read();
  EXPECT_EQ(s.location().line, 1);
  s.read();
  EXPECT_EQ(s.location().line, 2);
  s.read();
  EXPECT_EQ(s.location().line, 2);
}

TEST_F(reader_stream_fixture, checkpoint_reverts_to_previous_state_when_not_committed) {
  auto s = make("abcd");
  s.read();

  {
    auto c = s.make_checkpoint();
    s.read();
    EXPECT_EQ(s.peek(), U'c');
  }

  EXPECT_EQ(s.peek(), U'b');
  EXPECT_EQ(s.read(), U'b');
  EXPECT_EQ(s.read(), U'c');
  EXPECT_EQ(s.read(), U'd');
}

TEST_F(reader_stream_fixture, checkpoint_does_not_revert_when_committed) {
  auto s = make("ab");
  {
    auto c = s.make_checkpoint();
    s.read();
    c.commit();
  }

  EXPECT_EQ(s.read(), U'b');
}

TEST_F(reader_stream_fixture, location_is_correct_after_rollback) {
  auto s = make("abcd");
  s.read();
  EXPECT_EQ(s.location().column, 2);
  {
    auto c = s.make_checkpoint();
    s.read();
    EXPECT_EQ(s.location().column, 3);
  }
  EXPECT_EQ(s.location().column, 2);
}

TEST_F(reader_stream_fixture, stream_position_is_correct_after_reverting_two_nested_checkpoints) {
  auto s = make("abcd");
  s.read();
  {
    auto c1 = s.make_checkpoint();
    s.read();
    {
      auto c2 = s.make_checkpoint();
      s.read();
      EXPECT_EQ(s.peek(), U'd');
    }
  }

  EXPECT_EQ(s.read(), U'b');
}

TEST_F(reader_stream_fixture, stream_position_is_correct_after_reverting_inner_checkpoint_and_committing_outer) {
  auto s = make("abcd");
  s.read();
  {
    auto c1 = s.make_checkpoint();
    s.read();
    {
      auto c2 = s.make_checkpoint();
      s.read();
      EXPECT_EQ(s.peek(), U'd');
    }
    EXPECT_EQ(s.peek(), U'c');
    c1.commit();
  }

  EXPECT_EQ(s.read(), U'c');
}

TEST_F(reader_stream_fixture, location_is_correct_after_rolling_back_past_newline) {
  auto s = make("a\nb");
  EXPECT_EQ(s.location().column, 1);
  EXPECT_EQ(s.location().line, 1);

  {
    auto c = s.make_checkpoint();
    s.read();
    s.read();
    s.read();
    EXPECT_EQ(s.location().column, 2);
    EXPECT_EQ(s.location().line, 2);
  }

  EXPECT_EQ(s.location().column, 1);
  EXPECT_EQ(s.location().line, 1);
}
