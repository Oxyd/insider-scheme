#include "scheme_fixture.hpp"

#include "port.hpp"

using namespace insider;

struct port_fixture : scheme_fixture {
  ptr<textual_input_port>
  make_string_input_port(std::string data) {
    return make<textual_input_port>(ctx, std::make_unique<string_port_source>(std::move(data)), "<buffer>");
  }

  ptr<textual_output_port>
  make_string_output_port() {
    return make<textual_output_port>(ctx, std::make_unique<string_port_sink>());
  }
};

TEST_F(port_fixture, empty_port_is_empty) {
  auto p = make_string_input_port("");
  EXPECT_FALSE(p->peek_character());
  EXPECT_FALSE(p->read_character());
}

TEST_F(port_fixture, read_one_character_from_port) {
  auto p = make_string_input_port("a");
  EXPECT_EQ(p->peek_character(), 'a');
  EXPECT_EQ(p->read_character(), 'a');
  EXPECT_FALSE(p->peek_character());
}

TEST_F(port_fixture, peek_single_character_multiple_times) {
  auto p = make_string_input_port("abc");
  EXPECT_EQ(p->peek_character(), 'a');
  EXPECT_EQ(p->peek_character(), 'a');
  EXPECT_EQ(p->peek_character(), 'a');
}

TEST_F(port_fixture, read_sequence_of_characters) {
  auto p = make_string_input_port("abc");
  EXPECT_EQ(p->read_character(), 'a');
  EXPECT_EQ(p->read_character(), 'b');
  EXPECT_EQ(p->read_character(), 'c');
  EXPECT_FALSE(p->read_character());
}

TEST_F(port_fixture, read_non_ascii_character) {
  auto p = make_string_input_port(u8"á");
  EXPECT_EQ(p->peek_character(), U'á');
  EXPECT_EQ(p->read_character(), U'á');
  EXPECT_FALSE(p->peek_character());
}

TEST_F(port_fixture, read_sequence_of_non_ascii_characters) {
  auto p = make_string_input_port(u8"příšerně žluťoučký kůň");
  EXPECT_EQ(p->read_character(), U'p');
  EXPECT_EQ(p->read_character(), U'ř');
  EXPECT_EQ(p->read_character(), U'í');
  EXPECT_EQ(p->read_character(), U'š');
  EXPECT_EQ(p->read_character(), U'e');
  EXPECT_EQ(p->read_character(), U'r');
  EXPECT_EQ(p->read_character(), U'n');
  EXPECT_EQ(p->read_character(), U'ě');
}

TEST_F(port_fixture, peek_and_read_put_back_character) {
  auto p = make_string_input_port("a");
  p->put_back('b');
  EXPECT_EQ(p->peek_character(), 'b');
  EXPECT_EQ(p->read_character(), 'b');
  EXPECT_EQ(p->read_character(), 'a');
  EXPECT_FALSE(p->read_character());
}

TEST_F(port_fixture, put_back_multiple_characters) {
  auto p = make_string_input_port("a");
  p->put_back('b');
  p->put_back('c');
  EXPECT_EQ(p->read_character(), 'c');
  EXPECT_EQ(p->read_character(), 'b');
  EXPECT_EQ(p->read_character(), 'a');
  EXPECT_FALSE(p->read_character());
}

TEST_F(port_fixture, rewind) {
  auto p = make_string_input_port("abc");
  p->read_character();
  p->read_character();
  EXPECT_EQ(p->peek_character(), 'c');
  p->rewind();
  EXPECT_EQ(p->peek_character(), 'a');
  EXPECT_EQ(p->read_character(), 'a');
  EXPECT_EQ(p->read_character(), 'b');
  EXPECT_EQ(p->read_character(), 'c');
}

TEST_F(port_fixture, write_character) {
  auto p = make_string_output_port();
  p->write(U'a');
  EXPECT_EQ(p->get_string(), "a");
}

TEST_F(port_fixture, write_non_ascii_character) {
  auto p = make_string_output_port();
  p->write(U'á');
  EXPECT_EQ(p->get_string(), u8"á");
}

TEST_F(port_fixture, write_sequence_of_characters) {
  auto p = make_string_output_port();
  p->write(U'a');
  p->write(U'á');
  p->write(U'a');
  EXPECT_EQ(p->get_string(), u8"aáa");
}
