#include "scheme_fixture.hpp"

#include "port.hpp"
#include "string.hpp"

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

TEST_F(port_fixture, fresh_port_is_open) {
  auto p = make_string_input_port("");
  EXPECT_TRUE(p->open());
}

TEST_F(port_fixture, closed_port_is_not_open) {
  auto p = make_string_input_port("");
  p->close();
  EXPECT_FALSE(p->open());
}

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
  auto p = make_string_input_port("á");
  EXPECT_EQ(p->peek_character(), U'á');
  EXPECT_EQ(p->read_character(), U'á');
  EXPECT_FALSE(p->peek_character());
}

TEST_F(port_fixture, read_sequence_of_non_ascii_characters) {
  auto p = make_string_input_port("příšerně žluťoučký kůň");
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

TEST_F(port_fixture, cant_peek_or_read_from_closed_port) {
  auto p = make_string_input_port("abc");
  p->close();
  EXPECT_FALSE(p->peek_character());
  EXPECT_FALSE(p->read_character());
}

TEST_F(port_fixture, cant_read_from_closed_port_after_put_back) {
  auto p = make_string_input_port("abc");
  p->close();
  p->put_back('d');
  EXPECT_FALSE(p->read_character());
}

TEST_F(port_fixture, write_character) {
  auto p = make_string_output_port();
  p->write(U'a');
  EXPECT_EQ(p->get_string(), "a");
}

TEST_F(port_fixture, write_non_ascii_character) {
  auto p = make_string_output_port();
  p->write(U'á');
  EXPECT_EQ(p->get_string(), "á");
}

TEST_F(port_fixture, write_sequence_of_characters) {
  auto p = make_string_output_port();
  p->write(U'a');
  p->write(U'á');
  p->write(U'a');
  EXPECT_EQ(p->get_string(), "aáa");
}

TEST_F(port_fixture, fresh_output_port_is_open) {
  auto p = make_string_output_port();
  EXPECT_TRUE(p->open());
}

TEST_F(port_fixture, closed_output_port_is_not_open) {
  auto p = make_string_output_port();
  p->close();
  EXPECT_FALSE(p->open());
}

TEST_F(port_fixture, writing_to_closed_port_does_not_do_anything) {
  auto p = make_string_output_port();
  p->close();
  p->write("abc");
  EXPECT_EQ(p->get_string(), "");
}

TEST_F(port_fixture, unique_port_handle_closes_port_when_out_of_scope) {
  ptr<textual_input_port> p;
  {
    unique_port_handle<ptr<textual_input_port>> h{make_string_input_port("")};
    p = h.get();
    EXPECT_TRUE(p->open());
  }
  EXPECT_FALSE(p->open());
}

TEST_F(port_fixture, open_output_string_creates_string_port) {
  auto result = eval(R"(
    (let ((p (open-output-string)))
      (display "hello" p)
      (display " " p)
      (display "world" p)
      (get-output-string p))
  )");
  EXPECT_EQ(expect<string>(result)->value(), "hello world");
}

TEST_F(port_fixture, open_input_string_can_be_read) {
  auto result = eval(R"(
    (let ((p (open-input-string "foo bar")))
      (let ((x (read p)))
        (let ((y (read p)))
          (cons x y))))
  )");
  EXPECT_TRUE(equal(ctx, result, read("(foo . bar)")));
}
