#include "scheme_fixture.hpp"

#include "port.hpp"
#include "string.hpp"

using namespace insider;

struct port_fixture : scheme_fixture {
  ptr<textual_input_port>
  open_input_string(std::string data) {
    return make<textual_input_port>(ctx, std::make_unique<string_port_source>(std::move(data)), "<buffer>");
  }

  ptr<binary_input_port>
  open_input_bytevector(std::vector<std::uint8_t> data) {
    return make<binary_input_port>(ctx, std::make_unique<bytevector_port_source>(std::move(data)));
  }

  ptr<textual_output_port>
  make_string_output_port() {
    return make<textual_output_port>(ctx, std::make_unique<string_port_sink>());
  }

  ptr<binary_output_port>
  make_bytevector_output_port() {
    return make<binary_output_port>(ctx, std::make_unique<bytevector_port_sink>());
  }
};

TEST_F(port_fixture, fresh_port_is_open) {
  auto p = open_input_string("");
  EXPECT_TRUE(p->open());
}

TEST_F(port_fixture, closed_port_is_not_open) {
  auto p = open_input_string("");
  p->close();
  EXPECT_FALSE(p->open());
}

TEST_F(port_fixture, empty_port_is_empty) {
  auto p = open_input_string("");
  EXPECT_FALSE(p->peek_character());
  EXPECT_FALSE(p->read_character());
}

TEST_F(port_fixture, read_one_character_from_port) {
  auto p = open_input_string("a");
  EXPECT_EQ(p->peek_character(), U'a');
  EXPECT_EQ(p->read_character(), U'a');
  EXPECT_FALSE(p->peek_character());
}

TEST_F(port_fixture, peek_single_character_multiple_times) {
  auto p = open_input_string("abc");
  EXPECT_EQ(p->peek_character(), U'a');
  EXPECT_EQ(p->peek_character(), U'a');
  EXPECT_EQ(p->peek_character(), U'a');
}

TEST_F(port_fixture, read_sequence_of_characters) {
  auto p = open_input_string("abc");
  EXPECT_EQ(p->read_character(), U'a');
  EXPECT_EQ(p->read_character(), U'b');
  EXPECT_EQ(p->read_character(), U'c');
  EXPECT_FALSE(p->read_character());
}

TEST_F(port_fixture, read_non_ascii_character) {
  auto p = open_input_string("á");
  EXPECT_EQ(p->peek_character(), U'á');
  EXPECT_EQ(p->read_character(), U'á');
  EXPECT_FALSE(p->peek_character());
}

TEST_F(port_fixture, read_sequence_of_non_ascii_characters) {
  auto p = open_input_string("příšerně žluťoučký kůň");
  EXPECT_EQ(p->read_character(), U'p');
  EXPECT_EQ(p->read_character(), U'ř');
  EXPECT_EQ(p->read_character(), U'í');
  EXPECT_EQ(p->read_character(), U'š');
  EXPECT_EQ(p->read_character(), U'e');
  EXPECT_EQ(p->read_character(), U'r');
  EXPECT_EQ(p->read_character(), U'n');
  EXPECT_EQ(p->read_character(), U'ě');
}

TEST_F(port_fixture, rewind) {
  auto p = open_input_string("abc");
  p->read_character();
  p->read_character();
  EXPECT_EQ(p->peek_character(), U'c');
  p->rewind();
  EXPECT_EQ(p->peek_character(), U'a');
  EXPECT_EQ(p->read_character(), U'a');
  EXPECT_EQ(p->read_character(), U'b');
  EXPECT_EQ(p->read_character(), U'c');
}

TEST_F(port_fixture, cant_peek_or_read_from_closed_port) {
  auto p = open_input_string("abc");
  p->close();
  EXPECT_FALSE(p->peek_character());
  EXPECT_FALSE(p->read_character());
}

TEST_F(port_fixture, cant_read_empty_binary_input_port) {
  auto p = open_input_bytevector({});
  EXPECT_FALSE(p->read_u8());
}

TEST_F(port_fixture, can_read_bytes_from_binary_input_port) {
  auto p = open_input_bytevector({1, 2, 3});
  EXPECT_EQ(p->read_u8(), 1);
  EXPECT_EQ(p->read_u8(), 2);
  EXPECT_EQ(p->read_u8(), 3);
  EXPECT_FALSE(p->read_u8());
}

TEST_F(port_fixture, peek_does_not_advance_binary_input_port) {
  auto p = open_input_bytevector({1, 2, 3});
  EXPECT_EQ(p->peek_u8(), 1);
  EXPECT_EQ(p->peek_u8(), 1);
  EXPECT_EQ(p->read_u8(), 1);
  EXPECT_EQ(p->peek_u8(), 2);
  EXPECT_EQ(p->read_u8(), 2);
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

TEST_F(port_fixture, write_sequence_of_bytes) {
  auto p = make_bytevector_output_port();
  p->write(1);
  p->write(2);

  auto bv = p->get_bytevector(ctx);
  ASSERT_EQ(bv->size(), 2);
  EXPECT_EQ(bv->ref(0), 1);
  EXPECT_EQ(bv->ref(1), 2);
}

TEST_F(port_fixture, unique_port_handle_closes_port_when_out_of_scope) {
  ptr<textual_input_port> p;
  {
    unique_port_handle<ptr<textual_input_port>> h{open_input_string("")};
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
  EXPECT_TRUE(equal(result, read("(foo . bar)")));
}

TEST_F(port_fixture, read_syntax_reads_syntax) {
  auto result = eval(R"(
    (let ((p (open-input-string "foo")))
      (read-syntax p))
  )");
  EXPECT_EQ(expect<syntax>(result)->get_symbol()->value(), "foo");
}
