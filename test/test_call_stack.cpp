#include "scheme_fixture.hpp"

#include "vm/call_stack.hpp"

using namespace insider;

struct call_stack_fixture : scheme_fixture {
  tracked_ptr<call_stack> cs;

  call_stack_fixture()
    : cs{make_tracked<call_stack>(ctx)}
  { }

  void
  make_4_frames() {
    cs->push_frame(integer_to_ptr(1), 0, 0);
    cs->push_frame(integer_to_ptr(2), 0, 0);
    cs->push_frame(integer_to_ptr(3), 0, 0);
    cs->push_frame(integer_to_ptr(4), 0, 0);
  }
};

TEST_F(call_stack_fixture, create_frame) {
  cs->push_frame(integer_to_ptr(1), 0, 0);
  EXPECT_EQ(expect<integer>(current_frame_callable(cs.get())).value(), 1);
}

TEST_F(call_stack_fixture, pop_frame) {
  cs->push_frame(integer_to_ptr(1), 0, 0);
  cs->push_frame(integer_to_ptr(2), 0, 0);
  EXPECT_EQ(expect<integer>(current_frame_callable(cs.get())).value(), 2);

  cs->pop_frame();
  EXPECT_EQ(expect<integer>(current_frame_callable(cs.get())).value(), 1);
}

TEST_F(call_stack_fixture, push_after_pop) {
  cs->push_frame(integer_to_ptr(1), 0, 0);
  cs->push_frame(integer_to_ptr(2), 0, 0);
  cs->pop_frame();
  cs->push_frame(integer_to_ptr(3), 0, 0);

  EXPECT_EQ(expect<integer>(current_frame_callable(cs.get())).value(), 3);
  cs->pop_frame();
  EXPECT_EQ(expect<integer>(current_frame_callable(cs.get())).value(), 1);
}

TEST_F(call_stack_fixture, new_call_stack_is_empty) {
  EXPECT_TRUE(cs->empty());
}

TEST_F(call_stack_fixture, call_stack_is_empty_after_popping_all_frames) {
  cs->push_frame(integer_to_ptr(1), 0, 0);
  cs->push_frame(integer_to_ptr(2), 0, 0);
  EXPECT_FALSE(cs->empty());

  cs->pop_frame();
  EXPECT_FALSE(cs->empty());

  cs->pop_frame();
  EXPECT_TRUE(cs->empty());
}

TEST_F(call_stack_fixture, new_frame_has_no_extra_data) {
  cs->push_frame(integer_to_ptr(1), 0, 0);
  EXPECT_EQ(current_frame_extra(cs.get()), ptr<>{});
}

TEST_F(call_stack_fixture, can_set_and_retreive_extra_data) {
  cs->push_frame(integer_to_ptr(1), 0, 0);

  auto e = make<stack_frame_extra_data>(ctx);
  current_frame_set_extra(cs.get(), e);
  EXPECT_EQ(current_frame_extra(cs.get()), e);
}

TEST_F(call_stack_fixture, can_set_frame_locals) {
  cs->push_frame(integer_to_ptr(1), 1, 0);
  current_frame_local(cs.get(), 0) = integer_to_ptr(2);
  EXPECT_EQ(current_frame_local(cs.get(), 0), integer_to_ptr(2));
}

TEST_F(call_stack_fixture, get_local_of_parent_frame) {
  cs->push_frame(integer_to_ptr(1), 1, 0);
  current_frame_local(cs.get(), 0) = integer_to_ptr(2);

  cs->push_frame(integer_to_ptr(3), 1, 0);
  current_frame_local(cs.get(), 0) = integer_to_ptr(4);

  EXPECT_EQ(expect<integer>(cs->local(current_frame_parent(cs.get()), 0)).value(), 2);
}

TEST_F(call_stack_fixture, push_pop_individual) {
  cs->push(integer_to_ptr(1));
  EXPECT_EQ(expect<integer>(cs->pop()).value(), 1);
}

TEST_F(call_stack_fixture, iterate_call_stacks) {
  cs->push_frame(integer_to_ptr(1), 1, 0);
  current_frame_local(cs.get(), 0) = integer_to_ptr(1);

  cs->push_frame(integer_to_ptr(2), 1, 0);
  current_frame_local(cs.get(), 0) = integer_to_ptr(2);

  for (call_stack_iterator it{cs.get()}; it != call_stack_iterator{}; ++it)
    EXPECT_EQ(cs->local(*it, 0), cs->callable(*it));
}

TEST_F(call_stack_fixture, capture_tail_part_of_stack_and_append_to_empty_stack) {
  make_4_frames();
  integer::value_type start = cs->parent(current_frame_parent(cs.get()));
  EXPECT_EQ(expect<integer>(cs->callable(start)).value(), 2);

  call_stack::frame_span tail = cs->frames(start, cs->frames_end());

  auto new_cs = make<call_stack>(ctx);
  new_cs->append_frames(tail);

  call_stack_iterator it{new_cs};
  EXPECT_EQ(expect<integer>(new_cs->callable(*it++)).value(), 4);
  EXPECT_EQ(expect<integer>(new_cs->callable(*it++)).value(), 3);
  EXPECT_EQ(expect<integer>(new_cs->callable(*it++)).value(), 2);
  EXPECT_EQ(it, call_stack_iterator());
}

TEST_F(call_stack_fixture, capture_tail_part_of_stack_and_append_to_nonempty_stack) {
  make_4_frames();
  integer::value_type start = cs->parent(current_frame_parent(cs.get()));
  EXPECT_EQ(expect<integer>(cs->callable(start)).value(), 2);

  call_stack::frame_span tail = cs->frames(start, cs->frames_end());

  auto new_cs = make<call_stack>(ctx);
  new_cs->push_frame(integer_to_ptr(10), 4, 0);
  new_cs->push_frame(integer_to_ptr(20), 8, 0);

  new_cs->append_frames(tail);

  call_stack_iterator it{new_cs};
  EXPECT_EQ(expect<integer>(new_cs->callable(*it++)).value(), 4);
  EXPECT_EQ(expect<integer>(new_cs->callable(*it++)).value(), 3);
  EXPECT_EQ(expect<integer>(new_cs->callable(*it++)).value(), 2);
  EXPECT_EQ(expect<integer>(new_cs->callable(*it++)).value(), 20);
  EXPECT_EQ(expect<integer>(new_cs->callable(*it++)).value(), 10);
  EXPECT_EQ(it, call_stack_iterator());
}

TEST_F(call_stack_fixture, capture_middle_part_of_stack_and_append_to_empty_stack) {
  make_4_frames();
  integer::value_type start = cs->parent(current_frame_parent(cs.get()));
  integer::value_type end = cs->current_frame_index();
  call_stack::frame_span middle = cs->frames(start, end);

  auto new_cs = make<call_stack>(ctx);
  new_cs->append_frames(middle);

  call_stack_iterator it{new_cs};
  EXPECT_EQ(expect<integer>(new_cs->callable(*it++)).value(), 3);
  EXPECT_EQ(expect<integer>(new_cs->callable(*it++)).value(), 2);
  EXPECT_EQ(it, call_stack_iterator());
}

TEST_F(call_stack_fixture, move_frame_up) {
  cs->push_frame(integer_to_ptr(1), 0, 0);
  cs->push_frame(integer_to_ptr(2), 2, 0);
  current_frame_local(cs.get(), 0) = integer_to_ptr(1);
  current_frame_local(cs.get(), 1) = integer_to_ptr(2);

  cs->push_frame(integer_to_ptr(3), 3, 0);
  current_frame_local(cs.get(), 0) = integer_to_ptr(10);
  current_frame_local(cs.get(), 1) = integer_to_ptr(20);
  current_frame_local(cs.get(), 2) = integer_to_ptr(30);

  EXPECT_EQ(cs->callable(current_frame_parent(cs.get())), integer_to_ptr(2));

  cs->move_current_frame_up();

  EXPECT_EQ(cs->callable(current_frame_parent(cs.get())), integer_to_ptr(1));
  EXPECT_EQ(current_frame_local(cs.get(), 0), integer_to_ptr(10));
  EXPECT_EQ(current_frame_local(cs.get(), 1), integer_to_ptr(20));
  EXPECT_EQ(current_frame_local(cs.get(), 2), integer_to_ptr(30));

  EXPECT_EQ(current_frame_callable(cs.get()), integer_to_ptr(2));
}

TEST_F(call_stack_fixture, locals_can_be_accessed_through_span) {
  cs->push_frame({}, 2, 0);
  current_frame_local(cs.get(), 0) = integer_to_ptr(1);
  current_frame_local(cs.get(), 1) = integer_to_ptr(2);

  object_span span = cs->current_locals_span();
  EXPECT_EQ(span.size(), 2);
  EXPECT_EQ(span[0], integer_to_ptr(1));
  EXPECT_EQ(span[1], integer_to_ptr(2));
}
