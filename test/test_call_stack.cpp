#include "scheme_fixture.hpp"

#include "vm/call_stack.hpp"

using namespace insider;

struct call_stack_fixture : scheme_fixture {
  tracked_ptr<call_stack> cs;
  ptr<procedure> one = make_dummy_procedure();
  ptr<procedure> two = make_dummy_procedure();
  ptr<procedure> three = make_dummy_procedure();
  ptr<procedure> four = make_dummy_procedure();
  ptr<procedure> ten = make_dummy_procedure();
  ptr<procedure> twenty = make_dummy_procedure();

  call_stack_fixture()
    : cs{make_tracked<call_stack>(ctx)}
  { }

  void
  make_4_frames() {
    cs->push_frame(one, cs->size(), 1, 0, 0);
    cs->local(0) = one;
    cs->push_frame(two, cs->size(), 1, 0, 0);
    cs->local(0) = two;
    cs->push_frame(three, cs->size(), 1, 0, 0);
    cs->local(0) = three;
    cs->push_frame(four, cs->size(), 1, 0, 0);
    cs->local(0) = four;
  }

  ptr<procedure>
  make_dummy_procedure() {
    return make<procedure>(
      ctx,
      make<procedure_prototype>(ctx, mutable_bytecode{}, debug_info_map{}, 0, 0,
                                false, "<dummy>", std::vector<ptr<>>{}),
      0
    );
  }
};

TEST_F(call_stack_fixture, create_frame) {
  cs->push_frame(one, cs->size(), 1, nullptr, 0);
  cs->local(0) = one;
  EXPECT_EQ(cs->callable(), one);
}

TEST_F(call_stack_fixture, pop_frame) {
  cs->push_frame(one, cs->size(), 1, nullptr, 0);
  cs->local(0) = one;
  cs->push_frame(two, cs->size(), 1, nullptr, 0);
  cs->local(0) = two;
  EXPECT_EQ(cs->callable(), two);

  cs->pop_frame();
  EXPECT_EQ(cs->callable(), one);
}

TEST_F(call_stack_fixture, push_after_pop) {
  cs->push_frame(one, cs->size(), 1, nullptr, 0);
  cs->local(0) = one;
  cs->push_frame(two, cs->size(), 1, nullptr, 0);
  cs->local(0) = two;
  cs->pop_frame();
  cs->push_frame(three, cs->size(), 1, nullptr, 0);
  cs->local(0) = three;

  EXPECT_EQ(cs->callable(), three);
  cs->pop_frame();
  EXPECT_EQ(cs->callable(), one);
}

TEST_F(call_stack_fixture, new_call_stack_is_empty) {
  EXPECT_TRUE(cs->empty());
}

TEST_F(call_stack_fixture, call_stack_is_empty_after_popping_all_frames) {
  cs->push_frame(one, cs->size(), 1, nullptr, 0);
  cs->push_frame(two, cs->size(), 1, nullptr, 0);
  EXPECT_FALSE(cs->empty());

  cs->pop_frame();
  EXPECT_FALSE(cs->empty());

  cs->pop_frame();
  EXPECT_TRUE(cs->empty());
}

TEST_F(call_stack_fixture, new_frame_has_no_extra_data) {
  cs->push_frame(one, cs->size(), 1, nullptr, 0);
  EXPECT_EQ(cs->extra(), ptr<>{});
}

TEST_F(call_stack_fixture, can_set_and_retreive_extra_data) {
  cs->push_frame(one, cs->size(), 1, nullptr, 0);

  auto e = make<stack_frame_extra_data>(ctx);
  cs->set_extra(e);
  EXPECT_EQ(cs->extra(), e);
}

TEST_F(call_stack_fixture, can_set_frame_locals) {
  cs->push_frame(one, cs->size(), 1, nullptr, 0);
  cs->local(0) = integer_to_ptr(2);
  EXPECT_EQ(cs->local(0), integer_to_ptr(2));
}

TEST_F(call_stack_fixture, get_local_of_parent_frame) {
  cs->push_frame(one, cs->size(), 1, nullptr, 0);
  cs->local(0) = integer_to_ptr(2);

  cs->push_frame(three, cs->size(), 1, nullptr, 0);
  cs->local(0) = integer_to_ptr(4);

  EXPECT_EQ(expect<integer>(cs->local(*cs->parent(), 0)).value(), 2);
}

TEST_F(call_stack_fixture, iterate_call_stacks) {
  cs->push_frame(one, cs->size(), 2, nullptr, 0);
  cs->local(0) = one;
  cs->local(1) = integer_to_ptr(1);

  cs->push_frame(two, cs->size(), 2, nullptr, 0);
  cs->local(0) = two;
  cs->local(1) = integer_to_ptr(2);

  for (call_stack::frame_index f : cs->frames_range()) {
    if (cs->callable(f) == one)
      EXPECT_EQ(expect<integer>(cs->local(f, 1)).value(), 1);
    else
      EXPECT_EQ(expect<integer>(cs->local(f, 1)).value(), 2);
  }
}

TEST_F(call_stack_fixture, capture_tail_part_of_stack_and_append_to_empty_stack) {
  make_4_frames();
  call_stack::frame_index start = *cs->parent(*cs->parent());
  EXPECT_EQ(cs->callable(start), two);

  call_stack::frame_span tail = cs->frames(start, cs->frames_end());

  auto new_cs = make<call_stack>(ctx);
  new_cs->append_frames(tail);

  auto it = new_cs->frames_range().begin();
  EXPECT_EQ(new_cs->callable(*it++), four);
  EXPECT_EQ(new_cs->callable(*it++), three);
  EXPECT_EQ(new_cs->callable(*it++), two);
  EXPECT_EQ(it, new_cs->frames_range().end());
}

TEST_F(call_stack_fixture,
       capture_tail_part_of_stack_and_append_to_nonempty_stack) {
  make_4_frames();
  call_stack::frame_index start = *cs->parent(*cs->parent());
  EXPECT_EQ(cs->callable(start), two);

  call_stack::frame_span tail = cs->frames(start, cs->frames_end());

  auto new_cs = make<call_stack>(ctx);
  new_cs->push_frame(ten, new_cs->size(), 4, nullptr, 0);
  new_cs->local(0) = ten;
  new_cs->push_frame(twenty, new_cs->size(), 8, nullptr, 0);
  new_cs->local(0) = twenty;

  new_cs->append_frames(tail);

  auto it = new_cs->frames_range().begin();
  EXPECT_EQ(new_cs->callable(*it++), four);
  EXPECT_EQ(new_cs->callable(*it++), three);
  EXPECT_EQ(new_cs->callable(*it++), two);
  EXPECT_EQ(new_cs->callable(*it++), twenty);
  EXPECT_EQ(new_cs->callable(*it++), ten);
  EXPECT_EQ(it, new_cs->frames_range().end());
}

TEST_F(call_stack_fixture,
       capture_middle_part_of_stack_and_append_to_empty_stack) {
  make_4_frames();
  call_stack::frame_index start = *cs->parent(*cs->parent());
  call_stack::frame_index end = *cs->current_frame_index();
  call_stack::frame_span middle = cs->frames(start, end);

  auto new_cs = make<call_stack>(ctx);
  new_cs->append_frames(middle);

  auto it = new_cs->frames_range().begin();
  EXPECT_EQ(new_cs->callable(*it++), three);
  EXPECT_EQ(new_cs->callable(*it++), two);
  EXPECT_EQ(it, new_cs->frames_range().end());
}

TEST_F(call_stack_fixture, overlapping_frames_can_access_shared_registers) {
  cs->push_frame(one, 0, 4, nullptr, 0);
  current_frame(cs.get()).local(2) = integer_to_ptr(10);
  current_frame(cs.get()).local(3) = integer_to_ptr(20);

  cs->push_frame(two, 2, 2, nullptr, 0);
  EXPECT_EQ(expect<integer>(current_frame(cs.get()).local(0)).value(), 10);
  EXPECT_EQ(expect<integer>(current_frame(cs.get()).local(1)).value(), 20);
}

TEST_F(call_stack_fixture, stack_size_increases_when_larger_frame_is_pushed) {
  cs->push_frame(one, 0, 4, nullptr, 0);
  EXPECT_EQ(cs->size(), 4);

  cs->push_frame(two, 2, 4, nullptr, 0);
  EXPECT_EQ(cs->size(), 6);

  cs->pop_frame();
  EXPECT_EQ(cs->size(), 4);

  cs->pop_frame();
  EXPECT_EQ(cs->size(), 0);
}

TEST_F(call_stack_fixture, stack_size_decreases_when_small_frame_is_pushed) {
  cs->push_frame(one, 0, 6, nullptr, 0);
  EXPECT_EQ(cs->size(), 6);

  cs->push_frame(two, 2, 2, nullptr, 0);
  EXPECT_EQ(cs->size(), 4);

  cs->pop_frame();
  EXPECT_EQ(cs->size(), 6);
}

TEST_F(call_stack_fixture, shrunk_stack_has_correct_capacity) {
  cs->push_frame(one, 0, 6, nullptr, 0);
  cs->push_frame(two, 2, 2, nullptr, 0);

  auto copy = make<call_stack>(ctx, *cs.get());
  copy->pop_frame();
  copy->local(5) = integer_to_ptr(1);

  copy->push_frame(three, 6, 10, nullptr, 0);
  copy->pop_frame();

  EXPECT_EQ(expect<integer>(copy->local(5)).value(), 1);
}
