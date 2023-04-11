#include "scheme_fixture.hpp"

#include "vm/call_stack.hpp"

using namespace insider;

struct call_stack_fixture : scheme_fixture {
  call_stack cs;
  ptr<procedure> one = make_dummy_procedure();
  ptr<procedure> two = make_dummy_procedure();
  ptr<procedure> three = make_dummy_procedure();
  ptr<procedure> four = make_dummy_procedure();
  ptr<procedure> ten = make_dummy_procedure();
  ptr<procedure> twenty = make_dummy_procedure();

  void
  make_4_frames() {
    cs.push_frame({
     .type = call_stack::frame_type::scheme,
     .result_register = 0,
     .base = cs.size(),
     .size = 1,
     .previous_ip = 0,
     .extra = {}
    });
    cs.local(0) = one;
    cs.push_frame({
     .type = call_stack::frame_type::scheme,
     .result_register = 0,
     .base = cs.size(),
     .size = 1,
     .previous_ip = 0,
     .extra = {}
    });
    cs.local(0) = two;
    cs.push_frame({
     .type = call_stack::frame_type::scheme,
     .result_register = 0,
     .base = cs.size(),
     .size = 1,
     .previous_ip = 0,
     .extra = {}
    });
    cs.local(0) = three;
    cs.push_frame({
     .type = call_stack::frame_type::scheme,
     .result_register = 0,
     .base = cs.size(),
     .size = 1,
     .previous_ip = 0,
     .extra = {}
    });
    cs.local(0) = four;
  }

  ptr<procedure>
  make_dummy_procedure() {
    return make<procedure>(
      ctx,
      make<procedure_prototype>(
        ctx,
        mutable_bytecode{},
        procedure_prototype::meta{
          .locals_size = 0,
          .num_required_args = 0,
          .num_leading_args = 0,
          .has_rest = false,
          .closure_size = 0,
          .parameter_names = std::make_unique<ptr<keyword>[]>(0),
          .name = "<dummy>",
          .debug_info = debug_info_map{}
        },
        std::vector<ptr<>>{}
      ),
      0
    );
  }
};

TEST_F(call_stack_fixture, create_frame) {
  cs.push_frame({
    .type = call_stack::frame_type::scheme,
    .result_register = 0,
    .base = cs.size(),
    .size = 1,
    .previous_ip = nullptr,
    .extra = {}
  });
  cs.local(0) = one;
  EXPECT_EQ(cs.callable(), one);
}

TEST_F(call_stack_fixture, pop_frame) {
  cs.push_frame({
    .type = call_stack::frame_type::scheme,
    .result_register = 0,
    .base = cs.size(),
    .size = 1,
    .previous_ip = nullptr,
    .extra = {}
  });
  cs.local(0) = one;
  cs.push_frame({
    .type = call_stack::frame_type::scheme,
    .result_register = 0,
    .base = cs.size(),
    .size = 1,
    .previous_ip = nullptr,
    .extra = {}
  });
  cs.local(0) = two;
  EXPECT_EQ(cs.callable(), two);

  cs.pop_frame();
  EXPECT_EQ(cs.callable(), one);
}

TEST_F(call_stack_fixture, push_after_pop) {
  cs.push_frame({
    .type = call_stack::frame_type::scheme,
    .result_register = 0,
    .base = cs.size(),
    .size = 1,
    .previous_ip = nullptr,
    .extra = {}
  });
  cs.local(0) = one;
  cs.push_frame({
    .type = call_stack::frame_type::scheme,
    .result_register = 0,
    .base = cs.size(),
    .size = 1,
    .previous_ip = nullptr,
    .extra = {}
  });
  cs.local(0) = two;
  cs.pop_frame();
  cs.push_frame({
    .type = call_stack::frame_type::scheme,
    .result_register = 0,
    .base = cs.size(),
    .size = 1,
    .previous_ip = nullptr,
    .extra = {}
  });
  cs.local(0) = three;

  EXPECT_EQ(cs.callable(), three);
  cs.pop_frame();
  EXPECT_EQ(cs.callable(), one);
}

TEST_F(call_stack_fixture, new_call_stack_is_empty) {
  EXPECT_TRUE(cs.empty());
}

TEST_F(call_stack_fixture, call_stack_is_empty_after_popping_all_frames) {
  cs.push_frame({
    .type = call_stack::frame_type::scheme,
    .result_register = 0,
    .base = cs.size(),
    .size = 1,
    .previous_ip = nullptr,
    .extra = {}
  });
  cs.push_frame({
    .type = call_stack::frame_type::scheme,
    .result_register = 0,
    .base = cs.size(),
    .size = 1,
    .previous_ip = nullptr,
    .extra = {}
  });
  EXPECT_FALSE(cs.empty());

  cs.pop_frame();
  EXPECT_FALSE(cs.empty());

  cs.pop_frame();
  EXPECT_TRUE(cs.empty());
}

TEST_F(call_stack_fixture, new_frame_has_no_extra_data) {
  cs.push_frame({
    .type = call_stack::frame_type::scheme,
    .result_register = 0,
    .base = cs.size(),
    .size = 1,
    .previous_ip = nullptr,
    .extra = {}
  });
  EXPECT_EQ(cs.extra(), ptr<>{});
}

TEST_F(call_stack_fixture, can_set_and_retreive_extra_data) {
  cs.push_frame({
    .type = call_stack::frame_type::scheme,
    .result_register = 0,
    .base = cs.size(),
    .size = 1,
    .previous_ip = nullptr,
    .extra = {}
  });

  auto e = make<stack_frame_extra_data>(ctx);
  cs.set_extra(e);
  EXPECT_EQ(cs.extra(), e);
}

TEST_F(call_stack_fixture, can_set_frame_locals) {
  cs.push_frame({
    .type = call_stack::frame_type::scheme,
    .result_register = 0,
    .base = cs.size(),
    .size = 1,
    .previous_ip = nullptr,
    .extra = {}
  });
  cs.local(0) = integer_to_ptr(2);
  EXPECT_EQ(cs.local(0), integer_to_ptr(2));
}

TEST_F(call_stack_fixture, get_local_of_parent_frame) {
  cs.push_frame({
    .type = call_stack::frame_type::scheme,
    .result_register = 0,
    .base = cs.size(),
    .size = 1,
    .previous_ip = nullptr,
    .extra = {}
  });
  cs.local(0) = integer_to_ptr(2);

  cs.push_frame({
    .type = call_stack::frame_type::scheme,
    .result_register = 0,
    .base = cs.size(),
    .size = 1,
    .previous_ip = nullptr,
    .extra = {}
  });
  cs.local(0) = integer_to_ptr(4);

  EXPECT_EQ(expect<integer>(cs.local(*cs.parent(), 0)).value(), 2);
}

TEST_F(call_stack_fixture, iterate_call_stacks) {
  cs.push_frame({
    .type = call_stack::frame_type::scheme,
    .result_register = 0,
    .base = cs.size(),
    .size = 2,
    .previous_ip = nullptr,
    .extra = {}
  });
  cs.local(0) = one;
  cs.local(1) = integer_to_ptr(1);

  cs.push_frame({
    .type = call_stack::frame_type::scheme,
    .result_register = 0,
    .base = cs.size(),
    .size = 2,
    .previous_ip = nullptr,
    .extra = {}
  });
  cs.local(0) = two;
  cs.local(1) = integer_to_ptr(2);

  for (call_stack::frame_index f : cs.frames_range()) {
    if (cs.callable(f) == one)
      EXPECT_EQ(expect<integer>(cs.local(f, 1)).value(), 1);
    else
      EXPECT_EQ(expect<integer>(cs.local(f, 1)).value(), 2);
  }
}

TEST_F(call_stack_fixture, overlapping_frames_can_access_shared_registers) {
  cs.push_frame({
    .type = call_stack::frame_type::scheme,
    .result_register = 0,
    .base = 0,
    .size = 4,
    .previous_ip = nullptr,
    .extra = {}
  });
  current_frame(cs).local(2) = integer_to_ptr(10);
  current_frame(cs).local(3) = integer_to_ptr(20);

  cs.push_frame({
    .type = call_stack::frame_type::scheme,
    .result_register = 0,
    .base = 2,
    .size = 2,
    .previous_ip = nullptr,
    .extra = {}
  });
  EXPECT_EQ(expect<integer>(current_frame(cs).local(0)).value(), 10);
  EXPECT_EQ(expect<integer>(current_frame(cs).local(1)).value(), 20);
}

TEST_F(call_stack_fixture, stack_size_increases_when_larger_frame_is_pushed) {
  cs.push_frame({
    .type = call_stack::frame_type::scheme,
    .result_register = 0,
    .base = 0,
    .size = 4,
    .previous_ip = nullptr,
    .extra = {}
  });
  EXPECT_EQ(cs.size(), 4);

  cs.push_frame({
    .type = call_stack::frame_type::scheme,
    .result_register = 0,
    .base = 2,
    .size = 4,
    .previous_ip = nullptr,
    .extra = {}
  });
  EXPECT_EQ(cs.size(), 6);

  cs.pop_frame();
  EXPECT_EQ(cs.size(), 4);

  cs.pop_frame();
  EXPECT_EQ(cs.size(), 0);
}

TEST_F(call_stack_fixture, stack_size_decreases_when_small_frame_is_pushed) {
  cs.push_frame({
    .type = call_stack::frame_type::scheme,
    .result_register = 0,
    .base = 0,
    .size = 6,
    .previous_ip = nullptr,
    .extra = {}
  });
  EXPECT_EQ(cs.size(), 6);

  cs.push_frame({
    .type = call_stack::frame_type::scheme,
    .result_register = 0,
    .base = 2,
    .size = 2,
    .previous_ip = nullptr,
    .extra = {}
  });
  EXPECT_EQ(cs.size(), 4);

  cs.pop_frame();
  EXPECT_EQ(cs.size(), 6);
}

TEST_F(call_stack_fixture, shrunk_stack_has_correct_capacity) {
  cs.push_frame({
    .type = call_stack::frame_type::scheme,
    .result_register = 0,
    .base = 0,
    .size = 6,
    .previous_ip = nullptr,
    .extra = {}
  });
  cs.push_frame({
    .type = call_stack::frame_type::scheme,
    .result_register = 0,
    .base = 2,
    .size = 2,
    .previous_ip = nullptr,
    .extra = {}
  });

  call_stack copy{cs};
  copy.pop_frame();
  copy.local(5) = integer_to_ptr(1);

  copy.push_frame({
    .type = call_stack::frame_type::scheme,
    .result_register = 0,
    .base = 6,
    .size = 10,
    .previous_ip = nullptr,
    .extra = {}
  });
  copy.pop_frame();

  EXPECT_EQ(expect<integer>(copy.local(5)).value(), 1);
}
