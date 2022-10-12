#include "scheme_fixture.hpp"

#include "compiler/cfg.hpp"

using namespace insider;

struct cfg_fixture : scheme_fixture { };

static void
expect_cfg_equiv(cfg& g, std::vector<instruction> const& ref_instrs) {
  auto cfg_instrs = bytecode_to_instructions(analyse_and_compile_cfg(g).bc);
  EXPECT_EQ(cfg_instrs, ref_instrs);
}

TEST_F(cfg_fixture, single_cfg_block_to_bytecode) {
  cfg g(1);
  g[0].body.emplace_back(opcode::add, operand{0}, operand{1}, operand{0});
  g[0].body.emplace_back(opcode::add, operand{0}, operand{2}, operand{0});
  g[0].body.emplace_back(opcode::ret, operand{0});

  bytecode bc = analyse_and_compile_cfg(g).bc;
  std::vector<instruction> instrs = bytecode_to_instructions(bc);

  expect_cfg_equiv(
    g,
    {
      {opcode::add, operand{0}, operand{1}, operand{0}},
      {opcode::add, operand{0}, operand{2}, operand{0}},
      {opcode::ret, operand{0}}
    }
  );
}

TEST_F(cfg_fixture, compile_flow_off_into_another_block) {
  cfg g(2);
  g[0].body.emplace_back(opcode::add, operand{0}, operand{1}, operand{2});
  g[1].body.emplace_back(opcode::add, operand{3}, operand{4}, operand{5});

  expect_cfg_equiv(
    g,
    {
      {opcode::add, operand{0}, operand{1}, operand{2}},
      {opcode::add, operand{3}, operand{4}, operand{5}}
    }
  );
}

TEST_F(cfg_fixture, compile_unconditional_jump_backward) {
  cfg g(3);
  g[0].body.emplace_back(opcode::add, operand{0}, operand{0}, operand{0});
  g[1].body.emplace_back(opcode::add, operand{1}, operand{1}, operand{1});
  g[2].body.emplace_back(opcode::add, operand{2}, operand{2}, operand{2});
  g[2].ending = unconditional_jump{1};

  expect_cfg_equiv(
    g,
    {
      {opcode::add, operand{0}, operand{0}, operand{0}},  // 0
      {opcode::add, operand{1}, operand{1}, operand{1}},  // 4
      {opcode::add, operand{2}, operand{2}, operand{2}},  // 8
      {opcode::jump, immediate_to_operand(-10)}           // 12
                                                          // 14
    }
  );
}

TEST_F(cfg_fixture, compile_unconditional_jump_forward) {
  cfg g(3);
  g[0].body.emplace_back(opcode::add, operand{0}, operand{0}, operand{0});
  g[0].ending = unconditional_jump{2};
  g[1].body.emplace_back(opcode::add, operand{1}, operand{1}, operand{1});
  g[2].body.emplace_back(opcode::add, operand{2}, operand{2}, operand{2});
  g[2].ending = unconditional_jump{1};

  expect_cfg_equiv(
    g,
    {
      {opcode::add, operand{0}, operand{0}, operand{0}}, // 0
      {opcode::jump, immediate_to_operand(4)},           // 4
      {opcode::add, operand{1}, operand{1}, operand{1}}, // 6
      {opcode::add, operand{2}, operand{2}, operand{2}}, // 10
      {opcode::jump, immediate_to_operand(-10)}          // 14
                                                         // 16
    }
  );
}

TEST_F(cfg_fixture, compile_conditional_jump) {
  cfg g(3);
  g[0].body.emplace_back(opcode::add, operand{0}, operand{0}, operand{0});
  g[0].ending = conditional_jump{operand{0}, 2};
  g[1].body.emplace_back(opcode::add, operand{1}, operand{1}, operand{1});
  g[2].body.emplace_back(opcode::add, operand{2}, operand{2}, operand{2});

  expect_cfg_equiv(
    g,
    {
      {opcode::add, operand{0}, operand{0}, operand{0}},           // 0
      {opcode::jump_unless, operand{0}, immediate_to_operand(4)},  // 4
      {opcode::add, operand{1}, operand{1}, operand{1}},           // 7
      {opcode::add, operand{2}, operand{2}, operand{2}}            // 11
    }
  );
}

TEST_F(cfg_fixture, cfg_compilation_preserves_debug_info) {
  cfg g(2);
  g[0].body.emplace_back(opcode::add, operand{0}, operand{0}, operand{0}); // 0
  g[1].body.emplace_back(opcode::add, operand{1}, operand{1}, operand{0}); // 4
  g[1].body.emplace_back(opcode::add, operand{2}, operand{2}, operand{0}); // 8
  g[1].body.emplace_back(opcode::call, operand{1}, operand{0}, operand{1}); // 12
  g[1].debug_info[2].inlined_call_chain = {"foo", "bar"};

  auto [bc, di] = analyse_and_compile_cfg(g);
  EXPECT_EQ(di[12].inlined_call_chain, (std::vector<std::string>{"foo", "bar"}));
}

TEST_F(cfg_fixture, unreachable_blocks_are_not_emitted) {
  cfg g(3);
  g[0].body.emplace_back(opcode::add, operand{0}, operand{0}, operand{0});
  g[0].ending = unconditional_jump{2};
  g[1].body.emplace_back(opcode::add, operand{1}, operand{1}, operand{1});
  g[2].body.emplace_back(opcode::add, operand{2}, operand{2}, operand{2});

  expect_cfg_equiv(
    g,
    {
      {opcode::add, operand{0}, operand{0}, operand{0}},  // 0
      {opcode::jump, immediate_to_operand(0)},            // 4
      {opcode::add, operand{2}, operand{2}, operand{2}}   // 6
    }
  );
}

TEST_F(cfg_fixture, jumps_to_jumps_are_collapsed) {
  cfg g(5);
  g[0].body.emplace_back(opcode::add, operand{0}, operand{0}, operand{0});
  g[0].ending = conditional_jump{operand{0}, 2};

  g[1].body.emplace_back(opcode::add, operand{3}, operand{3}, operand{3});

  g[2].ending = unconditional_jump{4};

  g[3].body.emplace_back(opcode::add, operand{2}, operand{2}, operand{2});

  g[4].body.emplace_back(opcode::add, operand{1}, operand{1}, operand{1});
  g[4].ending = conditional_jump{operand{0}, 3};

  expect_cfg_equiv(
    g,
    {
      {opcode::add, operand{0}, operand{0}, operand{0}},            // 0
      {opcode::jump_unless, operand{0}, immediate_to_operand(10)},  // 4

      {opcode::add, operand{3}, operand{3}, operand{3}},            // 7

      {opcode::jump, immediate_to_operand(4)},                      // 11

      {opcode::add, operand{2}, operand{2}, operand{2}},            // 13

      {opcode::add, operand{1}, operand{1}, operand{1}},            // 17
      {opcode::jump_unless, operand{0}, immediate_to_operand(-11)}  // 21
                                                                    // 24
    }
  );
}

TEST_F(cfg_fixture,
       blocks_that_are_skipped_due_to_jump_collapsing_are_not_emitted) {
  cfg g(4);
  g[0].body.emplace_back(opcode::add, operand{0}, operand{0}, operand{0});
  g[0].ending = unconditional_jump{1};
  g[1].ending = unconditional_jump{3};
  g[2].body.emplace_back(opcode::add, operand{1}, operand{1}, operand{1});
  g[3].body.emplace_back(opcode::add, operand{2}, operand{2}, operand{2});
  g[3].ending = unconditional_jump{2};

  expect_cfg_equiv(
    g,
    {
      {opcode::add, operand{0}, operand{0}, operand{0}},  // 0
      {opcode::jump, immediate_to_operand(4)},            // 4
      {opcode::add, operand{1}, operand{1}, operand{1}},  // 6
      {opcode::add, operand{2}, operand{2}, operand{2}},  // 10
      {opcode::jump, immediate_to_operand(-10)}           // 14
                                                          // 16
    }
  );
}

TEST_F(cfg_fixture, jumps_to_rets_are_collapsed) {
  cfg g(4);
  g[0].body.emplace_back(opcode::add, operand{0}, operand{0}, operand{0});
  g[0].ending = conditional_jump{operand{0}, 2};
  g[1].body.emplace_back(opcode::add, operand{1}, operand{1}, operand{1});
  g[1].ending = unconditional_jump{3};
  g[2].body.emplace_back(opcode::add, operand{2}, operand{2}, operand{2});
  g[3].body.emplace_back(opcode::ret, operand{0});

  expect_cfg_equiv(
    g,
    {
      {opcode::add, operand{0}, operand{0}, operand{0}},           // 0
      {opcode::jump_unless, operand{0}, immediate_to_operand(6)},  // 4
      {opcode::add, operand{1}, operand{1}, operand{1}},           // 7
      {opcode::ret, operand{0}},                                   // 11
      {opcode::add, operand{2}, operand{2}, operand{2}},           // 13
      {opcode::ret, operand{0}}                                    // 17
    }
  );
}

TEST_F(cfg_fixture, code_after_tail_calls_is_pruned) {
  cfg g(1);
  g[0].body.emplace_back(opcode::tail_call, operand{0}, operand{0});
  g[0].body.emplace_back(opcode::ret, operand{0});

  expect_cfg_equiv(
    g,
    {
      {opcode::tail_call, operand{0}, operand{0}}
    }
  );
}

TEST_F(cfg_fixture, block_whose_predecesors_end_in_ret_is_pruned) {
  cfg g(2);
  g[0].body.emplace_back(opcode::ret, operand{0});
  g[1].body.emplace_back(opcode::add, operand{0}, operand{0}, operand{0});

  expect_cfg_equiv(
    g,
    {
      {opcode::ret, operand{0}}
    }
  );
}

TEST_F(cfg_fixture, jump_to_empty_block_followed_by_ret_is_collapsed) {
  cfg g(3);
  g[0].ending = unconditional_jump{1};
  g[2].body.emplace_back(opcode::ret, operand{0});

  expect_cfg_equiv(
    g,
    {
      {opcode::ret, operand{0}}
    }
  );
}

TEST_F(cfg_fixture, jump_after_tail_call_is_pruned) {
  cfg g(2);
  g[0].body.emplace_back(opcode::tail_call, operand{0}, operand{0});
  g[0].ending = unconditional_jump{1};

  expect_cfg_equiv(
    g,
    {
      {opcode::tail_call, operand{0}, operand{0}}
    }
  );
}

TEST_F(cfg_fixture, jump_to_following_block_is_removed) {
  cfg g(3);
  g[0].body.emplace_back(opcode::add, operand{0}, operand{0}, operand{0});
  g[0].ending = unconditional_jump{1};
  g[1].body.emplace_back(opcode::add, operand{1}, operand{1}, operand{1});
  g[1].ending = conditional_jump{operand{0}, 2};
  g[2].body.emplace_back(opcode::add, operand{2}, operand{2}, operand{2});

  expect_cfg_equiv(
    g,
    {
      {opcode::add, operand{0}, operand{0}, operand{0}},
      {opcode::add, operand{1}, operand{1}, operand{1}},
      {opcode::add, operand{2}, operand{2}, operand{2}},
    }
  );
}

TEST_F(cfg_fixture, jump_that_collapses_to_the_following_block_is_removed) {
  cfg g(3);
  g[0].body.emplace_back(opcode::add, operand{0}, operand{0}, operand{0});
  g[0].ending = unconditional_jump{2};
  g[1].body.emplace_back(opcode::add, operand{1}, operand{1}, operand{1});
  g[2].ending = unconditional_jump{1};

  expect_cfg_equiv(
    g,
    {
      {opcode::add, operand{0}, operand{0}, operand{0}},  // 0
      {opcode::add, operand{1}, operand{1}, operand{1}},  // 4
      {opcode::jump, immediate_to_operand(-6)}            // 8
                                                          // 10
    }
  );
}

TEST_F(cfg_fixture, pointless_jump_to_end_from_unreachable_block_is_removed) {
  cfg g(6);
  g[0].ending = conditional_jump{operand{0}, 3};
  g[1].body.emplace_back(opcode::add, operand{0}, operand{0}, operand{0});
  g[2].ending = unconditional_jump{4};
  g[3].body.emplace_back(opcode::tail_call, operand{0}, operand{0});

  expect_cfg_equiv(
    g,
    {
      {opcode::jump_unless, operand{0}, immediate_to_operand(4)},  // 0
      {opcode::add, operand{0}, operand{0}, operand{0}},           // 3
      {opcode::tail_call, operand{0}, operand{0}}                  // 7
    }
  );
}
