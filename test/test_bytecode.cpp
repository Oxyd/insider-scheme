#include "vm/bytecode.hpp"

#include <gtest/gtest.h>

using namespace insider;

TEST(bytecode, instruction_info_consistency) {
  EXPECT_EQ(opcode_to_info(opcode::add).opcode, opcode::add);
  EXPECT_EQ(opcode_to_info(opcode::add).mnemonic, "add");

  EXPECT_EQ(mnemonic_to_info("add").mnemonic, "add");
  EXPECT_EQ(mnemonic_to_info("add").opcode, opcode::add);
}
