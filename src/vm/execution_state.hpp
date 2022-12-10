#ifndef INSIDER_VM_EXECUTION_STATE_HPP
#define INSIDER_VM_EXECUTION_STATE_HPP

#include "memory/root_provider.hpp"
#include "vm/bytecode.hpp"
#include "vm/call_stack.hpp"

namespace insider {

class context;

class execution_state : public root_provider {
public:
  context&            ctx;
  instruction_pointer ip{};
  call_stack          stack;
  ptr<>               result;

  execution_state(context& ctx);

private:
  void
  visit_roots(member_visitor const&) override;
};

} // namespace insider

#endif
