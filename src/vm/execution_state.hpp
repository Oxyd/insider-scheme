#ifndef INSIDER_VM_EXECUTION_STATE_HPP
#define INSIDER_VM_EXECUTION_STATE_HPP

#include "memory/root_provider.hpp"
#include "runtime/integer.hpp"

namespace insider {

class call_stack;
class context;

class execution_state : public root_provider {
public:
  context&            ctx;
  integer::value_type pc = -1;
  ptr<call_stack>     stack;

  execution_state(context& ctx);

private:
  void
  visit_roots(member_visitor const&) override;
};

} // namespace insider

#endif
