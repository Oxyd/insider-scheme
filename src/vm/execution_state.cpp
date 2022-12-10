#include "vm/execution_state.hpp"

#include "context.hpp"
#include "vm/call_stack.hpp"

namespace insider {

execution_state::execution_state(context& ctx)
  : root_provider{ctx.store}
  , ctx{ctx}
{ }

void
execution_state::visit_roots(member_visitor const& f) {
  stack.visit_members(f);
  f(result);
}

} // namespace insider
