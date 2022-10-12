#include "vm/execution_state.hpp"

#include "context.hpp"
#include "vm/call_stack.hpp"

namespace insider {

execution_state::execution_state(context& ctx)
  : root_provider{ctx.store}
  , ctx{ctx}
  , stack{make<call_stack>(ctx)}
{
  ctx.store.make_permanent_arc(stack);
}

void
execution_state::visit_roots(member_visitor const& f) {
  f(stack);
  f(result);
}

} // namespace insider
