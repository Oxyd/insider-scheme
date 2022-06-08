#include "compiler/expression.hpp"

#include "context.hpp"
#include "module.hpp"

namespace insider {

std::unique_ptr<expression>
make_internal_reference(context& ctx, std::string name) {
  std::optional<module_::binding_type> binding
    = ctx.internal_module()->find(ctx.intern(name));
  assert(binding);
  assert(binding->variable);
  assert(binding->variable->global);

  return make_expression<top_level_reference_expression>(
    *binding->variable->global,
    std::move(name)
  );
}

} // namespace insider
