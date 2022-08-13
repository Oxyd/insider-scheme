#include "compiler/variable.hpp"

#include "memory/free_store.hpp"
#include "module.hpp"
#include "util/define_struct.hpp"

namespace insider {

void
variable_base::set_constant_initialiser(free_store& fs, ptr<> self,
                                        expression e) {
  constant_initialiser_ = e;
  fs.notify_arc(self, e.get());
}

static ptr<>
local_variable_constant_initialiser(context& ctx, ptr<local_variable> v) {
  if (auto ci = v->constant_initialiser())
    return ci.get();
  else
    return ctx.constants->f;
}

static ptr<>
top_level_variable_constant_initialiser(context& ctx,
                                        ptr<top_level_variable> v) {
  if (auto ci = v->constant_initialiser())
    return ci.get();
  else
    return ctx.constants->f;
}

void
export_variable(context& ctx, ptr<module_> result) {
  define_struct<local_variable>(ctx, "local-variable", result)
    .field<&local_variable::name>("name")
    .field<&local_variable::is_set>("set?")
    .field<&local_variable_constant_initialiser>("constant-initialiser")
    ;

  define_struct<local_variable>(ctx, "top-level-variable", result)
    .field<&top_level_variable::name>("name")
    .field<&top_level_variable::is_set>("set?")
    .field<&top_level_variable_constant_initialiser>("constant-initialiser")
    ;
}

} // namespace insider
