#include "parsing_context.hpp"

#include "compiler/compilation_config.hpp"
#include "vm/vm.hpp"

namespace insider {

parsing_context::parsing_context(vm& state, ptr<insider::module_> m,
                                 compilation_config const& config,
                                 source_file_origin const& origin)
  : root_provider{state.ctx.store.root_list()}
  , ctx{state.ctx}
  , state{state}
  , module_{m}
  , origin{origin}
  , config{config}
{ }

void
parsing_context::visit_roots(member_visitor const& f) {
  f(module_);

  for (auto& vars : environment)
    for (variable& var : vars)
      var.visit_members(f);

  for (use_site_scopes_list& uss : use_site_scopes)
    for (ptr<scope>& s : uss)
      f(s);
}

} // namespace insider
