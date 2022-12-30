#include "parsing_context.hpp"

#include "vm/vm.hpp"

namespace insider {

parsing_context::parsing_context(vm& state, ptr<insider::module_> m,
                                 pass_list passes,
                                 source_file_origin const& origin)
  : root_provider{state.ctx.store}
  , ctx{state.ctx}
  , state{state}
  , module_{m}
  , origin{origin}
  , passes{std::move(passes)}
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
