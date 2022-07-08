#include "parsing_context.hpp"

#include "context.hpp"

namespace insider {

parsing_context::parsing_context(context& ctx, ptr<insider::module_> m,
                                 source_file_origin const& origin)
  : root_provider{ctx.store}
  , ctx{ctx}
  , module_{m}
  , origin{origin}
{ }

void
parsing_context::visit_roots(member_visitor const& f) {
  f(module_);

  for (auto& vars : environment)
    for (ptr<>& var : vars)
      f(var);

  for (use_site_scopes_list& uss : use_site_scopes)
    for (ptr<scope>& s : uss)
      f(s);
}

} // namespace insider
