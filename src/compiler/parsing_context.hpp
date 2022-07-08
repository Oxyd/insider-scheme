#ifndef INSIDER_COMPILER_PARSING_CONTEXT_HPP
#define INSIDER_COMPILER_PARSING_CONTEXT_HPP

#include "compiler/variable.hpp"
#include "memory/root_provider.hpp"

#include <vector>

namespace insider {

class context;
class module_;
class scope;
class source_file_origin;

using use_site_scopes_list = std::vector<ptr<scope>>;

class parsing_context : public root_provider {
public:
  context& ctx;
  ptr<insider::module_> module_;
  source_file_origin const& origin;
  std::vector<std::vector<ptr<variable>>> environment;
  std::vector<use_site_scopes_list> use_site_scopes;

  parsing_context(context&, ptr<insider::module_>, source_file_origin const&);

  bool
  record_use_site_scopes() const {
    return !use_site_scopes.empty();
  }

  void
  visit_roots(member_visitor const&) override;
};

} // namespace insider

#endif
