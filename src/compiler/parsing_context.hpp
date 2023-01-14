#ifndef INSIDER_COMPILER_PARSING_CONTEXT_HPP
#define INSIDER_COMPILER_PARSING_CONTEXT_HPP

#include "compiler/variable.hpp"
#include "memory/root_provider.hpp"

#include <vector>

namespace insider {

class context;
class module_;
class scope;
class vm;
struct source_file_origin;

class parsing_context;

using use_site_scopes_list = std::vector<ptr<scope>>;
using pass = expression (*)(parsing_context&, expression);
using pass_list = std::vector<pass>;

class parsing_context : public root_provider {
public:
  context&                           ctx;
  vm&                                state;
  ptr<insider::module_>              module_;
  source_file_origin const&          origin;
  std::vector<std::vector<variable>> environment;
  std::vector<use_site_scopes_list>  use_site_scopes;
  pass_list                          passes;
  bool                               is_meta = false;

  parsing_context(vm& state, ptr<insider::module_>, pass_list passes,
                  source_file_origin const&);

  bool
  record_use_site_scopes() const {
    return !use_site_scopes.empty();
  }

  void
  visit_roots(member_visitor const&) override;
};

} // namespace insider

#endif
