#ifndef INSIDER_MODULE_RESOLVER_HPP
#define INSIDER_MODULE_RESOLVER_HPP

#include "compiler/module_name.hpp"
#include "compiler/module_specifier.hpp"
#include "memory/root_provider.hpp"
#include "module.hpp"

#include <map>

namespace insider {

class context;
class source_code_provider;

class module_resolver : root_provider {
public:
  explicit
  module_resolver(context&);

  ptr<module_>
  internal_module() { return internal_module_; }

  void
  set_internal_module(ptr<module_> im) { internal_module_ = im; }

  bool
  knows_module(context&, module_name const&);

  ptr<module_>
  find_module(context&, module_name const&);

  void
  prepend_source_code_provider(std::unique_ptr<source_code_provider>);

  void
  append_source_code_provider(std::unique_ptr<source_code_provider>);

private:
  // (insider internal)
  ptr<module_>                                       internal_module_;
  std::map<module_name, ptr<module_>>                modules_;
  std::vector<std::unique_ptr<source_code_provider>> source_providers_;

  void
  visit_roots(member_visitor const&) override;

  bool
  any_provider_has_module(context& ctx, module_name const& name);

  module_specifier
  find_module_in_providers(context& ctx, module_name const& name);

  ptr<module_>
  load_module(context&, module_name const&);
};

} // insider

#endif
