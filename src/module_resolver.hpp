#ifndef INSIDER_MODULE_RESOLVER_HPP
#define INSIDER_MODULE_RESOLVER_HPP

#include "compiler/module_name.hpp"
#include "module.hpp"

#include <map>

namespace insider {

class context;
class source_code_provider;

class module_resolver {
public:
  explicit
  module_resolver(context&);

  module_&
  internal_module() { return internal_module_; }

  bool
  knows_module(context&, module_name const&);

  module_*
  find_module(context&, module_name const&);

  void
  prepend_source_code_provider(std::unique_ptr<source_code_provider>);

  void
  append_source_code_provider(std::unique_ptr<source_code_provider>);

private:
  // (insider internal)
  module_                                            internal_module_;
  std::map<module_name, std::unique_ptr<module_>>    modules_;
  std::vector<std::unique_ptr<source_code_provider>> source_providers_;

  bool
  any_provider_has_module(context& ctx, module_name const& name);

  module_specifier
  find_module_in_providers(context& ctx, module_name const& name);

  module_*
  load_module(context&, module_name const&);
};

} // insider

#endif
