#ifndef INSIDER_COMPILER_PARSING_CONTEXT_HPP
#define INSIDER_COMPILER_PARSING_CONTEXT_HPP

#include "compiler/variable.hpp"
#include "memory/tracked_ptr.hpp"

#include <vector>

namespace insider {

class context;
class module_;
class scope;
class source_file_origin;

using use_site_scopes_list = std::vector<tracked_ptr<scope>>;

struct parsing_context {
  context& ctx;
  tracked_ptr<insider::module_> module_;
  source_file_origin const& origin;
  std::vector<std::vector<std::shared_ptr<variable>>> environment;
  std::vector<use_site_scopes_list> use_site_scopes;

  bool
  record_use_site_scopes() const {
    return !use_site_scopes.empty();
  }
};

} // namespace insider

#endif
