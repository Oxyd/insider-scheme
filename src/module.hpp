#ifndef INSIDER_MODULE_HPP
#define INSIDER_MODULE_HPP

#include "module_name.hpp"
#include "syntax.hpp"

#include <filesystem>

namespace insider {

class procedure;

// A module is a map from symbols to top-level variable indices. It also
// contains a top-level procedure which contains the code to be run when the
// module is loaded.
class module_ {
public:
  using binding_type = insider::scope::value_type;

  explicit
  module_(context&, std::optional<module_name> const& = {});

  std::optional<binding_type>
  find(ptr<symbol>) const;

  void
  import_(context&, ptr<symbol>, binding_type);

  void
  export_(ptr<symbol> name);

  std::unordered_set<std::string> const&
  exports() const { return exports_; }

  ptr<procedure>
  top_level_procedure() const;

  void
  set_top_level_procedure(tracked_ptr<procedure> const& p) { proc_ = p; }

  ptr<insider::scope>
  scope() const { return env_.get(); }

  std::vector<std::string>
  top_level_names() const;

  bool
  active() const { return active_; }

  void
  mark_active() { active_ = true; }

private:
  tracked_ptr<insider::scope>     env_;
  std::unordered_set<std::string> exports_; // Bindings available for export to other modules.
  tracked_ptr<procedure>          proc_;
  bool                            active_ = false;
};

// Turn a protomodule into a module. First instantiate all uninstantiated
// dependencies of the protomodule, then compile its body.
std::unique_ptr<module_>
instantiate(context&, protomodule const&);

// Import all exports from one module to another.
void
import_all_exported(context&, module_& to, module_& from);

// Import all top-level bindings (whether exported or not) from one module to another.
void
import_all_top_level(context&, module_& to, module_& from);

// Given a protomodule, go through all of its import declarations and perform
// them in the given module.
void
perform_imports(context&, module_& to, protomodule const& import_declarations);

operand
define_top_level(context&, std::string const& name, module_&, bool export_, ptr<> object);

// Recursively activate all dependencies of the given module, execute the
// module's body and return the result of the last expression in its body.
//
// Causes garbage collection.
tracked_ptr<>
execute(context&, module_&);

} // namespace insider

#endif
