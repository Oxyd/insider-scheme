#ifndef INSIDER_MODULE_HPP
#define INSIDER_MODULE_HPP

#include "compiler/compilation_config.hpp"
#include "compiler/module_specifier.hpp"
#include "compiler/scope.hpp"
#include "memory/free_store.hpp"
#include "memory/root.hpp"
#include "object.hpp"

#include <filesystem>
#include <optional>
#include <unordered_set>

namespace insider {

class context;
class procedure;
class symbol;
class vm;

// A module is a map from symbols to top-level variable indices. It also
// contains a top-level procedure which contains the code to be run when the
// module is loaded.
class module_ : public composite_object<module_> {
public:
  static constexpr char const* scheme_name = "insider::module";

  using binding_type = insider::scope::binding;

  enum class type {
    loaded,      // Loaded from a file; the default
    immutable,   // Immutable, returned by `environment`
    interactive  // Used by the REPL
  };

  explicit
  module_(context&, std::optional<module_name> = {});

  module_(context&, type);

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
  set_top_level_procedure(free_store& fs, ptr<procedure> const& p) {
    proc_ = p;
    fs.notify_arc(this, p);
  }

  ptr<insider::scope>
  scope() const { return env_; }

  bool
  active() const { return active_; }

  void
  mark_active() { active_ = true; }

  type
  get_type() const { return type_; }

  std::optional<module_name> const&
  name() const { return name_; }

  void
  visit_members(member_visitor const& f) const;

private:
  ptr<insider::scope>             env_;
  // Bindings available for export to other modules.
  std::unordered_set<std::string> exports_;
  ptr<procedure>                  proc_;
  bool                            active_ = false;
  type                            type_ = type::loaded;
  std::optional<module_name>      name_;
};

// Turn a module specifier into a module. First instantiate all uninstantiated
// dependencies of the module, then compile its body.
root_ptr<module_>
instantiate(context&, module_specifier const&, compilation_config const&);

// Import all exports from one module to another.
void
import_all_exported(context&,
                    root_ptr<module_> const& to,
                    root_ptr<module_> const& from);

// Import all top-level bindings (whether exported or not) from one module to
// another.
void
import_all_top_level(context&,
                     root_ptr<module_> const& to,
                     root_ptr<module_> const& from);

void
perform_imports(context&, root_ptr<module_> const& to, imports_list const&,
                compilation_config const&);

operand
define_top_level(context&, std::string const& name, ptr<module_>, bool export_,
                 ptr<> object);

operand
define_top_level_mutable(context&, std::string const& name, ptr<module_>,
                         bool export_, ptr<> object);

// Recursively activate all dependencies of the given module, execute the
// module's body and return the result of the last expression in its body.
//
// Causes garbage collection.
ptr<>
execute(vm&, root_ptr<module_> const&);

ptr<>
execute(context&, root_ptr<module_> const&);

root_ptr<module_>
make_interactive_module(context&, imports_list const&);

root_ptr<module_>
interaction_environment(context&);

} // namespace insider

#endif
