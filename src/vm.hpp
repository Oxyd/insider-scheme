#ifndef SCHEME_VM_HPP
#define SCHEME_VM_HPP

#include "scheme.hpp"

namespace game::scm {

// The virtual machine. The global environment is represented as a procedure
// whose call frame is at the top of the call stack. Global variables and
// procedures are stored as the top-level call frame's statics and locals.
//
// TODO: This entire thing should cooperate with the free store directly,
// instead of using the smart pointer types.

class call_frame : public dynamic_size_object<call_frame, object*> {
public:
  static std::size_t
  extra_storage_size(ptr<scm::procedure> const& procedure,
                     ptr<scm::closure> const& closure,
                     ptr<call_frame> const& parent,
                     std::vector<generic_ptr> const& arguments,
                     ptr<scm::module> const& m);

  std::uint32_t pc = 0;

  call_frame(ptr<scm::procedure> const& procedure,
             ptr<scm::closure> const& closure,
             ptr<call_frame> const& parent,
             std::vector<generic_ptr> const& arguments,
             ptr<scm::module> const& m);

  void
  for_each_subobject(std::function<void(object*)> const& f) override;

  ptr<scm::procedure>
  procedure() const { return procedure_; }

  generic_ptr
  local(std::size_t i) const;
  void
  set_local(std::size_t i, generic_ptr const& value);

  generic_ptr
  closure(std::size_t i) const;

  ptr<call_frame>
  parent() const { return parent_frame_; }

  ptr<scm::module>
  module() const { return module_; }

private:
  scm::procedure* procedure_;
  scm::closure*   closure_;
  call_frame*     parent_frame_;
  std::size_t     locals_size_;
  scm::module*    module_;
};

struct execution_state {
  context&        ctx;
  ptr<call_frame> root_frame;
  ptr<call_frame> current_frame;
  generic_ptr     global_return;
};

// Make execution state using the given procedure as the global call frame.
execution_state
make_state(context&, ptr<procedure> const&, ptr<module> const&);

// Run the bytecode in the execution context's global call frame.
generic_ptr
run(execution_state&);

} // namespace game::scm

#endif
