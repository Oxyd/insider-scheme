#ifndef INSIDER_VM_HPP
#define INSIDER_VM_HPP

#include "scheme.hpp"

#include <limits>

namespace insider {

// The virtual machine. The global environment is represented as a procedure
// whose call frame is at the top of the call stack. Global variables and
// procedures are stored as the top-level call frame's statics and locals.
//
// TODO: This entire thing should cooperate with the free store directly,
// instead of using the smart pointer types.

class call_frame : public dynamic_size_object<call_frame, object*> {
public:
  static std::size_t
  extra_elements(ptr<insider::procedure> const& procedure,
                 ptr<call_frame> const& parent,
                 std::vector<generic_ptr> const& closure,
                 std::vector<generic_ptr> const& arguments);

  std::uint32_t pc = 0;

  call_frame(ptr<insider::procedure> const& procedure,
             ptr<call_frame> const& parent,
             std::vector<generic_ptr> const& closure,
             std::vector<generic_ptr> const& arguments);

  call_frame(call_frame&&);

  std::size_t
  size() const { return locals_size_; }

  void
  trace(tracing_context&);

  void
  update_references();

  ptr<insider::procedure>
  procedure(free_store& store) const { return {store, procedure_}; }

  generic_ptr
  local(free_store& store, std::size_t i) const {
    assert(i < locals_size_);
    return {store, storage_element(i)};
  }

  void
  set_local(free_store& store, std::size_t i, generic_ptr const& value);

  ptr<call_frame>
  parent(free_store& store) const { return {store, parent_frame_}; }

  operand
  dest_register() const { return dest_register_; }

  void
  set_dest_register(operand d) { dest_register_ = d; }

private:
  insider::procedure* procedure_;
  call_frame*         parent_frame_;
  std::size_t         locals_size_;
  operand             dest_register_ = std::numeric_limits<operand>::max();
};

inline ptr<procedure>
call_frame_procedure(ptr<call_frame> const& cf) { return cf->procedure(cf.store()); }

inline generic_ptr
call_frame_local(ptr<call_frame> const& cf, std::size_t i) {
  return cf->local(cf.store(), i);
}

inline void
call_frame_set_local(ptr<call_frame> const& cf, std::size_t i, generic_ptr const& value) {
  cf->set_local(cf.store(), i, value);
}

inline ptr<call_frame>
call_frame_parent(ptr<call_frame> const& cf) { return cf->parent(cf.store()); }

struct execution_state {
  context&                 ctx;
  ptr<call_frame>          root_frame;
  ptr<call_frame>          current_frame;
  generic_ptr              global_return;
  std::vector<generic_ptr> argument_stack;
};

// Make execution state using the given procedure as the global call frame.
execution_state
make_state(context&, ptr<procedure> const&);

// Run the bytecode in the execution context's global call frame.
generic_ptr
run(execution_state&);

// Create a new execution state with the given procedure as the root frame,
// execute it, and return the procedure's return value.
generic_ptr
call(context&, generic_ptr callable, std::vector<generic_ptr> const& arguments);

} // namespace insider

#endif
