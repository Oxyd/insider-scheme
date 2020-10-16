#ifndef INSIDER_VM_HPP
#define INSIDER_VM_HPP

#include "scheme.hpp"

#include <limits>

namespace insider {

// Dynamic-sized stack to store local variables.
class root_stack : public composite_root_object<root_stack> {
public:
  generic_ptr
  ref(free_store& store, std::size_t i) { return {store, data_[i]}; }

  void
  set(free_store&, std::size_t i, generic_ptr const& value) {
    data_[i] = value.get();
  }

  void
  grow(std::size_t n) { data_.resize(data_.size() + n); }

  void
  shrink(std::size_t n) { data_.resize(data_.size() - n); }

  void
  erase(std::size_t begin, std::size_t end);

  std::size_t
  size() const { return data_.size(); }

  void
  trace(tracing_context&);

  void
  update_references();

private:
  std::vector<object*> data_;
};

inline generic_ptr
stack_ref(ptr<root_stack> const& s, std::size_t i) { return s->ref(s.store(), i); }

inline void
stack_set(ptr<root_stack> const& s, std::size_t i, generic_ptr const& value) {
  s->set(s.store(), i, value);
}

class call_frame {
public:
  bytecode_decoder        bytecode;
  ptr<insider::procedure> procedure;
  std::size_t             stack_top;
  operand                 dest_register = std::numeric_limits<operand>::max();

  call_frame(ptr<insider::procedure> const& proc, std::size_t stack_top)
    : bytecode{proc->bytecode}
    , procedure{proc}
    , stack_top{stack_top}
  { }
};

struct execution_state {
  context&                ctx;
  ptr<root_stack>              value_stack;
  std::vector<call_frame> call_stack;
  generic_ptr             global_return;

  execution_state(context& ctx)
    : ctx{ctx}
    , value_stack{make<root_stack>(ctx)}
  { }
};

generic_ptr
call_frame_local(execution_state& state, operand local);

// Make execution state using the given procedure as the global call frame.
execution_state
make_state(context&, ptr<procedure> const&,
           std::vector<generic_ptr> const& closure = {}, std::vector<generic_ptr> const& arguments = {});

// Run the bytecode in the execution context's global call frame.
generic_ptr
run(execution_state&);

// Create a new execution state with the given procedure as the root frame,
// execute it, and return the procedure's return value.
generic_ptr
call(context&, generic_ptr callable, std::vector<generic_ptr> const& arguments);

} // namespace insider

#endif
