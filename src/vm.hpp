#ifndef INSIDER_VM_HPP
#define INSIDER_VM_HPP

#include "scheme.hpp"

#include <limits>

namespace insider {

// Dynamic-sized stack to store local variables.
class root_stack : public composite_root_object<root_stack> {
public:
  static constexpr char const* scheme_name = "insider::root_stack";

  object*
  ref(std::size_t i) { return data_[i]; }

  void
  set(free_store&, std::size_t i, object* value) {
    data_[i] = value;
  }

  void
  grow(std::size_t n) { data_.resize(data_.size() + n); }

  void
  shrink(std::size_t n) { data_.resize(data_.size() - n); }

  void
  reserve(std::size_t n) { data_.reserve(n); }

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

inline void
stack_set(tracked_ptr<root_stack> const& s, std::size_t i, object* value) {
  s->set(s.store(), i, value);
}

class call_stack : public composite_root_object<call_stack> {
public:
  static constexpr char const* scheme_name = "insider::call_stack";

  struct frame {
    std::size_t         pc;
    insider::procedure* procedure;
    std::size_t         stack_top;
    operand             dest_register;
  };

  frame&
  push(insider::procedure* proc, std::size_t stack_top);

  void
  pop() { frames_.pop_back(); }

  void
  pop_parent() { frames_.erase(frames_.end() - 2); }

  frame&
  current_frame() { return frames_.back(); }

  frame&
  parent_frame() { assert(frames_.size() >= 2); return *(frames_.end() - 2); }

  void
  reserve(std::size_t n) { frames_.reserve(n); }

  std::size_t
  size() const { return frames_.size(); }

  auto
  rbegin() { return frames_.rbegin(); }

  auto
  rend() { return frames_.rend(); }

  void
  trace(tracing_context&);

  void
  update_references();

private:
  std::vector<frame> frames_;
};

struct execution_state {
  context&                         ctx;
  tracked_ptr<root_stack>          value_stack;
  tracked_ptr<insider::call_stack> call_stack;
  generic_tracked_ptr              global_return;

  execution_state(context& ctx);
};

object*
call_frame_local(execution_state& state, operand local);

// Make execution state using the given procedure as the global call frame.
execution_state
make_state(context&, procedure*,
           std::vector<object*> const& closure = {}, std::vector<object*> const& arguments = {});

// Run the bytecode in the execution context's global call frame.
//
// Causes a garbage collection.
object*
run(execution_state&);

// Create a new execution state with the given procedure as the root frame,
// execute it, and return the procedure's return value.
//
// Causes a garbage collection.
object*
call(context&, object* callable, std::vector<object*> const& arguments);

} // namespace insider

#endif
