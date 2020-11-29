#ifndef INSIDER_VM_HPP
#define INSIDER_VM_HPP

#include "scheme.hpp"

#include <limits>
#include <type_traits>

namespace insider {

// Dynamic-sized stack to store local variables.
class root_stack : public composite_root_object<root_stack> {
public:
  static constexpr char const* scheme_name = "insider::root_stack";

  root_stack();

  object*
  ref(std::size_t i) { return data_[i]; }

  void
  set(std::size_t i, object* value) {
    assert(is_valid(value));
    data_[i] = value;
  }

  void
  push(object* value) {
    assert(is_valid(value));

    resize_uninitialised(size_ + 1);
    data_[size_ - 1] = value;
  }

  object*
  pop() {
    return data_[--size_];
  }

  void
  grow(std::size_t n);

  void
  shrink(std::size_t n);

  void
  resize(std::size_t new_size);

  void
  erase(std::size_t begin, std::size_t end);

  std::size_t
  size() const { return size_; }

  void
  trace(tracing_context&) const;

  void
  update_references();

  std::size_t
  hash() const { return 0; }

private:
  std::unique_ptr<object*[]> data_;
  std::size_t size_ = 0;
  std::size_t alloc_;

  void
  resize_uninitialised(std::size_t new_size);
};

class call_stack : public composite_root_object<call_stack> {
public:
  static constexpr char const* scheme_name = "insider::call_stack";

  struct frame {
    std::size_t         pc;
    insider::procedure* procedure;
    std::size_t         stack_top;
    operand             dest_register;
    char const*         native_name;
  };

  call_stack();

  frame&
  push(insider::procedure* proc, std::size_t stack_top);

  frame&
  push_native(char const* name);

  void
  pop() { --size_; }

  frame&
  current_frame() { return frames_[size_ - 1]; }

  std::size_t
  size() const { return size_; }

  frame const&
  get(std::size_t i) const { return frames_[i]; }

  void
  trace(tracing_context&) const;

  void
  update_references();

  std::size_t
  hash() const { return 0; }

private:
  static_assert(std::is_trivial_v<frame>);

  std::unique_ptr<frame[]> frames_;
  std::size_t size_ = 0;
  std::size_t alloc_;

  void
  grow(std::size_t new_alloc);
};

struct execution_state {
  context&                         ctx;
  tracked_ptr<root_stack>          value_stack;
  tracked_ptr<insider::call_stack> call_stack;

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
generic_tracked_ptr
run(execution_state&);

// Create a new execution state with the given procedure as the root frame,
// execute it, and return the procedure's return value.
//
// Causes a garbage collection.
generic_tracked_ptr
call(context&, object* callable, std::vector<object*> const& arguments);

} // namespace insider

#endif
