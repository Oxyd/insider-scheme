#ifndef INSIDER_VM_CALL_STACK_HPP
#define INSIDER_VM_CALL_STACK_HPP

#include "memory/page_allocator.hpp"
#include "object.hpp"
#include "ptr.hpp"
#include "runtime/integer.hpp"
#include "util/integer_cast.hpp"
#include "util/object_span.hpp"
#include "vm/operand.hpp"

#include <functional>
#include <memory>
#include <optional>
#include <stdexcept>
#include <type_traits>

namespace insider {

class context;
class free_store;
class parameter_tag;

using native_continuation_type = std::function<ptr<>(context&, ptr<>)>;

class stack_frame_extra_data : public composite_object<stack_frame_extra_data> {
public:
  static constexpr char const* scheme_name = "insider::stack_frame_extra_data";

  ptr<insider::parameter_tag>           parameter_tag;
  ptr<>                                 parameter_value;
  std::vector<native_continuation_type> native_continuations;
  bool                                  allow_jump_out = true;
  bool                                  allow_jump_in  = true;
  ptr<>                                 before_thunk;
  ptr<>                                 after_thunk;
  ptr<>                                 exception_handler;
  std::optional<integer::value_type>    next_exception_handler_frame;

  void
  visit_members(member_visitor const& f);
};

// The runtime stack used by the VM to store procedure activation records, local
// variables and temporary values.
class call_stack : public composite_object<call_stack> {
public:
  static constexpr char const* scheme_name = "insider::call_stack";
  static constexpr integer::value_type stack_frame_header_size = 4;

  using frame_index = integer::value_type;

  struct frame_span {
    object_span data;
    frame_index last_frame_base;
    frame_index first_frame_base;
  };

  call_stack();

  call_stack(call_stack const&);

  call_stack&
  operator = (call_stack const&);

  void
  push_frame(ptr<> callable, std::size_t locals_size,
             integer::value_type previous_pc,
             ptr<stack_frame_extra_data> extra = {});

  void
  pop_frame();

  void
  resize_current_frame(std::size_t new_size);

  void
  move_tail_call_arguments(std::size_t new_args_size,
                           std::size_t old_args_size);

  frame_index
  current_frame_index() const { return current_base_; }

  ptr<>&
  callable(frame_index frame) {
    return data_[frame + callable_offset];
  }

  ptr<stack_frame_extra_data>
  extra(frame_index frame) {
    return assume<stack_frame_extra_data>(data_[frame + extra_data_offset]);
  }

  void
  set_extra(frame_index frame, ptr<stack_frame_extra_data> value) {
    data_[frame + extra_data_offset] = value;
  }

  ptr<>&
  local(frame_index frame, operand local) {
    return data_[frame + stack_frame_header_size + local];
  }

  object_span
  call_args_span(integer::value_type num_args) {
    return {&data_[current_base_ - num_args], &data_[current_base_]};
  }

  frame_index
  parent(frame_index frame) const {
    return assume<integer>(data_[frame + previous_base_offset]).value();
  }

  integer::value_type
  previous_pc(integer::value_type frame) const {
    return assume<integer>(data_[frame + previous_pc_offset]).value();
  }

  void
  set_previous_pc(frame_index frame, integer::value_type pc) {
    data_[frame + previous_pc_offset] = integer_to_ptr(pc);
  }

  void
  push(ptr<> x) {
    ensure_capacity(1);
    data_[size_++] = x;
  }

  ptr<>
  pop() { return data_[--size_]; }

  bool
  empty() const { return current_base_ == -1; }

  std::size_t
  size() const { return static_cast<std::size_t>(size_); }

  void
  clear() { current_base_ = -1; size_ = 0; }

  frame_span
  frames(frame_index begin, frame_index end) const;

  frame_index
  frames_end() const { return static_cast<frame_index>(size_); }

  void
  append_frames(frame_span);

  void
  visit_members(member_visitor const&);

private:
  static constexpr integer::value_type previous_base_offset = 0;
  static constexpr integer::value_type previous_pc_offset = 1;
  static constexpr integer::value_type callable_offset = 2;
  static constexpr integer::value_type extra_data_offset = 3;
  static constexpr std::size_t alloc_size = 4096;

  std::unique_ptr<ptr<>[]> data_;
  std::size_t              capacity_     = 0;
  std::size_t              size_         = 0;
  frame_index              current_base_ = -1;

  void
  ensure_capacity(std::size_t required_size);

  void
  grow_capacity(std::size_t requested_capacity);

  std::size_t
  find_new_capacity(std::size_t at_least) const;

  frame_index
  find_last_frame_base(frame_index end) const;

  void
  fix_base_offsets(frame_span const& frames);
};

inline ptr<>&
current_frame_callable(ptr<call_stack> stack) {
  return stack->callable(stack->current_frame_index());
}

inline ptr<stack_frame_extra_data>
current_frame_extra(ptr<call_stack> stack) {
  return stack->extra(stack->current_frame_index());
}

inline void
current_frame_set_extra(ptr<call_stack> stack, ptr<stack_frame_extra_data> e) {
  stack->set_extra(stack->current_frame_index(), e);
}

inline ptr<>&
current_frame_local(ptr<call_stack> stack, std::size_t i) {
  return stack->local(stack->current_frame_index(), to_signed<operand>(i));
}

inline call_stack::frame_index
current_frame_parent(ptr<call_stack> stack) {
  return stack->parent(stack->current_frame_index());
}

inline integer::value_type
current_frame_previous_pc(ptr<call_stack> stack) {
  return stack->previous_pc(stack->current_frame_index());
}

inline void
current_frame_set_previous_pc(ptr<call_stack> stack, integer::value_type pc) {
  stack->set_previous_pc(stack->current_frame_index(), pc);
}

inline void
pop_n(ptr<call_stack> stack, std::size_t n) {
  while (n > 0) {
    stack->pop();
    --n;
  }
}

class frame_reference {
public:
  frame_reference() = default;

  frame_reference(ptr<call_stack> stack, call_stack::frame_index idx)
    : stack_{stack}
    , idx_{idx}
  { }

  ptr<>&
  local(operand i) const { return stack_->local(idx_, i); }

  ptr<>&
  callable() const { return stack_->callable(idx_); }

  ptr<stack_frame_extra_data>
  extra() const { return stack_->extra(idx_); }

  void
  set_extra(ptr<stack_frame_extra_data> value) const {
    stack_->set_extra(idx_, value);
  }

  integer::value_type
  previous_pc() const { return stack_->previous_pc(idx_); }

  void
  set_previous_pc(integer::value_type pc) const {
    stack_->set_previous_pc(idx_, pc);
  }

  frame_reference
  parent() const { return {stack_, stack_->parent(idx_)}; }

  call_stack::frame_index
  index() const { return idx_; }

  explicit
  operator bool () const { return idx_ != -1; }

  bool
  operator == (frame_reference other) const {
    return stack_ == other.stack_ && idx_ == other.idx_;
  }

private:
  ptr<call_stack>         stack_;
  call_stack::frame_index idx_ = -1;
};

inline frame_reference
current_frame(ptr<call_stack> stack) {
  return {stack, stack->current_frame_index()};
}

class call_stack_iterator {
public:
  using value_type = integer::value_type;
  using difference_type = integer::value_type;
  using reference = integer::value_type;
  using pointer = integer::value_type*;
  using iterator_category = std::forward_iterator_tag;

  call_stack_iterator() = default;

  explicit
  call_stack_iterator(ptr<call_stack> cs)
    : stack_{cs}
    , current_{cs->current_frame_index()}
  { }

  integer::value_type
  operator * () const { return current_; }

  call_stack_iterator&
  operator ++ () {
    current_ = stack_->parent(current_);
    return *this;
  }

  call_stack_iterator
  operator ++ (int) {
    call_stack_iterator result{*this};
    ++*this;
    return result;
  }

  bool
  operator == (call_stack_iterator other) const {
    return current_ == other.current_;
  }

private:
  ptr<call_stack>         stack_{};
  call_stack::frame_index current_ = -1;
};

inline bool
operator != (call_stack_iterator lhs, call_stack_iterator rhs) {
  return !(lhs == rhs);
}

}

#endif
