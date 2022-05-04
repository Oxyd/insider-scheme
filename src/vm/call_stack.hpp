#ifndef INSIDER_VM_CALL_STACK_HPP
#define INSIDER_VM_CALL_STACK_HPP

#include "memory/page_allocator.hpp"
#include "object.hpp"
#include "ptr.hpp"
#include "runtime/integer.hpp"
#include "util/object_span.hpp"

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

  struct frame_span {
    object_span         data;
    integer::value_type last_frame_base;
    integer::value_type first_frame_base;
  };

  void
  push_frame(ptr<> callable, std::size_t locals_size);

  void
  pop_frame();

  void
  move_current_frame_up();

  integer::value_type
  current_frame_index() const { return current_base_; }

  ptr<>&
  callable(integer::value_type frame) {
    return data_[frame + callable_offset];
  }

  ptr<stack_frame_extra_data>
  extra(integer::value_type frame) {
    return assume<stack_frame_extra_data>(data_[frame + extra_data_offset]);
  }

  void
  set_extra(integer::value_type frame, ptr<stack_frame_extra_data> value) {
    data_[frame + extra_data_offset] = value;
  }

  ptr<>&
  local(integer::value_type frame, std::size_t local) {
    return data_[frame + stack_frame_header_size + local];
  }

  object_span
  current_locals_span();

  integer::value_type
  parent(integer::value_type frame) const {
    return assume<integer>(data_[frame + previous_base_offset]).value();
  }

  integer::value_type
  previous_pc(integer::value_type frame) const {
    return assume<integer>(data_[frame + previous_pc_offset]).value();
  }

  void
  set_previous_pc(integer::value_type frame, integer::value_type pc) {
    data_[frame + previous_pc_offset] = integer_to_ptr(pc);
  }

  void
  push(ptr<> x) {
    data_.push_back(x);
  }

  ptr<>
  pop() {
    auto result = data_.back();
    data_.pop_back();
    return result;
  }

  bool
  empty() const { return current_base_ == -1; }

  frame_span
  frames(integer::value_type begin, integer::value_type end) const;

  integer::value_type
  frames_end() const { return data_.size(); }

  void
  append_frames(frame_span);

  void
  visit_members(member_visitor const&);

private:
  static constexpr integer::value_type previous_base_offset = 0;
  static constexpr integer::value_type previous_pc_offset = 1;
  static constexpr integer::value_type callable_offset = 2;
  static constexpr integer::value_type extra_data_offset = 3;
  static constexpr std::size_t stack_frame_header_size = 4;

  std::vector<ptr<>>  data_;
  integer::value_type current_base_ = -1;

  integer::value_type
  find_last_frame_base(integer::value_type end) const;

  void
  fix_base_offsets(frame_span const& frames);
};

inline ptr<>
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
  return stack->local(stack->current_frame_index(), i);
}

inline integer::value_type
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

class frame_reference {
public:
  frame_reference() = default;

  frame_reference(ptr<call_stack> stack, integer::value_type idx)
    : stack_{stack}
    , idx_{idx}
  { }

  ptr<>&
  local(std::size_t i) const { return stack_->local(idx_, i); }

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

  integer::value_type
  index() const { return idx_; }

  explicit
  operator bool () const { return idx_ != -1; }

private:
  ptr<call_stack>     stack_;
  integer::value_type idx_ = -1;
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
  ptr<call_stack> stack_{};
  integer::value_type current_ = -1;
};

inline bool
operator != (call_stack_iterator lhs, call_stack_iterator rhs) {
  return !(lhs == rhs);
}

}

#endif
