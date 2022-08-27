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
#include <ranges>
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
  struct frame;

public:
  static constexpr char const* scheme_name = "insider::call_stack";

  using frame_index = std::size_t;

  struct frame_span {
    call_stack const*      stack;
    std::span<frame const> frames;
  };

  call_stack();

  call_stack(call_stack const&);

  call_stack&
  operator = (call_stack const&);

  void
  push_frame(ptr<> callable, std::size_t base, std::size_t locals_size,
             integer::value_type previous_pc, operand result_register,
             ptr<stack_frame_extra_data> extra = {});

  void
  pop_frame();

  void
  replace_frame(ptr<> new_callable, std::size_t new_locals_size);

  void
  replace_frame(ptr<> new_callable, std::size_t new_locals_size,
                std::size_t args_base, std::size_t args_size);

  void
  resize_current_frame(std::size_t new_size);

  std::optional<frame_index>
  current_frame_index() const {
    if (!empty())
      return frames_.size() - 1;
    else
      return std::nullopt;
  }

  ptr<>&
  callable(frame_index frame) {
    return frames_[frame].callable;
  }

  ptr<stack_frame_extra_data>
  extra(frame_index frame) {
    return frames_[frame].extra;
  }

  void
  set_extra(frame_index frame, ptr<stack_frame_extra_data> value) {
    frames_[frame].extra = value;
  }

  ptr<>&
  local(frame_index frame, operand local) {
    return data_[frames_[frame].base + local];
  }

  object_span
  current_frame_span() const {
    std::size_t base = frames_.back().base;
    return {&data_[base], &data_[base + frames_.back().size]};
  }

  std::optional<frame_index>
  parent(frame_index frame) const {
    if (frame > 0)
      return frame - 1;
    else
      return std::nullopt;
  }

  integer::value_type
  previous_pc(frame_index frame) const {
    return frames_[frame].previous_pc;
  }

  void
  set_previous_pc(frame_index frame, integer::value_type pc) {
    frames_[frame].previous_pc = pc;
  }

  operand
  result_register(frame_index frame) const {
    return frames_[frame].result_register;
  }

  std::size_t
  frame_base(frame_index frame) const {
    return frames_[frame].base;
  }

  std::size_t
  frame_size(frame_index frame) const {
    return frames_[frame].size;
  }

  void
  push(ptr<> x) {
    ensure_capacity(size_ + 1);
    data_[size_++] = x;
  }

  ptr<>
  pop() { return data_[--size_]; }

  bool
  empty() const { return frames_.empty(); }

  std::size_t
  size() const { return size_; }

  void
  clear() { frames_.clear(); size_ = 0; }

  auto
  frames_range() const {
    return std::views::iota(frame_index{0}, frames_.size())
           | std::views::reverse;
  }

  frame_span
  frames(frame_index begin, frame_index end) const;

  frame_index
  frames_end() const { return frames_.size(); }

  void
  append_frames(frame_span);

  void
  visit_members(member_visitor const&);

private:
  static constexpr std::size_t alloc_size = 4096;

  struct frame {
    std::size_t                 base;
    std::size_t                 size;
    integer::value_type         previous_pc;
    operand                     result_register;
    ptr<>                       callable;
    ptr<stack_frame_extra_data> extra;
  };

  std::vector<frame>       frames_;
  std::unique_ptr<ptr<>[]> data_;
  std::size_t              capacity_     = 0;
  std::size_t              size_         = 0;

  void
  update_size();

  void
  ensure_capacity(std::size_t required_size);

  void
  grow_capacity(std::size_t requested_capacity);

  std::size_t
  find_new_capacity(std::size_t at_least) const;
};

inline ptr<>&
current_frame_callable(ptr<call_stack> stack) {
  return stack->callable(*stack->current_frame_index());
}

inline ptr<stack_frame_extra_data>
current_frame_extra(ptr<call_stack> stack) {
  return stack->extra(*stack->current_frame_index());
}

inline void
current_frame_set_extra(ptr<call_stack> stack, ptr<stack_frame_extra_data> e) {
  stack->set_extra(*stack->current_frame_index(), e);
}

inline ptr<>&
current_frame_local(ptr<call_stack> stack, std::size_t i) {
  return stack->local(*stack->current_frame_index(), to_signed<operand>(i));
}

inline std::optional<call_stack::frame_index>
current_frame_parent(ptr<call_stack> stack) {
  return stack->parent(*stack->current_frame_index());
}

inline integer::value_type
current_frame_previous_pc(ptr<call_stack> stack) {
  return stack->previous_pc(*stack->current_frame_index());
}

inline void
current_frame_set_previous_pc(ptr<call_stack> stack, integer::value_type pc) {
  stack->set_previous_pc(*stack->current_frame_index(), pc);
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

  frame_reference(ptr<call_stack> stack,
                  std::optional<call_stack::frame_index> idx)
    : stack_{stack}
    , idx_{idx}
  { }

  ptr<>&
  local(operand i) const { return stack_->local(*idx_, i); }

  ptr<>&
  callable() const { return stack_->callable(*idx_); }

  ptr<stack_frame_extra_data>
  extra() const { return stack_->extra(*idx_); }

  void
  set_extra(ptr<stack_frame_extra_data> value) const {
    stack_->set_extra(*idx_, value);
  }

  integer::value_type
  previous_pc() const { return stack_->previous_pc(*idx_); }

  void
  set_previous_pc(integer::value_type pc) const {
    stack_->set_previous_pc(*idx_, pc);
  }

  operand
  result_register() const { return stack_->result_register(*idx_); }

  std::size_t
  base() const { return stack_->frame_base(*idx_); }

  std::size_t
  size() const { return stack_->frame_size(*idx_); }

  frame_reference
  parent() const { return {stack_, stack_->parent(*idx_)}; }

  call_stack::frame_index
  index() const { return *idx_; }

  explicit
  operator bool () const { return idx_.has_value(); }

  bool
  operator == (frame_reference other) const {
    return stack_ == other.stack_ && idx_ == other.idx_;
  }

private:
  ptr<call_stack>                        stack_;
  std::optional<call_stack::frame_index> idx_;
};

inline frame_reference
current_frame(ptr<call_stack> stack) {
  return {stack, stack->current_frame_index()};
}

}

#endif
