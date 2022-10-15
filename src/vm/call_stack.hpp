#ifndef INSIDER_VM_CALL_STACK_HPP
#define INSIDER_VM_CALL_STACK_HPP

#include "memory/page_allocator.hpp"
#include "object.hpp"
#include "ptr.hpp"
#include "runtime/basic_types.hpp"
#include "runtime/integer.hpp"
#include "util/integer_cast.hpp"
#include "util/object_span.hpp"
#include "vm/bytecode.hpp"
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

  enum class frame_type {
    scheme, native, dummy
  };

  call_stack();

  call_stack(call_stack const&);

  void
  push_frame(ptr<> callable, std::size_t base, std::size_t locals_size,
             instruction_pointer previous_ip, operand result_register,
             ptr<stack_frame_extra_data> extra = {}) {
    assert(frames_size_ > 0 || base == 0);
    assert(frames_size_ == 0 || base >= current_frame().base);
    assert(frames_size_ == 0
           || base <= current_frame().base + current_frame().size);

    ensure_frames_capacity(frames_size_ + 1);

    frames_[frames_size_++] = frame{base, locals_size, previous_ip,
                                    result_register, extra,
                                    callable_to_frame_type(callable)};

    current_base_ = base;
    data_size_ = base + locals_size;
    ensure_data_capacity(data_size_);
  }

  void
  pop_frame() {
    --frames_size_;
    if (frames_size_ > 0) {
      frame& f = current_frame();
      current_base_ = f.base;
      data_size_ = current_base_ + f.size;
    } else
      current_base_ = data_size_ = 0;

    assert(data_size_ <= data_capacity_);
  }

  void
  replace_frame(ptr<> new_callable, std::size_t new_locals_size) {
    assert(frames_size_ > 0);

    frame& f = current_frame();
    f.size = new_locals_size;
    f.type = callable_to_frame_type(new_callable);

    data_size_ = current_base_ + new_locals_size;
    ensure_data_capacity(data_size_);
  }

  void
  replace_frame(ptr<> new_callable, std::size_t new_locals_size,
                std::size_t args_base, std::size_t args_size) {
    assert(frames_size_ > 0);

    frame& f = current_frame();
    std::size_t current_base = f.base;

    std::size_t dest_begin = current_base;
    std::size_t src_begin  = current_base + args_base;
    std::size_t src_end    = src_begin + args_size + 1;

    assert(src_end <= data_size_);
    [[maybe_unused]] std::size_t dest_end = dest_begin + args_size + 1;
    assert(dest_end <= data_size_);

    std::copy(data_.get() + src_begin, data_.get() + src_end,
              data_.get() + dest_begin);

    f.size = new_locals_size;
    f.type = callable_to_frame_type(new_callable);

    data_size_ = current_base_ + new_locals_size;
    ensure_data_capacity(data_size_);
  }

  void
  resize_current_frame(std::size_t new_size);

  std::optional<frame_index>
  current_frame_index() const {
    if (!empty())
      return frames_size_ - 1;
    else
      return std::nullopt;
  }

  ptr<>&
  callable() { return local(0); }

  ptr<>&
  callable(frame_index frame) { return local(frame, 0); }

  frame_type
  current_frame_type() const { return current_frame().type; }

  ptr<stack_frame_extra_data>
  extra() {
    return current_frame().extra;
  }

  ptr<stack_frame_extra_data>
  extra(frame_index frame) {
    return frames_[frame].extra;
  }

  void
  set_extra(ptr<stack_frame_extra_data> value) {
    current_frame().extra = value;
  }

  void
  set_extra(frame_index frame, ptr<stack_frame_extra_data> value) {
    frames_[frame].extra = value;
  }

  ptr<>&
  local(operand local) {
    assert(local < current_frame().size);
    return data_[current_base_ + local];
  }

  ptr<>&
  local(frame_index frame, operand local) {
    return data_[frames_[frame].base + local];
  }

  object_span
  current_frame_span() const {
    std::size_t base = current_frame().base;
    return {&data_[base], &data_[base + current_frame().size]};
  }

  std::optional<frame_index>
  parent() const {
    if (frames_size_ >= 2)
      return frames_size_ - 2;
    else
      return std::nullopt;
  }

  std::optional<frame_index>
  parent(frame_index frame) const {
    if (frame > 0)
      return frame - 1;
    else
      return std::nullopt;
  }

  instruction_pointer
  previous_ip() const {
    return current_frame().previous_ip;
  }

  instruction_pointer
  previous_ip(frame_index frame) const {
    return frames_[frame].previous_ip;
  }

  void
  set_previous_ip(frame_index frame, instruction_pointer ip) {
    frames_[frame].previous_ip = ip;
  }

  operand
  result_register() const {
    return current_frame().result_register;
  }

  operand
  result_register(frame_index frame) const {
    return frames_[frame].result_register;
  }

  std::size_t
  frame_base() const { return current_base_; }

  std::size_t
  frame_base(frame_index frame) const {
    return frames_[frame].base;
  }

  std::size_t
  frame_size() const {
    return current_frame().size;
  }

  std::size_t
  frame_size(frame_index frame) const {
    return frames_[frame].size;
  }

  bool
  empty() const { return frames_size_ == 0; }

  std::size_t
  size() const { return data_size_; }

  void
  clear() { frames_size_ = 0; data_size_ = 0; }

  auto
  frames_range() const {
    return std::views::iota(frame_index{0}, frames_size_)
           | std::views::reverse;
  }

  frame_span
  frames(frame_index begin, frame_index end) const;

  frame_index
  frames_end() const { return frames_size_; }

  void
  append_frames(frame_span);

  void
  visit_members(member_visitor const&);

private:
  static constexpr std::size_t frames_alloc_size = 1024;
  static constexpr std::size_t data_alloc_size = 4096;

  struct frame {
    std::size_t                 base;
    std::size_t                 size;
    instruction_pointer         previous_ip;
    operand                     result_register;
    ptr<stack_frame_extra_data> extra;
    frame_type                  type;
  };

  std::unique_ptr<frame[]> frames_;
  std::size_t              frames_capacity_ = 0;
  std::size_t              frames_size_ = 0;
  std::unique_ptr<ptr<>[]> data_;
  std::size_t              data_capacity_ = 0;
  std::size_t              data_size_     = 0;
  std::size_t              current_base_  = 0;

  frame&
  current_frame() {
    assert(frames_size_ > 0);
    return frames_[frames_size_ - 1];
  }

  frame const&
  current_frame() const {
    assert(frames_size_ > 0);
    return frames_[frames_size_ - 1];
  }

  frame_type
  callable_to_frame_type(ptr<> callable) {
    if (!callable)
      return frame_type::dummy;
    else if (is<procedure>(callable))
      return frame_type::scheme;
    else {
      assert(is<native_procedure>(callable));
      return frame_type::native;
    }
  }

  void
  check_update_current_frame() {
    if (!empty())
      update_current_frame();
    else
      data_size_ = 0;
  }

  void
  update_current_frame() {
    frame& f = current_frame();
    current_base_ = f.base;
    data_size_ = current_base_ + f.size;
    ensure_data_capacity(data_size_);
  }

  void
  ensure_frames_capacity(std::size_t required_size) {
    if (required_size >= frames_capacity_) [[unlikely]]
      grow_frames(required_size);
  }

  void
  ensure_data_capacity(std::size_t required_size) {
    if (required_size >= data_capacity_) [[unlikely]]
      grow_data(required_size);
  }

  void
  grow_frames(std::size_t requested_size);

  void
  grow_data(std::size_t requested_size);

  std::size_t
  real_data_size() const;
};

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

  instruction_pointer
  previous_ip() const { return stack_->previous_ip(*idx_); }

  void
  set_previous_ip(instruction_pointer ip) const {
    stack_->set_previous_ip(*idx_, ip);
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
