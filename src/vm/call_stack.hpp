#ifndef INSIDER_VM_CALL_STACK_HPP
#define INSIDER_VM_CALL_STACK_HPP

#include "memory/member_visitor.hpp"
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

struct parameter_assignment {
  ptr<parameter_tag> tag;
  ptr<>              value;
};

using parameter_assignments = std::vector<parameter_assignment>;

class stack_frame_extra_data : public composite_object<stack_frame_extra_data> {
public:
  static constexpr char const* scheme_name = "insider::stack_frame_extra_data";

  parameter_assignments                 parameters;
  bool                                  allow_jump_out = true;
  bool                                  allow_jump_in  = true;
  ptr<>                                 before_thunk;
  ptr<>                                 after_thunk;
  ptr<>                                 exception_handler;
  std::optional<integer::value_type>    next_exception_handler_frame;

  void
  visit_members(member_visitor const& f) const;
};

using register_index = std::uint32_t;

// The runtime stack used by the VM to store procedure activation records, local
// variables and temporary values.
class call_stack {
public:
  using frame_index = std::size_t;

  enum class frame_type : std::uint8_t {
    scheme, native, native_continuation, dummy
  };

  struct frame {
    frame_type                  type;
    operand                     result_register;
    register_index              base;
    register_index              size;
    instruction_pointer         previous_ip;
    ptr<stack_frame_extra_data> extra;
  };

  struct frame_span {
    call_stack const*      stack;
    std::span<frame const> frames;
  };

  call_stack();

  call_stack(call_stack const&);

  call_stack(call_stack&&) = default;

  void
  push_frame(frame const& f) {
    assert(f.size > 0);
    assert(!frames_.empty() || f.base == 0);
    assert(frames_.empty() || f.base >= current_frame().base);
    assert(frames_.empty()
           || f.base <= current_frame().base + current_frame().size);

    frames_.push_back(f);

    current_base_ = f.base;
    data_size_ = f.base + f.size;
    ensure_data_capacity(data_size_);
  }

  void
  pop_frame() {
    frames_.pop_back();
    if (!frames_.empty()) {
      frame& f = current_frame();
      current_base_ = f.base;
      data_size_ = current_base_ + f.size;
    } else
      current_base_ = data_size_ = 0;

    assert(data_size_ <= data_capacity_);
  }

  void
  replace_frame(frame_type new_type, register_index new_locals_size) {
    assert(!frames_.empty());

    frame& f = current_frame();
    f.size = new_locals_size;
    f.type = new_type;

    data_size_ = current_base_ + new_locals_size;
    ensure_data_capacity(data_size_);
  }

  void
  replace_frame(frame_type new_type, register_index new_locals_size,
                register_index args_base, register_index args_size) {
    assert(!frames_.empty());

    frame& f = current_frame();
    register_index current_base = f.base;

    register_index dest_begin = current_base;
    register_index src_begin  = current_base + args_base;
    register_index src_end    = src_begin + args_size + 1;

    assert(src_end <= data_size_);
    [[maybe_unused]] register_index dest_end = dest_begin + args_size + 1;
    assert(dest_end <= data_size_);

    std::copy(data_.get() + src_begin, data_.get() + src_end,
              data_.get() + dest_begin);

    f.size = new_locals_size;
    f.type = new_type;

    data_size_ = current_base_ + new_locals_size;
    ensure_data_capacity(data_size_);
  }

  void
  resize_current_frame(register_index new_size);

  std::optional<frame_index>
  current_frame_index() const {
    if (!empty())
      return frames_.size() - 1;
    else
      return std::nullopt;
  }

  ptr<>&
  callable() { return local(0); }

  ptr<>
  callable() const { return local(0); }

  ptr<>&
  callable(frame_index frame) { return local(frame, 0); }

  frame_type
  type(frame_index frame) { return frames_[frame].type; }

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

  ptr<>
  local(operand local) const {
    assert(local < current_frame().size);
    return data_[current_base_ + local];
  }

  ptr<>&
  local(frame_index frame, operand local) {
    return data_[frames_[frame].base + local];
  }

  object_span
  current_frame_span() const {
    register_index base = current_frame().base;
    return {&data_[base], &data_[base + current_frame().size]};
  }

  std::optional<frame_index>
  parent() const {
    if (frames_.size() >= 2)
      return frames_.size() - 2;
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

  register_index
  frame_base() const { return current_base_; }

  register_index
  frame_base(frame_index frame) const {
    return frames_[frame].base;
  }

  register_index
  frame_size() const {
    return current_frame().size;
  }

  register_index
  frame_size(frame_index frame) const {
    return frames_[frame].size;
  }

  bool
  empty() const { return frames_.empty(); }

  register_index
  size() const { return data_size_; }

  std::size_t
  frame_count() const { return frames_.size(); }

  void
  clear() { frames_.clear(); data_size_ = 0; }

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
  append_frame(call_stack const& from, frame_index idx);

  void
  visit_members(member_visitor const&) const;

private:
  static constexpr std::size_t frames_alloc_size = 1024;
  static constexpr register_index data_alloc_size = 4096;

  std::vector<frame>       frames_;
  std::unique_ptr<ptr<>[]> data_;
  register_index           data_capacity_ = 0;
  register_index           data_size_     = 0;
  register_index           current_base_  = 0;

  frame&
  current_frame() {
    assert(!frames_.empty());
    return frames_.back();
  }

  frame const&
  current_frame() const {
    assert(!frames_.empty());
    return frames_.back();
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
  ensure_data_capacity(register_index required_size) {
    if (required_size >= data_capacity_) [[unlikely]]
      grow_data(required_size);
  }

  void
  grow_data(register_index requested_size);

  register_index
  real_data_size() const;
};

class captured_call_stack : public composite_object<captured_call_stack> {
public:
  static constexpr char const* scheme_name = "insider::captured_call_stack";

  call_stack stack;
  vm_id_type vm_id;

  captured_call_stack(call_stack stack, vm_id_type vm_id)
    : stack{std::move(stack)}
    , vm_id{vm_id}
  { }

  void
  visit_members(member_visitor const& f) const {
    stack.visit_members(f);
  }
};

class frame_reference {
public:
  frame_reference() = default;

  frame_reference(call_stack& stack,
                  std::optional<call_stack::frame_index> idx)
    : stack_{&stack}
    , idx_{idx}
  { }

  ptr<>&
  local(operand i) const { return stack_->local(*idx_, i); }

  ptr<>&
  callable() const { return stack_->callable(*idx_); }

  call_stack::frame_type
  type() const { return stack_->type(*idx_); }

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
  parent() const { return {*stack_, stack_->parent(*idx_)}; }

  call_stack::frame_index
  index() const { return *idx_; }

  explicit
  operator bool () const { return idx_.has_value(); }

  bool
  operator == (frame_reference other) const {
    return &stack_ == &other.stack_ && idx_ == other.idx_;
  }

private:
  call_stack*                            stack_;
  std::optional<call_stack::frame_index> idx_;
};

inline frame_reference
current_frame(call_stack& stack) {
  return {stack, stack.current_frame_index()};
}

inline call_stack::frame_type
callable_to_frame_type(ptr<> callable) {
  if (!callable)
    return call_stack::frame_type::dummy;
  else if (is<procedure>(callable))
    return call_stack::frame_type::scheme;
  else {
    assert(is<native_procedure>(callable));
    return call_stack::frame_type::native;
  }
}

}

#endif
