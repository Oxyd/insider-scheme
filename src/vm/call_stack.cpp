#include "call_stack.hpp"

#include "context.hpp"
#include "memory/free_store.hpp"
#include "runtime/basic_types.hpp"
#include "runtime/integer.hpp"
#include "util/integer_cast.hpp"

#include <algorithm>
#include <ranges>

namespace insider {

void
stack_frame_extra_data::visit_members(member_visitor const &f) {
  f(parameter_tag);
  f(parameter_value);
  f(before_thunk);
  f(after_thunk);
  f(exception_handler);
}

call_stack::call_stack()
  : data_{std::make_unique<ptr<>[]>(alloc_size)}
  , capacity_{alloc_size}
{ }

call_stack::call_stack(call_stack const& other)
  : frames_{other.frames_}
  , data_{std::make_unique<ptr<>[]>(other.capacity_)}
  , capacity_{other.capacity_}
  , size_{other.size_}
  , current_base_{other.current_base_}
{
  std::ranges::copy(std::views::counted(other.data_.get(), size_), data_.get());
}

call_stack&
call_stack::operator = (call_stack const& other) {
  if (this != &other) {
    call_stack copy{other};
    frames_ = std::move(copy.frames_);
    data_ = std::move(copy.data_);
    capacity_ = copy.capacity_;
    size_ = copy.size_;
  }

  return *this;
}

void
call_stack::push_frame(ptr<> callable, std::size_t base,
                       std::size_t locals_size, instruction_pointer previous_ip,
                       operand result_register,
                       ptr<stack_frame_extra_data> extra) {
  assert(!callable || is_callable(callable));
  assert(!frames_.empty() || base == 0);
  assert(frames_.empty() || base >= frames_.back().base);
  assert(frames_.empty() || base <= frames_.back().base + frames_.back().size);

  frames_.emplace_back(frame{base, locals_size, previous_ip,
                             result_register, callable, extra,
                             callable_to_frame_type(callable)});
  update_current_frame();
}

void
call_stack::pop_frame() {
  frames_.pop_back();
  check_update_current_frame();
}

void
call_stack::replace_frame(ptr<> new_callable, std::size_t new_locals_size) {
  assert(!frames_.empty());
  assert(is_callable(new_callable));

  frames_.back().callable = new_callable;
  frames_.back().size = new_locals_size;
  frames_.back().type = callable_to_frame_type(new_callable);
  update_current_frame();
}

void
call_stack::replace_frame(ptr<> new_callable, std::size_t new_locals_size,
                          std::size_t args_base, std::size_t args_size) {
  assert(!frames_.empty());
  assert(is_callable(new_callable));

  std::size_t current_base = frames_.back().base;

  std::size_t dest_begin = current_base;
  std::size_t src_begin  = current_base + args_base;
  std::size_t src_end    = src_begin + args_size;

  assert(src_end <= size_);
  [[maybe_unused]] std::size_t dest_end = dest_begin + args_size;
  assert(dest_end <= size_);

  std::copy(data_.get() + src_begin, data_.get() + src_end,
            data_.get() + dest_begin);

  frames_.back().callable = new_callable;
  frames_.back().size = new_locals_size;
  frames_.back().type = callable_to_frame_type(new_callable);

  size_ = current_base_ + new_locals_size;
  ensure_capacity(size_);
}

void
call_stack::resize_current_frame(std::size_t new_size) {
  frames_.back().size = new_size;
  update_current_frame();
}

auto
call_stack::frames(frame_index begin, frame_index end) const -> frame_span {
  assert(end >= begin);
  return {this, {&frames_[begin], &frames_[end]}};
}

void
call_stack::append_frames(frame_span frames) {
  if (frames.frames.empty())
    return;

  std::size_t frames_begin = frames.frames.front().base;
  std::size_t frames_end
    = frames.frames.back().base + frames.frames.back().size;
  std::size_t frames_size = frames_end - frames_begin;
  ensure_capacity(size_ + frames_size);

  std::copy(frames.stack->data_.get() + frames_begin,
            frames.stack->data_.get() + frames_end,
            data_.get() + size_);

  int base_diff = to_signed<int>(size_) - frames.frames.front().base;
  for (frame f : frames.frames) {
    f.base += base_diff;
    frames_.push_back(f);
  }

  update_current_frame();
}

void
call_stack::visit_members(member_visitor const& f) {
  for (frame& fr : frames_) {
    f(fr.callable);
    f(fr.extra);
  }

  for (std::size_t i = 0; i < size_; ++i)
    f(data_[i]);

  // Clear data in the allocated area after the current end of stack. This is
  // because if the collector moves or deallocates those members, and then the
  // stack grows, we now have live invalid pointers. If another collection
  // happens before those invalid pointers are reassigned, we'll be feeding
  // invalid pointers to the GC which will crash.

  std::fill(data_.get() + size_, data_.get() + capacity_, ptr<>{});
}

auto
call_stack::callable_to_frame_type(ptr<> callable) -> frame_type {
  if (is<closure>(callable))
    return frame_type::scheme;
  else if (!callable)
    return frame_type::dummy;
  else {
    assert(is<native_procedure>(callable));
    return frame_type::native;
  }
}

void
call_stack::check_update_current_frame() {
  if (!frames_.empty())
    update_current_frame();
  else
    size_ = 0;
}

void
call_stack::update_current_frame() {
  current_base_ = frames_.back().base;
  size_ = current_base_ + frames_.back().size;
  ensure_capacity(size_);
}

void
call_stack::ensure_capacity(std::size_t required_size) {
  if (required_size >= capacity_) [[unlikely]]
    grow_capacity(required_size);
}

void
call_stack::grow_capacity(std::size_t requested_capacity) {
  std::size_t new_cap = find_new_capacity(requested_capacity);
  auto new_data = std::make_unique<ptr<>[]>(new_cap);
  std::copy(data_.get(), data_.get() + size_, new_data.get());
  data_ = std::move(new_data);
  capacity_ = new_cap;
}

std::size_t
call_stack::find_new_capacity(std::size_t at_least) const {
  // Normally we expect to grow the capacity by just a single alloc_size, so
  // this loop will only do a single iteration.

  std::size_t result = capacity_;
  while (result < at_least)
    result += alloc_size;
  return result;
}

} // namespace insider
