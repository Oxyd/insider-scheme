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
call_stack::push_frame(ptr<> callable, std::size_t locals_size,
                       integer::value_type previous_pc, operand result_register,
                       ptr<stack_frame_extra_data> extra) {
  assert(!callable || is_procedure(callable));

  std::size_t new_base = size_;
  size_ += locals_size;
  frames_.emplace_back(frame{new_base, locals_size, previous_pc,
                             result_register, callable, extra});

  assert(size_ == frames_.back().base + frames_.back().size);
}

void
call_stack::pop_frame() {
  assert(frames_.back().size <= size_);

  frame old_frame = frames_.back();
  frames_.pop_back();
  size_ -= old_frame.size;
}

void
call_stack::resize_current_frame(std::size_t new_size) {
  frames_.back().size = new_size;
  size_ = frames_.back().base + new_size;
  if (size_ > capacity_) [[unlikely]]
    grow_capacity(size_);
}

void
call_stack::move_tail_call_arguments_and_pop_frame(std::size_t new_args_size,
                                                   std::size_t old_args_size) {
  assert(!frames_.empty());
  std::size_t current_base = frames_.back().base;

  assert(current_base >= old_args_size);
  assert(size_ >= new_args_size);

  std::size_t dest_begin = current_base - old_args_size;
  std::size_t dest_end   = dest_begin + new_args_size;
  std::size_t src_begin  = size_ - new_args_size;
  std::size_t src_end    = size_;

  std::copy(data_.get() + src_begin, data_.get() + src_end,
            data_.get() + dest_begin);

  frames_.pop_back();
  size_ = dest_end;
}

auto
call_stack::frames(frame_index begin, frame_index end) const -> frame_span {
  assert(end >= begin);
  return {this, {&frames_[begin], &frames_[end]},
          end == frames_.size() ? size_ : frames_[end].base};
}

void
call_stack::append_frames(frame_span frames) {
  if (frames.frames.empty())
    return;

  std::size_t frames_begin = frames.frames.front().base;
  std::size_t frames_end = frames.last_frame_real_end;
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

  std::size_t last_frame_size
    = frames.last_frame_real_end - frames.frames.back().base;
  size_ = frames_.back().base + last_frame_size;
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
