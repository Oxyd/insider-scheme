#include "call_stack.hpp"

#include "context.hpp"
#include "memory/free_store.hpp"
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
  : data_{std::make_unique<ptr<>[]>(other.capacity_)}
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
    data_ = std::move(copy.data_);
    capacity_ = copy.capacity_;
    size_ = copy.size_;
    current_base_ = copy.current_base_;
  }

  return *this;
}

void
call_stack::push_frame(ptr<> callable, std::size_t locals_size,
                       integer::value_type previous_pc,
                       ptr<stack_frame_extra_data> extra) {
  auto new_base = static_cast<frame_index>(size_);
  ensure_additional_capacity(stack_frame_header_size + locals_size);
  size_ += stack_frame_header_size
           + static_cast<integer::value_type>(locals_size);

  data_[new_base + previous_base_offset] = integer_to_ptr(current_base_);
  data_[new_base + previous_pc_offset] = integer_to_ptr(previous_pc);
  data_[new_base + callable_offset] = callable;
  data_[new_base + extra_data_offset] = extra;
  current_base_ = new_base;
}

void
call_stack::resize_current_frame(std::size_t new_size) {
  size_ = current_base_ + stack_frame_header_size
          + static_cast<integer::value_type>(new_size);
  if (size_ > capacity_) [[unlikely]]
    grow_capacity(size_);
}

void
call_stack::move_tail_call_arguments(std::size_t new_args_size,
                                     std::size_t old_args_size) {
  frame_index parent_base = parent(current_frame_index());

  integer::value_type dest_begin
    = current_frame_index() - to_signed<integer::value_type>(old_args_size);
  integer::value_type dest_end
    = dest_begin + to_signed<integer::value_type>(new_args_size);
  integer::value_type src_begin
    = size_ - to_signed<integer::value_type>(new_args_size);
  integer::value_type src_end = size_;

  std::copy(data_.get() + src_begin, data_.get() + src_end,
            data_.get() + dest_begin);

  current_base_ = parent_base;
  size_ = dest_end;
}

auto
call_stack::frames(frame_index begin, frame_index end) const
  -> frame_span
{
  assert(end >= 0);
  assert(end >= begin);

  return {object_span{data_.get() + begin, static_cast<std::size_t>(end - begin)},
          find_last_frame_base(end),
          begin};
}

void
call_stack::append_frames(frame_span frames) {
  ensure_additional_capacity(frames.data.size());
  std::ranges::copy(frames.data, data_.get() + size_);
  size_ += static_cast<frame_index>(frames.data.size());
  fix_base_offsets(frames);
}

void
call_stack::visit_members(member_visitor const& f) {
  for (frame_index i = 0; i < size_; ++i)
    f(data_[i]);

  // Clear data in the allocated area after the current end of stack. This is
  // because if the collector moves or deallocates those members, and then the
  // stack grows, we now have live invalid pointers. If another collection
  // happens before those invalid pointers are reassigned, we'll be feeding
  // invalid pointers to the GC which will crash.

  std::fill(data_.get() + size_, data_.get() + capacity_, ptr<>{});
}

void
call_stack::ensure_additional_capacity(std::size_t additional_size) {
  integer::value_type required_size
    = size_ + static_cast<integer::value_type>(additional_size);
  if (required_size >= capacity_) [[unlikely]]
    grow_capacity(size_ + additional_size);
}

void
call_stack::grow_capacity(std::size_t requested_capacity) {
  frame_index new_cap = find_new_capacity(requested_capacity);
  auto new_data = std::make_unique<ptr<>[]>(new_cap);
  std::copy(data_.get(), data_.get() + size_, new_data.get());
  data_ = std::move(new_data);
  capacity_ = new_cap;
}

call_stack::frame_index
call_stack::find_new_capacity(std::size_t at_least) const {
  // Normally we expect to grow the capacity by just a single alloc_size, so
  // this loop will only do a single iteration.

  frame_index result = capacity_;
  while (result < static_cast<frame_index>(at_least))
    result += alloc_size;
  return result;
}

call_stack::frame_index
call_stack::find_last_frame_base(frame_index end) const {
  if (end < size_)
    return assume<integer>(data_[end + previous_base_offset]).value();
  else
    return current_base_;
}

void
call_stack::fix_base_offsets(frame_span const& frames) {
  frame_index old_end = size_ - static_cast<frame_index>(frames.data.size());
  frame_index base_delta = old_end - frames.first_frame_base;

  frame_index current = frames.last_frame_base + base_delta;
  while (current > old_end) {
    frame_index old_offset
      = expect<integer>(data_[current + previous_base_offset]).value();
    frame_index new_offset = old_offset > -1 ?  old_offset + base_delta : -1;
    data_[current + previous_base_offset] = integer_to_ptr(new_offset);
    current = new_offset;
  }

  if (current != -1)
    data_[current + previous_base_offset] = integer_to_ptr(current_base_);
  current_base_ = frames.last_frame_base + base_delta;
}

} // namespace insider
