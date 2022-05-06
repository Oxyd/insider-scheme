#include "call_stack.hpp"

#include "context.hpp"
#include "memory/free_store.hpp"

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
  std::copy(other.data_.get(), other.data_.get() + size_, data_.get());
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
                       integer::value_type previous_pc) {
  auto new_base = static_cast<frame_index>(size_);
  ensure_additional_capacity(stack_frame_header_size + locals_size);
  size_ += stack_frame_header_size + locals_size;

  data_[new_base + previous_base_offset] = integer_to_ptr(current_base_);
  data_[new_base + previous_pc_offset] = integer_to_ptr(previous_pc);
  data_[new_base + callable_offset] = callable;
  data_[new_base + extra_data_offset] = {};
  current_base_ = new_base;
}

void
call_stack::resize_current_frame(std::size_t new_size) {
  size_ = current_base_ + stack_frame_header_size + new_size;
  if (size_ < capacity_)
    grow_capacity(size_);
}

void
call_stack::move_current_frame_up() {
  frame_index old_base = current_base_;
  frame_index new_base = parent(old_base);
  std::size_t current_frame_size = size_ - old_base;

  for (std::size_t i = stack_frame_header_size; i < current_frame_size; ++i)
    data_[new_base + i] = data_[old_base + i];

  current_base_ = new_base;
  size_ = current_base_ + current_frame_size;
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
  std::copy(frames.data.begin(), frames.data.end(), data_.get() + size_);
  size_ += frames.data.size();
  fix_base_offsets(frames);
}

void
call_stack::visit_members(member_visitor const& f) {
  for (std::size_t i = 0; i < size_; ++i)
    f(data_[i]);
}

void
call_stack::ensure_additional_capacity(std::size_t additional_size) {
  if (size_ + additional_size >= capacity_)
    grow_capacity(size_ + additional_size);
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

call_stack::frame_index
call_stack::find_last_frame_base(frame_index end) const {
  if (static_cast<std::size_t>(end) < size_)
    return assume<integer>(data_[end + previous_base_offset]).value();
  else
    return current_base_;
}

void
call_stack::fix_base_offsets(frame_span const& frames) {
  frame_index old_end = size_ - frames.data.size();
  frame_index base_delta = old_end - frames.first_frame_base;

  frame_index current = frames.last_frame_base + base_delta;
  while (current > old_end) {
    frame_index new_offset = expect<integer>(
      data_[current + previous_base_offset]
    ).value() + base_delta;
    data_[current + previous_base_offset] = integer_to_ptr(new_offset);
    current = new_offset;
  }

  assert(current == old_end);
  data_[current + previous_base_offset] = integer_to_ptr(current_base_);
  current_base_ = frames.last_frame_base + base_delta;
}

} // namespace insider
