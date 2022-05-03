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

void
call_stack::push_frame(ptr<> callable, std::size_t locals_size) {
  integer::value_type new_base = data_.size();
  data_.resize(data_.size() + stack_frame_header_size + locals_size);
  data_[new_base + previous_base_offset] = integer_to_ptr(current_base_);
  data_[new_base + callable_offset] = callable;
  current_base_ = new_base;
}

void
call_stack::pop_frame() {
  integer::value_type old_base = current_base_;
  current_base_ = assume<integer>(data_[current_base_ + previous_base_offset]).value();
  assert(current_base_ == -1
         || old_base - current_base_ >= static_cast<integer::value_type>(stack_frame_header_size));
  data_.resize(old_base);
}

void
call_stack::move_current_frame_up() {
  integer::value_type old_base = current_base_;
  integer::value_type new_base = parent(old_base);
  std::size_t current_frame_size = data_.size() - old_base;

  for (std::size_t i = stack_frame_header_size; i < current_frame_size; ++i)
    data_[new_base + i] = data_[old_base + i];

  current_base_ = new_base;
  data_.resize(current_base_ + current_frame_size);
}

object_span
call_stack::current_locals_span() {
  std::size_t locals_start = current_base_ + stack_frame_header_size;
  return {&data_[locals_start], data_.size() - locals_start};
}

auto
call_stack::frames(integer::value_type begin, integer::value_type end) const -> frame_span {
  assert(end >= 0);
  assert(end >= begin);

  return {object_span{data_.data() + begin, static_cast<std::size_t>(end - begin)},
          find_last_frame_base(end),
          begin};
}

integer::value_type
call_stack::find_last_frame_base(integer::value_type end) const {
  if (static_cast<std::size_t>(end) < data_.size())
    return assume<integer>(data_[end + previous_base_offset]).value();
  else
    return current_base_;
}

void
call_stack::append_frames(frame_span frames) {
  data_.insert(data_.end(), frames.data.begin(), frames.data.end());
  fix_base_offsets(frames);
}

void
call_stack::fix_base_offsets(frame_span const& frames) {
  integer::value_type old_end = data_.size() - frames.data.size();
  integer::value_type base_delta = old_end - frames.first_frame_base;

  integer::value_type current = frames.last_frame_base + base_delta;
  while (current > old_end) {
    integer::value_type new_offset = expect<integer>(data_[current + previous_base_offset]).value() + base_delta;
    data_[current + previous_base_offset] = integer_to_ptr(new_offset);
    current = new_offset;
  }

  assert(current == old_end);
  data_[current + previous_base_offset] = integer_to_ptr(current_base_);
  current_base_ = frames.last_frame_base + base_delta;
}

void
call_stack::visit_members(member_visitor const& f) {
  for (ptr<>& x : data_)
    f(x);
}

} // namespace insider
