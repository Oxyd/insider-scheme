#include "call_stack.hpp"

#include "memory/free_store.hpp"
#include "runtime/basic_types.hpp"
#include "util/integer_cast.hpp"

#include <algorithm>
#include <iterator>
#include <ranges>

namespace insider {

void
stack_frame_extra_data::visit_members(member_visitor const& f) const {
  for (auto const& p : parameters) {
    f(p.tag);
    f(p.value);
  }
  f(before_thunk);
  f(after_thunk);
  f(exception_handler);
}

call_stack::call_stack()
  : data_{std::make_unique<ptr<>[]>(data_alloc_size)}
  , data_capacity_{data_alloc_size}
{
  frames_.reserve(frames_alloc_size);
}

call_stack::call_stack(call_stack const& other)
  : data_{std::make_unique<ptr<>[]>(other.real_data_size())}
  , data_capacity_{other.real_data_size()}
  , data_size_{other.data_size_}
  , current_base_{other.current_base_}
{
  std::ranges::copy(other.frames_, std::back_inserter(frames_));
  std::ranges::copy(std::views::counted(other.data_.get(), data_size_),
                    data_.get());
}

void
call_stack::resize_current_frame(register_index new_size) {
  current_frame().size = new_size;
  update_current_frame();
}

auto
call_stack::frames(frame_index begin, frame_index end) const -> frame_span {
  assert(end >= begin);
  return {this, {&frames_[begin], &frames_[end]}};
}

void
call_stack::append_frame(ptr<call_stack> from, frame_index idx) {
  ensure_data_capacity(data_size_ + from->frame_size(idx));

  frame new_frame = from->frames_[idx];
  std::copy(from->data_.get() + new_frame.base,
            from->data_.get() + new_frame.base + new_frame.size,
            data_.get() + data_size_);

  int base_diff = to_signed<int>(data_size_) - new_frame.base;

  new_frame.base += base_diff;
  frames_.push_back(new_frame);

  current_base_ = new_frame.base;
  data_size_ = current_base_ + new_frame.size;
}

void
call_stack::visit_members(member_visitor const& f) const {
  for (frame const& frame : frames_)
    f(frame.extra);

  for (std::size_t i = 0; i < data_size_; ++i)
    f(data_[i]);

  // Clear data in the allocated area after the current end of stack. This is
  // because if the collector moves or deallocates those members, and then the
  // stack grows, we now have live invalid pointers. If another collection
  // happens before those invalid pointers are reassigned, we'll be feeding
  // invalid pointers to the GC which will crash.

  std::fill(data_.get() + data_size_, data_.get() + data_capacity_, ptr<>{});
}

static auto
find_new_capacity(auto old_capacity, auto at_least,
                  auto alloc_size) {
  // Normally we expect to grow the capacity by just a single alloc_size, so
  // this loop will only do a single iteration.

  auto result = old_capacity;
  while (result < at_least)
    result += alloc_size;
  return result;
}

template <typename T>
static void
grow(std::unique_ptr<T[]>& container, auto& capacity,
     auto requested_size, auto alloc_size) {
  auto new_cap = find_new_capacity(capacity, requested_size, alloc_size);
  auto new_data = std::make_unique<T[]>(new_cap);
  std::copy(container.get(), container.get() + capacity, new_data.get());
  container = std::move(new_data);
  capacity = new_cap;
}

void
call_stack::grow_data(register_index requested_size) {
  grow<ptr<>>(data_, data_capacity_, requested_size, data_alloc_size);
}

register_index
call_stack::real_data_size() const {
  register_index result = 0;
  for (frame const& f : frames_)
    result = std::max(result, f.base + f.size);
  return result;
}

} // namespace insider
