#include "call_stack.hpp"

#include "free_store.hpp"

namespace insider {

void
stack_frame_extra_data::visit_members(member_visitor const &f) {
  f(parameter_tag);
  f(parameter_value);
  f(before_thunk);
  f(after_thunk);
  f(exception_handler);
  if (next_exception_handler_frame)
    f(*next_exception_handler_frame);
}

stack_frame::stack_frame(stack_frame&& other)
  : dynamic_size_object{other.size_}
  , previous_pc{other.previous_pc}
  , parent{other.parent}
  , callable{other.callable}
  , extra{other.extra}
{
  for (std::size_t i = 0; i < size_; ++i)
    storage_element(i) = other.storage_element(i);
}

void
stack_frame::visit_members(member_visitor const& f) {
  f(parent);
  f(callable);
  f(extra);

  for (std::size_t i = 0; i < size_; ++i)
    f(storage_element(i));
}

stack_cache::stack_cache(free_store& store)
  : store_{store}
  , storage_{store_.allocator().allocate()}
  , top_{storage_.get()}
{ }

void
stack_cache::transfer_to_nursery() {
  std::size_t size = used_size();
  store_.transfer_to_nursery(std::move(storage_), size);
  storage_ = store_.allocator().allocate();
  top_ = storage_.get();
}

} // namespace insider
