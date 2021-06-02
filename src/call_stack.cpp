#include "call_stack.hpp"

#include "free_store.hpp"

namespace insider {

stack_frame::stack_frame(stack_frame&& other)
  : previous_pc{other.previous_pc}
  , parent{other.parent}
  , callable{other.callable}
  , num_locals_{other.num_locals_}
{
  for (std::size_t i = 0; i < num_locals_; ++i)
    storage_element(i) = other.storage_element(i);
}

void
stack_frame::visit_members(member_visitor const& f) {
  f(parent);
  f(callable);
  for (std::size_t i = 0; i < num_locals_; ++i)
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
