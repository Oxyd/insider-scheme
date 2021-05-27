#include "call_stack.hpp"

#include "free_store.hpp"

#include <stdexcept>

namespace insider {

static constexpr std::size_t stack_size = 4 * 1024 * 1024;

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

stack_cache::stack_cache()
  : storage_{std::make_unique<std::byte[]>(stack_size)}
  , top_{storage_.get()}
{ }

ptr<stack_frame>
stack_cache::make(std::size_t num_locals, ptr<> callable, ptr<stack_frame> parent, integer::value_type previous_pc) {
  std::size_t size = sizeof(word_type) + sizeof(stack_frame) + num_locals * sizeof(ptr<>);
  std::size_t used_stack_size = top_ - storage_.get();

  if (size > stack_size - used_stack_size)
    throw std::runtime_error{"Stack exhausted"};

  std::byte* storage = top_;
  top_ += size;

  init_object_header(storage, stack_frame::type_index, generation::stack);
  return new (storage + sizeof(word_type)) stack_frame(num_locals, callable, parent, previous_pc);
}

void
stack_cache::deallocate(ptr<stack_frame> frame) {
  if (object_generation(frame) != generation::stack)
    return;

  assert(reinterpret_cast<std::byte*>(frame.value()) + object_size(frame) == top_);
  top_ = reinterpret_cast<std::byte*>(frame.value()) - sizeof(word_type);
}

} // namespace insider
