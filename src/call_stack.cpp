#include "call_stack.hpp"

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
{ }

std::byte*
stack_cache::allocate(std::size_t size) {
  assert(size >= 2 * sizeof(word_type));

  if (size >= stack_size - top_)
    throw std::runtime_error{"Stack exhausted"};

  std::byte* result = storage_.get() + top_;
  top_ += size;
  return result;
}

void
stack_cache::deallocate(std::byte* x, std::size_t allocation_size) {
  if (storage_.get() + top_ != x + allocation_size)
    throw std::runtime_error{"Deallocating an object not on top of the stack"};

  assert(top_ >= allocation_size);
  top_ -= allocation_size;
}

} // namespace insider
