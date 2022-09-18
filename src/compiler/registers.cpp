#include "compiler/registers.hpp"

#include <ranges>

namespace insider {

shared_register
register_allocator::allocate_register() {
  if (registers_free_.empty())
    return {*this, operand{registers_max_++}};
  else {
    auto result = operand{*registers_free_.begin()};
    registers_free_.erase(registers_free_.begin());
    return {*this, result};
  }
}

shared_register
register_allocator::allocate_argument_register() {
  operand result = first_argument_register();
  if (result == registers_max_)
    ++registers_max_;
  else
    registers_free_.erase(result);
  return {*this, result};
}

void
register_allocator::ref(operand o) {
  ++register_ref_counts_[o];
}

void
register_allocator::unref(operand o) {
  auto ref_count = register_ref_counts_.find(o);
  assert(ref_count != register_ref_counts_.end());

  if (--ref_count->second == 0) {
    registers_free_.emplace(o);
    register_ref_counts_.erase(ref_count);
  }
}

operand
register_allocator::first_argument_register() const {
  operand r = registers_max_;
  while (r > 0 && registers_free_.contains(operand(r - 1)))
    --r;
  return r;
}

shared_register::shared_register(register_allocator& alloc, operand value)
  : alloc_{&alloc}
  , value_{value}
{
  alloc_->ref(*value_);
}

shared_register::shared_register(shared_register const& other)
  : alloc_{other.alloc_}
  , value_{other.value_}
{
  if (alloc_)
    alloc_->ref(*value_);
}

shared_register::shared_register(shared_register&& other) noexcept
  : alloc_{other.alloc_}
  , value_{other.value_}
{
  other.alloc_ = nullptr;
}

shared_register::~shared_register() {
  if (alloc_)
    alloc_->unref(*value_);
}

shared_register&
shared_register::operator = (shared_register&& other) noexcept {
  if (alloc_)
    alloc_->unref(*value_);

  alloc_ = other.alloc_;
  value_ = other.value_;
  other.alloc_ = nullptr;

  return *this;
}

auto
variable_bindings::push_scope(scope s) -> unique_scope {
  scopes_.emplace_back(std::move(s));
  return unique_scope{this};
}

void
variable_bindings::pop_scope() {
  scopes_.pop_back();
}

shared_register
variable_bindings::lookup(ptr<local_variable> const& v) const {
  for (auto const& scope : scopes_ | std::views::reverse)
    for (binding const& b : scope)
      if (b.variable == v)
        return b.destination;

  assert(false); // TODO: This shouldn't ever happen any longer.
  return {};
}

bool
variable_bindings::register_is_store_for_variable(shared_register const& loc) const {
  for (auto const& scope : scopes_)
    for (binding const& b : scope)
      if (b.destination == loc)
        return true;
  return false;
}

} // namespace insider
