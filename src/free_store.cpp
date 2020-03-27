#include "free_store.hpp"

#include <cassert>

namespace scm {

bool
object::eqv(generic_ptr const& other) const {
  return this == other.get();
}

generic_ptr::generic_ptr(free_store& store, object* value)
  : generic_ptr_base{store, value}
{
  store.register_root(this);
}

generic_ptr::generic_ptr(generic_ptr const& other)
  : generic_ptr_base(other)
{
  assert(store_ || !value_);

  if (store_)
    store_->register_root(this);
}

generic_ptr::~generic_ptr() {
  if (store_)
    store_->unregister_root(this);
}

generic_ptr&
generic_ptr::operator = (generic_ptr const& other) {
  if (this == &other)
    return *this;

  if (store_ != other.store_) {
    if (store_)
      store_->unregister_root(this);

    store_ = other.store_;
    if (store_)
      store_->register_root(this);
  }

  value_ = other.value_;
  assert(store_ || !value_);

  return *this;
}

generic_weak_ptr::generic_weak_ptr(free_store& store, object* value)
  : generic_ptr_base{store, value}
{
  if (value)
    store.register_weak(this, value);
}

generic_weak_ptr::generic_weak_ptr(generic_weak_ptr const& other)
  : generic_weak_ptr{other.store(), other.value_}
{
  assert(store_ || !value_);

  if (store_ && value_)
    store_->register_weak(this, value_);
}

generic_weak_ptr::~generic_weak_ptr() {
  if (value_ && store_)
    store_->unregister_weak(this, value_);
}

generic_weak_ptr&
generic_weak_ptr::operator = (generic_weak_ptr const& other) {
  if (this == &other)
    return *this;
  else
    return operator = (other.value_);
}

generic_weak_ptr&
generic_weak_ptr::operator = (object* value) {
  if (value_)
    store_->unregister_weak(this, value_);

  value_ = value;

  if (value_)
    store_->register_weak(this, value_);

  return *this;
}

generic_ptr
generic_weak_ptr::lock() const {
  return {*store_, value_};
}

static void
destroy(object* o) {
  o->~object();
  delete [] reinterpret_cast<std::byte*>(o);
}

free_store::~free_store() {
  for (object* o : objects_)
    destroy(o);
}

void
free_store::register_root(generic_ptr* root) {
  roots_.emplace(root);
}

void
free_store::unregister_root(generic_ptr* root) {
  roots_.erase(root);

#ifndef NDEBUG
  collect_garbage();
#endif
}

void
free_store::register_weak(generic_weak_ptr* ptr, object* pointee) {
  weak_ptrs_.emplace(pointee, ptr);
}

void
free_store::unregister_weak(generic_weak_ptr* ptr, object* pointee) {
  auto [begin, end] = weak_ptrs_.equal_range(pointee);
  for (auto it = begin; it != end; ++it)
    if (it->second == ptr) {
      weak_ptrs_.erase(it);
      return;
    }
}

static void
mark(std::unordered_set<generic_ptr*> const& roots, bool current_mark) {
  std::vector<object*> stack;
  for (generic_ptr const* root : roots)
    if (*root)
      stack.push_back(root->get());

  while (!stack.empty()) {
    object* top = stack.back();
    stack.pop_back();

    if (top->mark != current_mark) {
      top->mark = current_mark;
      top->for_each_subobject([&] (object* subobject) {
        if (subobject && subobject->mark != current_mark)
          stack.push_back(subobject);
      });
    }
  }
}

static void
sweep(std::vector<object*>& objects,
      std::unordered_multimap<object*, generic_weak_ptr*>& weak_ptrs,
      bool current_mark) {
  auto live_end = std::partition(objects.begin(), objects.end(),
                                 [&] (auto const& object) {
                                   return object->mark == current_mark;
                                 });
  for (auto dead = live_end; dead != objects.end(); ++dead) {
    auto [weak_begin, weak_end] = weak_ptrs.equal_range(*dead);
    for (auto weak_ptr = weak_begin; weak_ptr != weak_end; ++weak_ptr)
      weak_ptr->second->reset();

    weak_ptrs.erase(weak_begin, weak_end);
    destroy(*dead);
  }

  objects.erase(live_end, objects.end());
}

void
free_store::collect_garbage() {
  current_mark_ = !current_mark_;
  mark(roots_, current_mark_);
  sweep(objects_, weak_ptrs_, current_mark_);
}

} // namespace scm
