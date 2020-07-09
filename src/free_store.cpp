#include "free_store.hpp"

#include <cassert>

namespace insider {

static std::vector<type_descriptor>&
types() {
  static std::vector<type_descriptor> value;
  return value;
}

// Object header word:
//
// Bits:   63 ..  1        0
// Fields: | type | colour |

static word_type color_bit = 1 << 0;

static word_type&
header_word(object* o) {
  return *reinterpret_cast<word_type*>(reinterpret_cast<std::byte*>(o) - sizeof(word_type));
}

void
init_object_header(std::byte* storage, word_type type) {
  new (storage) word_type(type << 1);
}

word_type
object_type_index(object* o) { return header_word(o) >> 1; }

static type_descriptor const&
object_type(object* o) { return types()[object_type_index(o)]; }

static detail::color
object_color(object* o) { return static_cast<detail::color>(header_word(o) & 1); }

static void
set_object_color(object* o, detail::color c) {
  header_word(o) = (header_word(o) & ~color_bit) | static_cast<word_type>(c);
}

void
tracing_context::trace(object* o) {
  if (o && object_color(o) != current_color_)
    stack_.push_back(o);
}

word_type
new_type(type_descriptor d) {
  types().push_back(d);
  return types().size() - 1;
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
  object_type(o).destroy(o);
  delete [] (reinterpret_cast<std::byte*>(o) - sizeof(word_type));
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
#ifndef NDEBUG
  // Unregistering a root during a collection means there is a generic_ptr owned
  // (perhaps indirectly) by a Scheme object. This is not how we want things to
  // work.

  assert(!collecting_);
#endif

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
mark(std::unordered_set<generic_ptr*> const& roots, detail::color current_color) {
  std::vector<object*> stack;
  tracing_context tc{stack, current_color};

  for (generic_ptr const* root : roots)
    if (*root)
      stack.push_back(root->get());

  while (!stack.empty()) {
    object* top = stack.back();
    stack.pop_back();

    if (object_color(top) != current_color) {
      set_object_color(top, current_color);
      object_type(top).trace(top, tc);
    }
  }
}

static void
sweep(std::vector<object*>& objects,
      std::unordered_multimap<object*, generic_weak_ptr*>& weak_ptrs,
      detail::color current_color) {
  auto live_end = std::partition(objects.begin(), objects.end(),
                                 [&] (auto const& object) {
                                   return object_color(object) == current_color;
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
#ifndef NDEBUG
  collecting_ = true;

  struct guard {
    free_store* fs;

    ~guard() { fs->collecting_ = false; }
  } guard{this};
#endif

  if (current_color_ == detail::color::white)
    current_color_ = detail::color::black;
  else
    current_color_ = detail::color::white;

  mark(roots_, current_color_);
  sweep(objects_, weak_ptrs_, current_color_);
}

object*
free_store::add(std::unique_ptr<std::byte[]> storage, object* result) {
  try {
    set_object_color(result, current_color_);
    objects_.push_back(result);
  } catch (...) {
    object_type(result).destroy(result);
    throw;
  }

  storage.release();
  return result;
}

} // namespace insider
