#include "free_store.hpp"

#include <cassert>

#ifdef __GNUC__
#include <cxxabi.h>
#endif

namespace insider {

static constexpr std::size_t page_size = 4096;
static constexpr std::size_t nursery_min_pages = 1;
static constexpr std::size_t large_threshold = 256;

static std::vector<type_descriptor>&
types() {
  static std::vector<type_descriptor> value;
  return value;
}

enum class color : word_type {
  white = 0,
  black = 1,
};

// Object header word:
//
// Bits:   63 ..  2        1       0
// Fields: | type | colour | alive |

static constexpr word_type alive_shift = 0;
static constexpr word_type color_shift = 1;
static constexpr word_type type_shift = 2;

static constexpr word_type alive_bit = 1 << alive_shift;
static constexpr word_type color_bit = 1 << color_shift;

static word_type&
header_word(object* o) {
  assert(is_object_ptr(o));
  return *reinterpret_cast<word_type*>(reinterpret_cast<std::byte*>(o) - sizeof(word_type));
}

static void
init_object_header(std::byte* storage, word_type type) {
  new (storage) word_type(type << type_shift | alive_bit);
}

static word_type
type_index(word_type header) { return header >> type_shift; }

word_type
object_type_index(object* o) { return type_index(header_word(o)); }

std::string
type_name(word_type index) { return types()[index].name; }

static type_descriptor const&
object_type(word_type header) { return types()[type_index(header)]; }

type_descriptor const&
object_type(object* o) { return object_type(header_word(o)); }

static std::size_t
object_size(object* o) {
  type_descriptor const& t = object_type(o);
  return t.constant_size ? t.size : t.get_size(o);
}

static color
object_color(word_type header) {
  return static_cast<color>((header & color_bit) >> color_shift);
}

static color
object_color(object* o) { return object_color(header_word(o)); }

static void
set_object_color(object* o, color c) {
  header_word(o) = (header_word(o) & ~color_bit) | (static_cast<word_type>(c) << color_shift);
}

static bool
is_alive(word_type header) { return header & alive_bit; }

bool
is_alive(object* o) { return is_alive(header_word(o)); }

object*
forwarding_address(object* o) {
  assert(!is_alive(o));
  return reinterpret_cast<object*>(header_word(o));
}

bool
is_object_ptr(object* o) {
  return !(reinterpret_cast<word_type>(o) & 1);
}

word_type
fixnum_payload(object* o) {
  assert(!is_object_ptr(o));
  return reinterpret_cast<word_type>(o) >> 1;
}

object*
fixnum_to_ptr(word_type w) {
  return reinterpret_cast<object*>((w << 1) | 1);
}

static void
set_forwarding_address(object* from, object* target) {
  header_word(from) = reinterpret_cast<word_type>(target);
  assert((header_word(from) & alive_bit) == 0);
}

std::string
detail::demangle(char const* name) {
#ifdef __GNUC__
  struct free_deallocator {
    void
    operator () (char* p) { std::free(p); }
  };

  int status;
  std::unique_ptr<char, free_deallocator> result{abi::__cxa_demangle(name, nullptr, nullptr, &status)};

  if (status == 0)
    return std::string(result.get());
  else
    return std::string(name);

#else
  return std::string(name);
#endif
}

void
tracing_context::trace(object* o) {
  if (o && is_object_ptr(o) && object_color(o) == color::white) {
    assert(is_alive(o));
    assert(object_type_index(o) < types().size());

    set_object_color(o, color::black);
    stack_.push_back(o);
  }
}

word_type
new_type(type_descriptor d) {
  types().push_back(d);
  return types().size() - 1;
}

generic_ptr::generic_ptr(free_store& store, object* value)
  : generic_ptr_base{store, value}
{
  assert(!value_ || !is_object_ptr(value_) || is_alive(value_));
  store.register_root(this);
}

generic_ptr::generic_ptr(generic_ptr const& other)
  : generic_ptr_base(other)
{
  assert(store_ || !value_ || !is_object_ptr(value_));
  assert(!value_ || !is_object_ptr(value_) || is_alive(value_));

  if (store_)
    store_->register_root(this);
}

generic_ptr::generic_ptr(word_type payload) {
  value_ = fixnum_to_ptr(payload);
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
  assert(store_ || !value_ || !is_object_ptr(value_));
  assert(!value_ || !is_object_ptr(value_) || is_alive(value_));

  return *this;
}

generic_weak_ptr::generic_weak_ptr(free_store& store, object* value)
  : generic_ptr_base{store, value}
{
  if (value)
    store.register_weak(this);
}

generic_weak_ptr::generic_weak_ptr(generic_weak_ptr const& other)
  : generic_weak_ptr{other.store(), other.value_}
{
  assert(store_ || !value_);

  if (store_ && value_)
    store_->register_weak(this);
}

generic_weak_ptr::~generic_weak_ptr() {
  if (store_)
    store_->unregister_weak(this);
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
    store_->unregister_weak(this);

  value_ = value;

  if (value_)
    store_->register_weak(this);

  return *this;
}

generic_ptr
generic_weak_ptr::lock() const {
  return {*store_, value_};
}

static page
allocate_page() {
  return {std::make_unique<std::byte[]>(page_size), 0};
}

free_store::free_store() {
  for (std::size_t i = 0; i < nursery_min_pages; ++i) {
    nursery_fromspace_.pages.emplace_back(allocate_page());
    nursery_tospace_.pages.emplace_back(allocate_page());
  }
}

free_store::~free_store() {
  assert(roots_.empty());
  collect_garbage();
}

void
free_store::register_root(generic_ptr* root) {
  roots_.emplace(root);
}

void
free_store::unregister_root(generic_ptr* root) {
  roots_.erase(root);
}

void
free_store::register_weak(generic_weak_ptr* ptr) {
  weak_roots_.emplace(ptr);
}

void
free_store::unregister_weak(generic_weak_ptr* ptr) {
  weak_roots_.erase(ptr);
}

static void
trace(std::unordered_set<generic_ptr*> const& roots) {
  std::vector<object*> stack;
  tracing_context tc{stack};

  for (generic_ptr const* root : roots)
    if (*root && is_object_ptr(root->get()) && object_color(root->get()) == color::white) {
      assert(is_alive(root->get()));
      set_object_color(root->get(), color::black);
      stack.push_back(root->get());
    }

  while (!stack.empty()) {
    object* top = stack.back();
    stack.pop_back();
    object_type(top).trace(top, tc);
  }
}

static std::size_t
page_free(page const& p) {
  return page_size - p.used;
}

static std::byte*
allocate(space& s, std::size_t size) {
  assert(size < page_size);

  if (page_free(s.pages[s.current]) < size) {
    s.pages.emplace_back(allocate_page());
    s.current = s.pages.size() - 1;
  }

  std::byte* result = s.pages[s.current].storage.get() + s.pages[s.current].used;
  std::uninitialized_fill_n(result, size, std::byte{0});
  s.pages[s.current].used += size;
  return result;
}

static object*
move_object(object* o, space& to) {
  type_descriptor const& t = object_type(o);
  std::size_t size = sizeof(word_type) + object_size(o);
  std::byte* storage = allocate(to, size);

  init_object_header(storage, object_type_index(o));
  return t.move(o, storage + sizeof(word_type));
}

// Move live objects from `from` to `to` and destroy dead objects in
// `from`. After this function, all objects in from have been destroyed.
static void
migrate_objects(space const& from, space& to) {
  for (page const& from_page : from.pages) {
    std::size_t i = 0;
    while (i < from_page.used) {
      std::byte* storage = from_page.storage.get() + i;
      object* o = reinterpret_cast<object*>(storage + sizeof(word_type));
      type_descriptor const& t = object_type(o);
      std::size_t size = object_size(o);

      if (object_color(o) == color::black) {
        object* target = move_object(o, to);
        set_forwarding_address(o, target);

        assert(!is_alive(o));
        assert(is_alive(target));
        assert(object_color(target) == color::white);
      } else
        set_forwarding_address(o, nullptr);

      t.destroy(o);
      std::uninitialized_fill_n(storage + sizeof(word_type), size, std::byte{0xAA});
      i += size + sizeof(word_type);
    }
  }
}

static void
update_references(space const& s) {
  for (page const& p : s.pages) {
    std::size_t i = 0;
    while (i < p.used) {
      std::byte* storage = p.storage.get() + i;
      object* o = reinterpret_cast<object*>(storage + sizeof(word_type));
      object_type(o).update_references(o);
      i += object_size(o) + sizeof(word_type);
    }
  }
}

static void
update_references(large_space const& space) {
  for (auto const& storage : space) {
    object* o = reinterpret_cast<object*>(storage.get() + sizeof(word_type));
    object_type(o).update_references(o);
  }
}

static void
trim_space(space& s) {
  std::size_t new_size = std::max(nursery_min_pages,
                                  s.pages.size() - std::count_if(s.pages.begin(), s.pages.end(),
                                                                 [] (page const& p) { return p.used == 0; }));
  assert(std::all_of(s.pages.begin() + new_size, s.pages.end(), [] (page const& p) { return p.used == 0; }));
  s.pages.resize(new_size);
}

static void
clear_space(space& s) {
  for (page& p : s.pages)
    p.used = 0;
  s.current = 0;
}

static large_space
sweep_large(large_space& space) {
  std::size_t survivors = 0;
  for (auto& storage : space) {
    object* o = reinterpret_cast<object*>(storage.get() + sizeof(word_type));
    if (object_color(o) == color::white) {
      object_type(o).destroy(o);
      set_forwarding_address(o, nullptr);
    } else {
      set_object_color(o, color::white);
      ++survivors;
    }
  }

  std::vector<std::unique_ptr<std::byte[]>> new_space;
  new_space.reserve(survivors);

  for (auto& storage : space)
    if (is_alive(*reinterpret_cast<word_type*>(storage.get())))
      new_space.emplace_back(std::move(storage));

  return new_space;
}

void
free_store::collect_garbage() {
  if (disable_level_ > 0) {
    collection_requested_ = true;
    return;
  }

  trace(roots_);
  migrate_objects(nursery_fromspace_, nursery_tospace_);
  auto new_large = sweep_large(nursery_large_objects_);

  update_references(nursery_tospace_);
  update_references(new_large);
  update_roots();

  clear_space(nursery_fromspace_);
  trim_space(nursery_fromspace_);
  if (nursery_fromspace_.pages.size() > nursery_tospace_.pages.size())
    nursery_fromspace_.pages.resize(nursery_tospace_.pages.size());

  std::swap(nursery_fromspace_, nursery_tospace_);

  nursery_large_objects_ = std::move(new_large);

  assert(nursery_fromspace_.pages.size() >= nursery_min_pages);
  assert(nursery_tospace_.pages.size() >= nursery_min_pages);

  collection_requested_ = false;
}

void
free_store::disable_collection() {
  ++disable_level_;
}

void
free_store::enable_collection() {
  --disable_level_;

  if (disable_level_ == 0 && collection_requested_)
    collect_garbage();
}

std::byte*
free_store::allocate_object(std::size_t size, word_type type) {
  std::size_t total_size = size + sizeof(word_type);

  std::byte* storage = nullptr;
  if (total_size >= large_threshold)
    storage = nursery_large_objects_.emplace_back(std::make_unique<std::byte[]>(total_size)).get();
  else {
    if (page_free(nursery_fromspace_.pages[nursery_fromspace_.current]) < total_size)
      collect_garbage();

    storage = allocate(nursery_fromspace_, total_size);
  }

  init_object_header(storage, type);
  std::byte* object_storage = storage + sizeof(word_type);
  return object_storage;
}

void
free_store::update_roots() {
  for (generic_ptr* p : roots_) {
    if (p->get() && is_object_ptr(p->get()) && !is_alive(p->get())) {
      p->value_ = forwarding_address(p->get());
      assert(p->value_ != nullptr);
    }

    assert(!p->get() || !is_object_ptr(p->get()) || is_alive(p->get()));
    assert(!p->get() || !is_object_ptr(p->get()) || object_color(p->get()) == color::white);
  }

  for (generic_weak_ptr* wp : weak_roots_) {
    if (wp->get() && is_object_ptr(wp->get()) && !is_alive(wp->get()))
      wp->value_ = forwarding_address(wp->get());
  }
}

} // namespace insider
