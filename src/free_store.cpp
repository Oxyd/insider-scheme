#include "free_store.hpp"

#include <algorithm>
#include <cassert>

#ifdef __GNUC__
#include <cxxabi.h>
#endif

#include <fmt/format.h>

namespace insider {

static constexpr std::size_t page_size = 4096;
static constexpr std::size_t large_threshold = 256;
static constexpr std::size_t min_nursery_pages = 1024;
static constexpr std::size_t nursery_reserve_pages = 10;
static constexpr std::size_t mature_reserve_pages = 10;
static constexpr std::size_t major_collection_frequency = 32;

static std::vector<type_descriptor>&
types() {
  static std::vector<type_descriptor> value;
  return value;
}

enum class color : word_type {
  white = 0,
  grey = 1,
  black = 2,
};

// Object header word:
//
// Bits:   63 ..  5    ..      3   ..   1       0
// Fields: | type | generation | colour | alive |

static constexpr word_type alive_shift = 0;
static constexpr word_type color_shift = 1;
static constexpr word_type generation_shift = 3;
static constexpr word_type type_shift = 5;

static constexpr word_type alive_bit = 1 << alive_shift;
static constexpr word_type color_bits = (1 << color_shift) | (1 << (color_shift + 1));
static constexpr word_type generation_bits = (1 << generation_shift) | (1 << (generation_shift + 1));

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

std::size_t
object_size(object* o) {
  type_descriptor const& t = object_type(o);
  return t.constant_size ? t.size : t.get_size(o);
}

static color
object_color(word_type header) {
  return static_cast<color>((header & color_bits) >> color_shift);
}

static color
object_color(object* o) { return object_color(header_word(o)); }

static void
set_object_color(object* o, color c) {
  header_word(o) = (header_word(o) & ~color_bits) | (static_cast<word_type>(c) << color_shift);
}

static bool
is_alive(word_type header) { return header & alive_bit; }

bool
is_alive(object* o) { return is_alive(header_word(o)); }

static word_type
get_generation(word_type header) {
  return (header & generation_bits) >> generation_shift;
}

word_type
object_generation(object* o) { return get_generation(header_word(o)); }

static void
set_object_generation(object* o, word_type gen) {
  header_word(o) = (header_word(o) & ~generation_bits) | (static_cast<word_type>(gen) << generation_shift);
}

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
fixnum_to_ptr(word_type w) noexcept {
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
  if (o && is_object_ptr(o) && object_color(o) == color::white
      && object_generation(o) <= max_generation_) {
    assert(is_alive(o));
    assert(object_type_index(o) < types().size());

    stack_.push_back(o);
    set_object_color(o, color::grey);
  }
}

word_type
new_type(type_descriptor d) {
  types().push_back(d);
  return types().size() - 1;
}

static page
allocate_page() {
  return {std::make_unique<std::byte[]>(page_size), 0};
}

page
page_allocator::allocate() {
  if (!reserve_.empty()) {
    page result = std::move(reserve_.back());
    reserve_.pop_back();

    result.used = 0;
    return result;
  }

  ++allocated_pages_;
  return allocate_page();
}

void
page_allocator::deallocate(page p) {
  reserve_.emplace_back(std::move(p));
}

void
page_allocator::keep_at_most(std::size_t n) {
  if (reserve_.size() > n) {
    deallocated_pages_ += reserve_.size() - n;
    reserve_.resize(n);
  }
}

dense_space::dense_space(page_allocator& pa)
  : allocator_{&pa}
{ }

dense_space::~dense_space() {
  clear();
}

dense_space::dense_space(dense_space&& other) noexcept
  : allocator_{other.allocator_}
  , pages_{std::move(other.pages_)}
  , total_used_{other.total_used_}
{
  other.pages_.clear();
  other.total_used_ = 0;
}

dense_space&
dense_space::operator = (dense_space&& other) noexcept {
  clear();

  pages_ = std::move(other.pages_);
  total_used_ = other.total_used_;
  allocator_ = other.allocator_;

  other.pages_.clear();
  other.total_used_ = 0;

  return *this;
}

static std::size_t
page_free(page const& p) {
  return page_size - p.used;
}

std::byte*
dense_space::allocate(std::size_t size) {
  assert(size < page_size);

  if (pages_.empty() || page_free(pages_.back()) < size)
    pages_.emplace_back(allocator_->allocate());

  page& p = pages_.back();
  std::byte* result = p.storage.get() + p.used;
  std::uninitialized_fill_n(result, size, std::byte{0});

  p.used += size;
  total_used_ += size;

  return result;
}

void
dense_space::clear() {
  assert(empty() || allocator_);

  if (!empty())
    for (page& p : pages_)
      allocator_->deallocate(std::move(p));

  total_used_ = 0;
}

bool
dense_space::has_preallocated_storage(std::size_t size) const {
  return !pages_.empty() && page_free(pages_.back()) >= size;
}

static object*
move_object(object* o, dense_space& to) {
  type_descriptor const& t = object_type(o);
  std::size_t size = sizeof(word_type) + object_size(o);
  std::byte* storage = to.allocate(size);

  init_object_header(storage, object_type_index(o));
  return t.move(o, storage + sizeof(word_type));
}

free_store::free_store()
  : target_nursery_pages_{min_nursery_pages}
{ }

free_store::~free_store() {
  assert(!roots_->next());
  assert(!roots_->prev());
  collect_garbage();
}

static void
trace(generic_ptr* roots, generation_list const& gens, word_type max_generation) {
  std::vector<object*> stack;
  tracing_context tc{stack, max_generation};

  for (generic_ptr* root = roots; root; root = root->next())
    if (root->get() && is_object_ptr(root->get())
        && object_generation(root->get()) <= max_generation
        && object_color(root->get()) == color::white) {
      assert(is_alive(root->get()));
      set_object_color(root->get(), color::grey);
      stack.push_back(root->get());
    }

  for (word_type g = 0; g <= max_generation; ++g)
    for (object* o : gens[g].incoming_arcs) {
      assert(is_object_ptr(o));
      assert(object_generation(o) > g);

      object_type(o).trace(o, tc);
    }

  while (!stack.empty()) {
    object* top = stack.back();
    stack.pop_back();

    assert(object_color(top) != color::white);
    if (object_color(top) == color::grey) {
      object_type(top).trace(top, tc);
      set_object_color(top, color::black);
    }
  }
}

static void
move_survivors(dense_space& from, dense_space& to, word_type to_gen) {
  from.for_all([&] (object* o) {
    type_descriptor const& t = object_type(o);
    std::size_t size = object_size(o);

    assert(object_color(o) != color::grey);
    if (object_color(o) == color::black) {
      object* target = move_object(o, to);
      set_forwarding_address(o, target);
      set_object_generation(target, to_gen);
    } else
      set_forwarding_address(o, nullptr);

    t.destroy(o);
    std::uninitialized_fill_n(reinterpret_cast<std::byte*>(o), size, std::byte{0xAA});
  });
}

// Move live objects from one generation to another. Returns a space containing
// all the dead objects and forwarding addresses to moved objects.
[[nodiscard]]
static dense_space
promote(generation& from, generation& to) {
  move_survivors(from.small, to.small, to.generation_number);

  large_space& large = from.large;
  for (std::unique_ptr<std::byte[]>& storage : large) {
    object* o = reinterpret_cast<object*>(storage.get() + sizeof(word_type));
    assert(object_color(o) != color::grey);

    if (object_color(o) == color::black) {
      to.large.emplace_back(std::move(storage));
      set_object_generation(o, to.generation_number);
      set_object_color(o, color::white);
    }

    storage.reset();
  }

  large.erase(std::remove_if(large.begin(), large.end(), [] (auto const& storage) { return !storage; }),
              large.end());

  return std::move(from.small);
}

// Move living mature objects to a new space. Returns the old space, with dead
// objects and forwrding addresses for moved objects.
[[nodiscard]]
static dense_space
purge_mature(generation& from) {
  dense_space temp{from.small.allocator()};
  move_survivors(from.small, temp, from.generation_number);

  large_space& large = from.large;
  for (std::unique_ptr<std::byte[]>& storage : large) {
    object* o = reinterpret_cast<object*>(storage.get() + sizeof(word_type));
    assert(object_color(o) != color::grey);

    if (object_color(o) == color::white)
      storage.reset();

    set_object_color(o, color::white);
  }

  large.erase(std::remove_if(large.begin(), large.end(), [] (auto const& storage) { return !storage; }),
              large.end());

  std::swap(from.small, temp);
  return temp;
}

static void
move_incoming_arcs(generation& from, generation& to) {
  for (object* o : from.incoming_arcs)
    if (is_alive(o)) {
      assert(object_generation(o) > to.generation_number);
      to.incoming_arcs.emplace(o);
    } else if (!is_alive(o) && forwarding_address(o) != nullptr) {
      assert(object_generation(forwarding_address(o)) > to.generation_number);
      to.incoming_arcs.emplace(forwarding_address(o));
    }

  from.incoming_arcs.clear();
}

static void
update_references(dense_space const& space) {
  space.for_all([] (object* o) { object_type(o).update_references(o); });
}

static void
update_references(large_space const& space) {
  for (auto const& storage : space) {
    object* o = reinterpret_cast<object*>(storage.get() + sizeof(word_type));
    object_type(o).update_references(o);
  }
}

static void
update_references(std::unordered_set<object*> const& set) {
  for (object* o : set) {
    if (is_alive(o))
      object_type(o).update_references(o);
    else if (object* fwd = forwarding_address(o); fwd != nullptr)
      object_type(fwd).update_references(fwd);
  }
}

static std::size_t
space_occupied_size(large_space const& s) {
  std::size_t result = 0;
  for (auto const& storage : s) {
    object* o = reinterpret_cast<object*>(storage.get() + sizeof(word_type));
    result += sizeof(word_type) + object_size(o);
  }

  return result;
}

static std::string
format_stats(generation const& nursery_1, generation const& nursery_2, generation const& mature) {
  return fmt::format("\n"
                     "  -- Nursery 1: {} pages, {} bytes, {} large objects, {} large bytes\n"
                     "  -- Nursery 2: {} pages, {} bytes, {} large objects, {} large bytes\n"
                     "  -- Mature: {} pages, {} bytes, {} large objects, {} large bytes",
                     nursery_1.small.pages_used(), nursery_1.small.bytes_used(),
                     nursery_1.large.size(), space_occupied_size(nursery_1.large),
                     nursery_2.small.pages_used(), nursery_2.small.bytes_used(),
                     nursery_2.large.size(), space_occupied_size(nursery_2.large),
                     mature.small.pages_used(), mature.small.bytes_used(),
                     mature.large.size(), space_occupied_size(mature.large));
}

static void
verify(generation const& g) {
  static_cast<void>(g);

#ifndef NDEBUG
  g.small.for_all([&] (object* o) {
    assert(!is_object_ptr(o) || object_generation(o) == g.generation_number);
  });

  for (auto const& storage : g.large)
    assert(object_generation(reinterpret_cast<object*>(storage.get() + sizeof(word_type))) == g.generation_number);
#endif
}

void
free_store::collect_garbage(bool major) {
  word_type max_generation = major ? generation::mature : generation::nursery_2;

  if (disable_level_ > 0) {
    requested_collection_level_ = max_generation;
    return;
  }

  generation& nursery_1 = generations_[generation::nursery_1];
  generation& nursery_2 = generations_[generation::nursery_2];
  generation& mature = generations_[generation::mature];

  if (verbose_collection)
    fmt::print("GC: Old: {}\n", format_stats(nursery_1, nursery_2, mature));

  trace(roots_, generations_, max_generation);

  dense_space old_mature;
  if (max_generation >= generation::mature)
    old_mature = purge_mature(mature);
  dense_space old_nursery_2 = promote(nursery_2, mature);
  dense_space old_nursery_1 = promote(nursery_1, nursery_2);

  std::unordered_set<object*> old_n2_incoming = std::move(nursery_2.incoming_arcs);
  nursery_2.incoming_arcs.clear();

  move_incoming_arcs(nursery_1, nursery_2);
  nursery_1.incoming_arcs.clear();

  assert(nursery_1.small.empty());
  assert(nursery_1.large.empty());

  update_references(nursery_2.small);
  update_references(mature.small);
  update_references(nursery_2.large);
  update_references(mature.large);
  update_references(old_n2_incoming);

  verify(nursery_1);
  verify(nursery_2);
  verify(mature);

  update_roots();

  requested_collection_level_ = std::nullopt;

  target_nursery_pages_ = std::max(min_nursery_pages, nursery_1.small.pages_used() + nursery_reserve_pages);
  allocator_.keep_at_most(2 * target_nursery_pages_ + mature_reserve_pages);

  if (verbose_collection) {
    fmt::print("GC: New: {}\n", format_stats(nursery_1, nursery_2, mature));
    fmt::print("  -- target nursery pages: {}\n"
               "  -- allocator reserve: {} pages\n"
               "  -- allocated pages: {}\n"
               "  -- deallocated pages: {}\n" ,
               target_nursery_pages_, allocator_.reserve_pages(),
               allocator_.allocated_pages(), allocator_.deallocated_pages());
  }

  allocator_.reset_stats();
}

void
free_store::update() {
  if (requested_collection_level_)
    collect_garbage(*requested_collection_level_ == generation::mature);
}

std::byte*
free_store::allocate_object(std::size_t size, word_type type) {
  std::size_t total_size = size + sizeof(word_type);

  std::byte* storage = nullptr;
  if (total_size >= large_threshold)
    storage = generations_[generation::nursery_1].large.emplace_back(std::make_unique<std::byte[]>(total_size)).get();
  else {
    dense_space& space = generations_[generation::nursery_1].small;
    storage = space.allocate(total_size);

    if (space.pages_used() >= target_nursery_pages_)
      request_collection();
  }

  init_object_header(storage, type);
  std::byte* object_storage = storage + sizeof(word_type);
  return object_storage;
}

void
free_store::update_roots() {
  for (generic_ptr* p = roots_; p; p = p->next()) {
    if (p->get() && is_object_ptr(p->get()) && !is_alive(p->get())) {
      p->value_ = forwarding_address(p->get());
      assert(p->value_ != nullptr);
    }

    assert(!p->get() || !is_object_ptr(p->get()) || is_alive(p->get()));
    assert(!p->get() || !is_object_ptr(p->get()) || object_color(p->get()) == color::white);
  }

  for (generic_weak_ptr* wp = weak_roots_; wp; wp = wp->next()) {
    if (wp->get() && is_object_ptr(wp->get()) && !is_alive(wp->get()))
      wp->value_ = forwarding_address(wp->get());
  }
}

void
free_store::request_collection() {
  collection_number_ = (collection_number_ + 1) % major_collection_frequency;
  if (collection_number_ == 0)
    requested_collection_level_ = generation::mature;
  else
    requested_collection_level_ = generation::nursery_2;
}

} // namespace insider
