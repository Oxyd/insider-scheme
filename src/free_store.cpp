#include "free_store.hpp"

#include <algorithm>
#include <cassert>

#include <fmt/format.h>

#undef small

namespace insider {

static constexpr std::size_t page_size = 4096;
static constexpr std::size_t large_threshold = 256;
static constexpr std::size_t min_nursery_pages = 1024;
static constexpr std::size_t min_nursery_size = 2 * min_nursery_pages * page_size;
static constexpr std::size_t nursery_reserve_pages = 10;
static constexpr std::size_t nursery_reserve_bytes = nursery_reserve_pages * page_size;
static constexpr std::size_t mature_reserve_pages = 10;
static constexpr std::size_t major_collection_frequency = 32;

enum class color : word_type {
  white = 0,
  grey = 1,
  black = 2,
};

static void
init_object_header(std::byte* storage, word_type type, generation gen = generation::nursery_1) {
  new (storage) word_type((type << type_shift)
                          | alive_bit
                          | (static_cast<word_type>(gen) << generation_shift));
}

std::size_t
object_size(ptr<> o) {
  type_descriptor const& t = object_type(o);
  return t.constant_size ? t.size : t.get_size(o);
}

static color
object_color(word_type header) {
  return static_cast<color>((header & color_bits) >> color_shift);
}

static color
object_color(ptr<> o) { return object_color(header_word(o)); }

static void
set_object_color(ptr<> o, color c) {
  header_word(o) = (header_word(o) & ~color_bits) | (static_cast<word_type>(c) << color_shift);
}

static bool
is_alive(word_type header) { return header & alive_bit; }

bool
is_alive(ptr<> o) { return o != nullptr && is_alive(header_word(o)); }

static generation
get_generation(word_type header) {
  return static_cast<generation>((header & generation_bits) >> generation_shift);
}

generation
object_generation(ptr<> o) { return get_generation(header_word(o)); }

static void
set_object_generation(ptr<> o, generation gen) {
  header_word(o) = (header_word(o) & ~generation_bits) | (static_cast<word_type>(gen) << generation_shift);
}

ptr<>
forwarding_address(ptr<> o) {
  assert(!is_alive(o));
  return reinterpret_cast<object*>(header_word(o));
}

static void
set_forwarding_address(ptr<> from, ptr<> target) {
  header_word(from) = reinterpret_cast<word_type>(target.value());
  assert((header_word(from) & alive_bit) == 0);
}

void
tracing_context::trace(ptr<> o) {
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

std::byte*
large_space::allocate(std::size_t size) {
  std::byte* result = allocations_.emplace_back(std::make_unique<std::byte[]>(size)).get();
  bytes_used_ += size;
  return result;
}

void
large_space::move(std::size_t i, large_space& to) {
  assert(allocations_[i]);

  ptr<> o = reinterpret_cast<object*>(allocations_[i].get() + sizeof(word_type));
  std::size_t size = object_size(o) + sizeof(word_type);

  to.allocations_.emplace_back(std::move(allocations_[i]));
  to.bytes_used_ += size;
  bytes_used_ -= size;

  allocations_[i].reset();
}

void
large_space::deallocate(std::size_t i) {
  assert(allocations_[i]);

  std::byte* storage = allocations_[i].get();
  ptr<> o = reinterpret_cast<object*>(storage + sizeof(word_type));
  assert(is_alive(o));

  bytes_used_ -= object_size(o) + sizeof(word_type);
  object_type(o).destroy(o);

  allocations_[i].reset();
}

void
large_space::remove_empty() {
  allocations_.erase(std::remove_if(allocations_.begin(), allocations_.end(),
                                    [] (auto const& storage) { return !storage; }),
                     allocations_.end());
}

static ptr<>
move_object(ptr<> o, dense_space& to) {
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

  permanent_roots_.clear();
  collect_garbage(true);

#ifndef NDEBUG
  assert(nursery_1_.small.empty());
  assert(nursery_1_.large.empty());
  assert(nursery_2_.small.empty());
  assert(nursery_2_.large.empty());
  assert(mature_.small.empty());
  assert(mature_.large.empty());
#endif
}

static void
trace(generic_tracked_ptr* roots, std::vector<ptr<>> const& permanent_roots,
      nursery_generation const& nursery_1, nursery_generation const& nursery_2,
      generation max_generation) {
  assert(max_generation >= generation::nursery_2);

  std::vector<ptr<>> stack;
  tracing_context tc{stack, max_generation};

  for (generic_tracked_ptr* root = roots; root; root = root->next())
    if (root->get() && is_object_ptr(root->get())
        && object_generation(root->get()) <= max_generation
        && object_color(root->get()) == color::white) {
      assert(is_alive(root->get()));
      set_object_color(root->get(), color::grey);
      stack.push_back(root->get());
    }

  if (max_generation < generation::mature)
    for (ptr<> o : permanent_roots)
      if (object_color(o) == color::white) {
        set_object_color(o, color::grey);
        object_type(o).trace(o, tc);
      }

  for (nursery_generation const* g : {&nursery_1, &nursery_2})
    for (ptr<> o : g->incoming_arcs)
      if (object_generation(o) > max_generation && object_color(o) == color::white) {
        assert(is_object_ptr(o));
        assert(object_generation(o) > g->generation_number);

        object_type(o).trace(o, tc);
      }

  while (!stack.empty()) {
    ptr<> top = stack.back();
    stack.pop_back();

    assert(object_color(top) != color::white);
    if (object_color(top) == color::grey) {
      object_type(top).trace(top, tc);
      set_object_color(top, color::black);
    }
  }
}

static void
move_survivors(dense_space& from, dense_space& to, generation to_gen) {
  from.for_all([&] (ptr<> o) {
    type_descriptor const& t = object_type(o);
    std::size_t size = object_size(o);

    assert(object_color(o) != color::grey);
    if (object_color(o) == color::black) {
      ptr<> target = move_object(o, to);
      set_forwarding_address(o, target);
      set_object_generation(target, to_gen);
    } else
      set_forwarding_address(o, nullptr);

    t.destroy(o);
    std::uninitialized_fill_n(reinterpret_cast<std::byte*>(o.value()), size, std::byte{0xAA});
  });
}

// Move live objects from one generation to another. Returns a space containing
// all the dead objects and forwarding addresses to moved objects.
template <typename FromG, typename ToG>
[[nodiscard]]
static dense_space
promote(FromG& from, ToG& to) {
  move_survivors(from.small, to.small, to.generation_number);

  large_space& large = from.large;
  for (std::size_t i = 0; i < large.object_count(); ++i) {
    ptr<> o = reinterpret_cast<object*>(large.get(i) + sizeof(word_type));
    assert(object_color(o) != color::grey);

    if (object_color(o) == color::black) {
      large.move(i, to.large);
      set_object_generation(o, to.generation_number);
      set_object_color(o, color::white);
    } else
      large.deallocate(i);
  }

  large.remove_empty();

  return std::move(from.small);
}

// Move living mature objects to a new space. Returns the old space, with dead
// objects and forwrding addresses for moved objects.
template <typename Generation>
[[nodiscard]]
static dense_space
purge_mature(Generation& from) {
  dense_space temp{from.small.allocator()};
  move_survivors(from.small, temp, from.generation_number);

  large_space& large = from.large;
  for (std::size_t i = 0; i < large.object_count(); ++i) {
    ptr<> o = reinterpret_cast<object*>(large.get(i) + sizeof(word_type));
    assert(object_color(o) != color::grey);

    if (object_color(o) == color::white)
      large.deallocate(i);
    else
      set_object_color(o, color::white);
  }

  large.remove_empty();

  std::swap(from.small, temp);
  return temp;
}

static void
move_incoming_arcs(nursery_generation& from, nursery_generation& to) {
  for (ptr<> o : from.incoming_arcs)
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
  space.for_all([] (ptr<> o) { object_type(o).update_references(o); });
}

static void
update_references(large_space const& space) {
  for (std::size_t i = 0; i < space.object_count(); ++i) {
    ptr<> o = reinterpret_cast<object*>(space.get(i) + sizeof(word_type));
    object_type(o).update_references(o);
  }
}

static void
update_references(std::unordered_set<ptr<>> const& set) {
  for (ptr<> o : set) {
    if (is_alive(o))
      object_type(o).update_references(o);
    else if (ptr<> fwd = forwarding_address(o); fwd != nullptr)
      object_type(fwd).update_references(fwd);
  }
}

static std::string
format_stats(nursery_generation const& nursery_1, nursery_generation const& nursery_2,
             mature_generation const& mature) {
  return fmt::format("\n"
                     "  -- Nursery 1: {} pages, {} bytes, {} large objects, {} large bytes\n"
                     "  -- Nursery 2: {} pages, {} bytes, {} large objects, {} large bytes\n"
                     "  -- Mature: {} pages, {} bytes, {} large objects, {} large bytes",
                     nursery_1.small.pages_used(), nursery_1.small.bytes_used(),
                     nursery_1.large.object_count(), nursery_1.large.bytes_used(),
                     nursery_2.small.pages_used(), nursery_2.small.bytes_used(),
                     nursery_2.large.object_count(), nursery_2.large.bytes_used(),
                     mature.small.pages_used(), mature.small.bytes_used(),
                     mature.large.object_count(), mature.large.bytes_used());
}

template <typename Generation>
static void
verify([[maybe_unused]] Generation const& g) {
#ifndef NDEBUG
  g.small.for_all([&] (ptr<> o) {
    assert(!is_object_ptr(o) || object_generation(o) == g.generation_number);
  });

  for (std::size_t i = 0; i < g.large.object_count(); ++i)
    assert(object_generation(reinterpret_cast<object*>(g.large.get(i) + sizeof(word_type))) == g.generation_number);
#endif
}

void
free_store::collect_garbage(bool major) {
  generation max_generation = major ? generation::mature : generation::nursery_2;

  if (disable_level_ > 0) {
    requested_collection_level_ = max_generation;
    return;
  }

  if (verbose_collection)
    fmt::print("GC: Old: {}\n", format_stats(nursery_1_, nursery_2_, mature_));

  trace(roots_, permanent_roots_, nursery_1_, nursery_2_, max_generation);

  dense_space old_mature;
  if (max_generation >= generation::mature)
    old_mature = purge_mature(mature_);
  dense_space old_nursery_2 = promote(nursery_2_, mature_);
  dense_space old_nursery_1 = promote(nursery_1_, nursery_2_);

  std::unordered_set<ptr<>> old_n2_incoming = std::move(nursery_2_.incoming_arcs);
  nursery_2_.incoming_arcs.clear();

  move_incoming_arcs(nursery_1_, nursery_2_);
  nursery_1_.incoming_arcs.clear();

  assert(nursery_1_.small.empty());
  assert(nursery_1_.large.empty());

  update_references(nursery_2_.small);
  update_references(mature_.small);
  update_references(nursery_2_.large);
  update_references(mature_.large);
  update_references(old_n2_incoming);

  verify(nursery_1_);
  verify(nursery_2_);
  verify(mature_);

  update_roots();
  update_permanent_roots();
  reset_colors(max_generation);

  requested_collection_level_ = std::nullopt;

  target_nursery_pages_ = std::max(min_nursery_pages, nursery_2_.small.pages_used() + nursery_reserve_pages);
  target_nursery_bytes_ = std::max(min_nursery_size,
                                   nursery_2_.small.bytes_used()
                                   + nursery_2_.large.bytes_used()
                                   + nursery_reserve_bytes);

  allocator_.keep_at_most(2 * target_nursery_pages_ + mature_reserve_pages);

  if (verbose_collection) {
    fmt::print("GC: New: {}\n", format_stats(nursery_1_, nursery_2_, mature_));
    fmt::print("  -- target nursery pages: {}\n"
               "  -- target nursery bytes: {}\n"
               "  -- allocator reserve: {} pages\n"
               "  -- allocated pages: {}\n"
               "  -- deallocated pages: {}\n" ,
               target_nursery_pages_, target_nursery_bytes_,
               allocator_.reserve_pages(),
               allocator_.allocated_pages(), allocator_.deallocated_pages());
  }

  allocator_.reset_stats();
}

std::byte*
free_store::allocate_object(std::size_t size, word_type type) {
  std::size_t total_size = size + sizeof(word_type);

  std::byte* storage = nullptr;
  if (total_size >= large_threshold)
    storage = nursery_1_.large.allocate(total_size);
  else
    storage = nursery_1_.small.allocate(total_size);

  check_nursery_size();

  init_object_header(storage, type, generation::nursery_1);
  std::byte* object_storage = storage + sizeof(word_type);
  return object_storage;
}

void
free_store::update_roots() {
  for (generic_tracked_ptr* p = roots_; p; p = p->next()) {
    if (p->get() && is_object_ptr(p->get()) && !is_alive(p->get())) {
      p->value_ = forwarding_address(p->get()).value();
      assert(p->value_ != nullptr);
    }

    assert(!p->get() || !is_object_ptr(p->get()) || is_alive(p->get()));
    assert(!p->get() || !is_object_ptr(p->get()) || object_type(p->get()).permanent_root
           || object_color(p->get()) == color::white);
  }

  for (generic_weak_ptr* wp = weak_roots_; wp; wp = wp->next()) {
    if (wp->get() && is_object_ptr(wp->get()) && !is_alive(wp->get()))
      wp->value_ = forwarding_address(wp->get()).value();
  }
}

void
free_store::update_permanent_roots() {
  for (auto r = permanent_roots_.begin(); r != permanent_roots_.end();) {
    if (!is_alive(*r))
      *r = forwarding_address(*r);

    if (!*r)
      r = permanent_roots_.erase(r);
    else
      ++r;
  }
}

void
free_store::reset_colors(generation max_generation) {
  if (max_generation < generation::mature) {
    for (nursery_generation* g : {&nursery_1_, &nursery_2_})
      for (ptr<> o : g->incoming_arcs)
        set_object_color(o, color::white);

    for (ptr<> o : permanent_roots_)
      set_object_color(o, color::white);
  }

#ifndef NDEBUG
  if (max_generation == generation::mature) {
    for (nursery_generation* g : {&nursery_1_, &nursery_2_})
      for (ptr<> o : g->incoming_arcs)
        assert(object_color(o) == color::white);

    for (ptr<> o : permanent_roots_)
      assert(object_color(o) == color::white);
  }
#endif
}

void
free_store::check_nursery_size() {
  if (nursery_1_.small.pages_used() > target_nursery_pages_
      || nursery_1_.small.bytes_used() + nursery_1_.large.bytes_used() > target_nursery_bytes_)
    request_collection();
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
