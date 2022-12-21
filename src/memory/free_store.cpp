#include "free_store.hpp"
#include "memory/member_visitor.hpp"
#include "memory/root_provider.hpp"

#include <algorithm>
#include <cassert>

#include <fmt/format.h>

#undef small

namespace insider {

static constexpr std::size_t large_threshold = 256;
static constexpr std::size_t min_nursery_pages = 4096;
static constexpr std::size_t min_nursery_size
  = 2 * min_nursery_pages * page_size;
static constexpr std::size_t nursery_reserve_pages = 10;
static constexpr std::size_t nursery_reserve_bytes
  = nursery_reserve_pages * page_size;
static constexpr std::size_t mature_reserve_pages = 10;
static constexpr std::size_t major_collection_frequency = 32;

enum class color : word_type {
  white = 0,
  grey = 1,
  black = 2,
};

static color
object_color(word_type header) {
  return static_cast<color>((header & color_bits) >> color_shift);
}

static color
object_color(ptr<> o) { return object_color(header_word(o)); }

static void
set_object_color(ptr<> o, color c) {
  header_word(o) = (header_word(o) & ~color_bits)
                   | (static_cast<word_type>(c) << color_shift);
}

static bool
is_alive(word_type header) { return (header & alive_bit) != 0u; }

bool
is_alive(ptr<> o) { return o != nullptr && is_alive(header_word(o)); }

static void
set_object_generation(ptr<> o, generation gen) {
  header_word(o) = (header_word(o) & ~generation_bits)
                   | (static_cast<word_type>(gen) << generation_shift);
}

static ptr<>
forwarding_address(ptr<> o) {
  assert(!is_alive(o));
  return reinterpret_cast<object*>(header_word(o));
}

static void
set_forwarding_address(ptr<> from, ptr<> target) {
  header_word(from) = reinterpret_cast<word_type>(target.value());
  assert((header_word(from) & alive_bit) == 0);
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

std::byte*
dense_space::allocate(std::size_t size) {
  assert(size < page_size);

  if (pages_.empty() || page_size - pages_.back().used < size)
    pages_.emplace_back(page{allocator_->allocate()});

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
      allocator_->deallocate(std::move(p.storage));

  total_used_ = 0;
}

bool
dense_space::has_preallocated_storage(std::size_t size) const {
  return !pages_.empty() && page_size - pages_.back().used >= size;
}

dense_space::page&
dense_space::take(page_allocator::page p, std::size_t used) {
  pages_.emplace_back(page{std::move(p)});

  page& new_page = pages_.back();
  new_page.used = used;
  new_page.for_all([] (ptr<> o) {
    set_object_generation(o, generation::nursery_1);
  });

  total_used_ += used;
  return new_page;
}

std::byte*
large_space::allocate(std::size_t size) {
  std::byte* result = allocations_.emplace_back(
    std::make_unique<std::byte[]>(size)
  ).get();
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

static void
destroy(ptr<> o, type_descriptor const& type, std::size_t size) {
  type.destroy(o);
  std::uninitialized_fill_n(reinterpret_cast<std::byte*>(o.value()), size,
                            std::byte{0xAA});
}

void
large_space::stage_for_deallocation(std::size_t i) {
  assert(allocations_[i]);

  std::byte* storage = allocations_[i].get();
  ptr<> o = reinterpret_cast<object*>(storage + sizeof(word_type));
  assert(is_alive(o));

  type_descriptor const& type = object_type(o);
  std::size_t size = object_size(o);

  bytes_used_ -= size + sizeof(word_type);
  destroy(o, type, size);
  set_forwarding_address(o, nullptr);

  to_deallocate_.push_back(i);
}

void
large_space::deallocate_staged() {
  for (std::size_t index : to_deallocate_)
    allocations_[index].reset();
  to_deallocate_.clear();
  compact();
}

void
large_space::compact() {
  allocations_.erase(
    std::remove_if(allocations_.begin(), allocations_.end(),
                   [] (auto const& storage) { return !storage; }),
    allocations_.end()
  );
}

static ptr<>
move_object(ptr<> o, dense_space& to) {
  type_descriptor const& t = object_type(o);
  std::size_t size = sizeof(word_type) + object_size(o);
  std::byte* storage = to.allocate(size);

  init_object_header(storage, object_type_index(o), object_hash(o));
  return t.move(o, storage + sizeof(word_type));
}

free_store::free_store()
  : target_nursery_pages_{min_nursery_pages}
{ }

static void
destroy_all_objects(auto& space) {
  space.for_all([] (ptr<> o) { object_type(o).destroy(o); });
}

free_store::~free_store() {
  destroy_all_objects(generations_.nursery_1.small);
  destroy_all_objects(generations_.nursery_1.large);
  destroy_all_objects(generations_.nursery_2.small);
  destroy_all_objects(generations_.nursery_2.large);
  destroy_all_objects(generations_.mature.small);
  destroy_all_objects(generations_.mature.large);
}

void
free_store::make_permanent_arc(ptr<> from) {
  permanent_roots_.push_back(from);
}

void
free_store::transfer_to_nursery(page_allocator::page p, std::size_t used) {
  generations_.nursery_1.small.take(std::move(p), used);
  check_nursery_size();
}

namespace {
  template <typename F>
  struct visitor : member_visitor {
    F& f;

    visitor(F& f) : f{f} { }

    void
    operator () (ptr_wrapper ptr) const override {
      f(ptr.value, ptr.weak);
    }
  };
}

template <typename F>
static void
visit_members(ptr<> object, F&& f) {
  object_type(object).visit_members(object, visitor<F>{f});
}

template <typename F>
static void
visit_roots(root_list const& roots, F&& f) {
  roots.visit_roots(visitor<F>{f});
}

static void
trace(std::vector<ptr<>> const& permanent_roots,
      root_list const& root_list,
      nursery_generation const& nursery_1, nursery_generation const& nursery_2,
      generation max_generation) {
  assert(max_generation >= generation::nursery_2);

  std::vector<ptr<>> stack;
  auto trace = [&] (ptr<> object) {
    visit_members(
      object,
      [&] (ptr<>& member, bool weak) {
        if (!weak
            && member && is_object_ptr(member)
            && object_color(member) == color::white
            && object_generation(member) <= max_generation) {
          assert(is_alive(member));
          assert(object_type_index(member) < types().size);

          stack.push_back(member);
          set_object_color(member, color::grey);
        }
      }
    );
  };

  if (max_generation < generation::mature)
    for (ptr<> o : permanent_roots)
      if (object_color(o) == color::white
          && object_generation(o) == generation::mature) {
        set_object_color(o, color::grey);
        trace(o);
      }

  visit_roots(root_list, [&] (ptr<>& root, bool weak) {
    if (!weak
        && root
        && is_object_ptr(root)
        && object_generation(root) <= max_generation
        && object_color(root) == color::white) {
      assert(is_alive(root));
      set_object_color(root, color::grey);
      stack.push_back(root);
    }
  });

  for (nursery_generation const* g : {&nursery_1, &nursery_2})
    for (ptr<> o : g->incoming_arcs)
      if (object_generation(o) > max_generation
          && object_color(o) == color::white) {
        assert(is_object_ptr(o));
        assert(object_generation(o) > g->generation_number);

        trace(o);
      }

  while (!stack.empty()) {
    ptr<> top = stack.back();
    stack.pop_back();

    assert(object_color(top) != color::white);
    if (object_color(top) == color::grey) {
      trace(top);
      set_object_color(top, color::black);
    }
  }
}

static void
find_new_arcs_to_nursery(std::vector<ptr<>> const& objects,
                         nursery_generation& nursery) {
  std::vector<ptr<>> arcs;

  for (ptr<> o : objects) {
    assert(is_alive(o));
    bool pushed = false;
    visit_members(o, [&] (ptr<>& member, bool) {
      if (!pushed && member && is_object_ptr(member)) {
        assert(is_alive(member));

        // There can be no pointers to nursery 1 because this function is called
        // after nursery 1 survivors have been promoted.
        assert(object_generation(member) != generation::nursery_1);

        if (object_generation(member) == generation::nursery_2) {
          arcs.push_back(o);
          pushed = true;
        }
      }
    });
  }

  for (ptr<> arc : arcs)
    nursery.incoming_arcs.emplace(arc);
}

static void
move_survivors(dense_space& from, dense_space& to, generation to_gen,
               std::vector<ptr<>>* moved_objects) {
  from.for_all([&] (ptr<> o) {
    type_descriptor const& type = object_type(o);
    std::size_t size = object_size(o);

    assert(object_color(o) != color::grey);
    if (object_color(o) == color::black) {
      ptr<> target = move_object(o, to);
      set_forwarding_address(o, target);
      set_object_generation(target, to_gen);

      if (moved_objects)
        moved_objects->push_back(target);
    } else
      set_forwarding_address(o, nullptr);

    destroy(o, type, size);
  });
}

static void
promote_large(large_space& large, std::vector<ptr<>>* moved_objects,
              auto&& move = [] (std::size_t, ptr<>) { }) {
  for (std::size_t i = 0; i < large.object_count(); ++i) {
    ptr<> o = reinterpret_cast<object*>(large.get(i) + sizeof(word_type));
    assert(object_color(o) != color::grey);

    if (object_color(o) == color::black) {
      move(i, o);
      set_object_color(o, color::white);

      if (moved_objects)
        moved_objects->push_back(o);
    } else
      large.stage_for_deallocation(i);
  }
}

static void
promote_large(large_space& large, std::vector<ptr<>>* moved_objects) {
  promote_large(large, moved_objects, [] (std::size_t, ptr<>) { });
}

// Move live objects from one generation to another. Returns a space containing
// all the dead objects and forwarding addresses to moved objects.
[[nodiscard]]
static dense_space
promote(auto& from, auto& to, std::vector<ptr<>>* moved_objects) {
  move_survivors(from.small, to.small, to.generation_number, moved_objects);
  promote_large(from.large, moved_objects, [&] (std::size_t i, ptr<> o) {
    from.large.move(i, to.large);
    set_object_generation(o, to.generation_number);
  });

  return std::move(from.small);
}

// Move living mature objects to a new space. Returns the old space, with dead
// objects and forwrding addresses for moved objects.
[[nodiscard]]
static dense_space
purge_mature(mature_generation& mature, std::vector<ptr<>>* moved_objects) {
  dense_space temp{mature.small.allocator()};
  move_survivors(mature.small, temp, generation::mature, moved_objects);
  promote_large(mature.large, moved_objects);

  std::swap(mature.small, temp);
  return temp;
}

static void
move_incoming_arcs(std::unordered_set<ptr<>> const& arcs,
                   nursery_generation& to) {
  for (ptr<> o : arcs)
    if (is_alive(o)) {
      assert(object_generation(o) > to.generation_number);
      to.incoming_arcs.emplace(o);
    } else if (!is_alive(o) && forwarding_address(o) != nullptr) {
      assert(object_generation(forwarding_address(o)) > to.generation_number);
      to.incoming_arcs.emplace(forwarding_address(o));
    }
}

static void
update_member(ptr<>& member, bool weak) {
  if (member && is_object_ptr(member) && !is_alive(member)) {
    assert(is_alive(forwarding_address(member)) || weak);
    member.reset(forwarding_address(member));
  }
}

static void
update_members(ptr<> o) {
  visit_members(o, update_member);
}

static void
update_references(dense_space const& space) {
  space.for_all(update_members);
}

static void
update_references(large_space const& space) {
  for (std::size_t i = 0; i < space.object_count(); ++i)
    if (std::byte* storage = space.get(i)) {
      ptr<> o = reinterpret_cast<object*>(storage + sizeof(word_type));
      if (is_alive(o))
        update_members(o);
    }
}

static void
update_references(root_list const& list) {
  visit_roots(list, update_member);
}

static void
update_references(auto const& incoming) {
  for (ptr<> x : incoming)
    update_members(x);
}

static std::string
format_stats(nursery_generation const& nursery_1,
             nursery_generation const& nursery_2,
             mature_generation const& mature) {
  return fmt::format(
    "\n"
    "  -- Nursery 1: {} pages, {} bytes, {} large objects, {} large bytes\n"
    "  -- Nursery 2: {} pages, {} bytes, {} large objects, {} large bytes\n"
    "  -- Mature: {} pages, {} bytes, {} large objects, {} large bytes",
    nursery_1.small.pages_used(), nursery_1.small.bytes_used(),
    nursery_1.large.object_count(), nursery_1.large.bytes_used(),
    nursery_2.small.pages_used(), nursery_2.small.bytes_used(),
    nursery_2.large.object_count(), nursery_2.large.bytes_used(),
    mature.small.pages_used(), mature.small.bytes_used(),
    mature.large.object_count(), mature.large.bytes_used()
  );
}

static void
verify([[maybe_unused]] auto const& g) {
#ifndef NDEBUG
  g.small.for_all([&] (ptr<> o) {
    assert(!is_object_ptr(o) || object_generation(o) == g.generation_number);
  });

  for (std::size_t i = 0; i < g.large.object_count(); ++i)
    assert(
      object_generation(
        reinterpret_cast<object*>(g.large.get(i) + sizeof(word_type))
      ) == g.generation_number
    );
#endif
}

void
free_store::collect_garbage(bool major) {
  generation max_generation = major ? generation::mature : generation::nursery_2;

  if (disable_level_ > 0) {
    requested_collection_level_ = max_generation;
    return;
  }

  if (verbose_collection) {
    fmt::print("GC: {} collection\n", major ? "major" : "minor");
    fmt::print("GC: Old: {}\n",
               format_stats(generations_.nursery_1, generations_.nursery_2,
                            generations_.mature));
  }

  trace(permanent_roots_, roots_, generations_.nursery_1, generations_.nursery_2,
        max_generation);

  std::unordered_set<ptr<>> old_n1_incoming
    = std::move(generations_.nursery_1.incoming_arcs);
  generations_.nursery_1.incoming_arcs.clear();

  std::unordered_set<ptr<>> old_n2_incoming
    = std::move(generations_.nursery_2.incoming_arcs);
  generations_.nursery_2.incoming_arcs.clear();

  std::vector<ptr<>> new_mature_objects;
  dense_space old_mature;
  if (max_generation >= generation::mature)
    old_mature = purge_mature(generations_.mature, &new_mature_objects);

  dense_space old_nursery_2
    = promote(generations_.nursery_2, generations_.mature, &new_mature_objects);
  dense_space old_nursery_1
    = promote(generations_.nursery_1, generations_.nursery_2, nullptr);

  assert(generations_.nursery_1.small.empty());

  // If we purged the mature generation, nursery-1 now has incoming arcs from
  // mature survivors and from nursery-2 survivors who have been promoted to
  // mature. In that case, there is nothing to do, just move those arcs to
  // nursery-2.
  //
  // If this is a minor collection, we nursery-1's incoming arcs only contain
  // promoted nursery-2 survivors. So we need to go through old incoming arcs
  // into nursery-1, and if the source is still alive, add it to the current
  // incoming arcs.

  assert(generations_.nursery_2.incoming_arcs.empty());

  generations_.nursery_2.incoming_arcs
    = std::move(generations_.nursery_1.incoming_arcs);

  if (max_generation < generation::mature)
    move_incoming_arcs(old_n1_incoming, generations_.nursery_2);

  update_references(generations_.nursery_2.small);
  update_references(generations_.nursery_2.large);

  if (max_generation == generation::mature) {
    update_references(generations_.mature.small);
    update_references(generations_.mature.large);
  } else {
    update_references(generations_.nursery_2.incoming_arcs);
    update_references(old_n2_incoming);
    update_references(new_mature_objects);
  }

  find_new_arcs_to_nursery(new_mature_objects, generations_.nursery_2);

  generations_.nursery_1.large.deallocate_staged();
  generations_.nursery_2.large.deallocate_staged();
  generations_.mature.large.deallocate_staged();

  verify(generations_.nursery_1);
  verify(generations_.nursery_2);
  verify(generations_.mature);

  update_permanent_roots();
  update_references(permanent_roots_);
  update_references(roots_);
  reset_colors(max_generation);

  requested_collection_level_ = std::nullopt;

  target_nursery_pages_
    = std::max(min_nursery_pages,
               generations_.nursery_2.small.pages_used() + nursery_reserve_pages);
  target_nursery_bytes_ = std::max(min_nursery_size,
                                   generations_.nursery_2.small.bytes_used()
                                   + generations_.nursery_2.large.bytes_used()
                                   + nursery_reserve_bytes);

  allocator_.keep_at_most(2 * target_nursery_pages_ + mature_reserve_pages);

  roots_.compact();

  if (verbose_collection) {
    fmt::print("GC: New: {}\n",
               format_stats(generations_.nursery_1, generations_.nursery_2,
                            generations_.mature));
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
    storage = generations_.nursery_1.large.allocate(total_size);
  else
    storage = generations_.nursery_1.small.allocate(total_size);

  check_nursery_size();

  init_object_header(storage, type, next_hash_(), generation::nursery_1);
  std::byte* object_storage = storage + sizeof(word_type);
  return object_storage;
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
    for (nursery_generation* g : {&generations_.nursery_1,
                                  &generations_.nursery_2})
      for (ptr<> o : g->incoming_arcs)
        set_object_color(o, color::white);

    for (ptr<> o : permanent_roots_)
      set_object_color(o, color::white);
  }

#ifndef NDEBUG
  if (max_generation == generation::mature) {
    for (nursery_generation* g : {&generations_.nursery_1,
                                  &generations_.nursery_2})
      for (ptr<> o : g->incoming_arcs)
        assert(object_color(o) == color::white);

    for (ptr<> o : permanent_roots_)
      assert(object_color(o) == color::white);
  }
#endif
}

void
free_store::check_nursery_size() {
  if (generations_.nursery_1.small.pages_used() > target_nursery_pages_
      || generations_.nursery_1.small.bytes_used()
         + generations_.nursery_1.large.bytes_used()
         > target_nursery_bytes_)
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
