#include "free_store.hpp"
#include "memory/member_visitor.hpp"
#include "object.hpp"

#include <fmt/format.h>

#include <algorithm>
#include <cassert>

#undef small

namespace insider {

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
object_color(ptr<> o) { return object_color(o.header()->flags); }

static color
object_color(object_header* h) {
  return object_color(h->flags);
}

static void
set_object_color(word_type& header, color c) {
  header = (header & ~color_bits) | (static_cast<word_type>(c) << color_shift);
}

static void
set_object_color(object_header* h, color c) {
  set_object_color(h->flags, c);
}

static void
set_object_color(ptr<> o, color c) {
  o.header()->flags = (o.header()->flags & ~color_bits)
                      | (static_cast<word_type>(c) << color_shift);
}

static bool
is_alive(word_type header) { return (header & alive_bit) != 0u; }

static bool
is_alive(object_header* h) {
  return h != nullptr && is_alive(h->flags);
}

bool
is_alive(ptr<> o) { return o != nullptr && is_alive(o.header()->flags); }

static void
set_object_generation(object_header* h, generation gen) {
  h->flags = (h->flags & ~generation_bits)
             | (static_cast<word_type>(gen) << generation_shift);
}

static object_header*
forwarding_address(word_type header) {
  return reinterpret_cast<object_header*>(header);
}

static object_header*
forwarding_address(object_header* h) {
  return forwarding_address(h->flags);
}

static ptr<>
forwarding_address(ptr<> o) {
  assert(!is_alive(o));
  return ptr<>{reinterpret_cast<object_header*>(o.header()->flags)};
}

static void
set_forwarding_address(object_header* from, ptr<> target) {
  from->flags = target.as_word();
  assert((from->flags & alive_bit) == 0);
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
  new_page.for_all([] (object_header* h) {
    set_object_generation(h, generation::nursery_1);
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

  auto* o = reinterpret_cast<object_header*>(allocations_[i].get());
  std::size_t size = storage_size(o);

  to.allocations_.emplace_back(std::move(allocations_[i]));
  to.bytes_used_ += size;
  bytes_used_ -= size;

  allocations_[i].reset();
}

void
large_space::stage_for_deallocation(std::size_t i) {
  assert(allocations_[i]);

  auto* o = reinterpret_cast<object_header*>(allocations_[i].get());
  assert(is_alive(o));

  type_descriptor const& type = object_type(o);
  std::size_t size = storage_size(o);

  bytes_used_ -= size;
  type.destroy(o);
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
move_object(object_header* o, dense_space& to) {
  type_descriptor const& t = object_type(o);
  std::size_t size = storage_size(o);
  auto* header = reinterpret_cast<object_header*>(to.allocate(size));

  *header = make_object_header(type_index(o), object_hash(o));
  return t.move(o, header);
}

free_store::free_store()
  : target_nursery_pages_{min_nursery_pages}
{ }

static void
destroy_all_objects(auto& space) {
  space.for_all([] (object_header* o) {
    object_type(o).destroy(o);
  });
}

free_store::~free_store() {
  destroy_all_objects(generations_.nursery_1.small);
  destroy_all_objects(generations_.nursery_1.large);
  destroy_all_objects(generations_.nursery_2.small);
  destroy_all_objects(generations_.nursery_2.large);
  destroy_all_objects(generations_.mature.small);
  destroy_all_objects(generations_.mature.large);
}

namespace {
  template <typename F>
  struct visitor : member_visitor {
    F& f;

    visitor(F& f) : f{f} { }

    void
    operator () (ptr<> const& ptr) const override {
      f(ptr, false);
    }

    void
    weak(ptr<> const& ptr) const override {
      f(ptr, true);
    }
  };
}

template <typename F>
static void
visit_members(object_header* o, F&& f) {
  object_type(o).visit_members(o, visitor<F>{f});
}

template <typename F>
static void
visit_roots(root_list const& roots, F&& f) {
  roots.visit_roots(visitor<F>{f});
}

static void
trace(root_list const& root_list,
      nursery_generation const& nursery_1, nursery_generation const& nursery_2,
      generation max_generation) {
  assert(max_generation >= generation::nursery_2);

  std::vector<ptr<>> stack;
  auto trace = [&] (object_header* object) {
    visit_members(
      object,
      [&] (ptr<> const& member, bool weak) {
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

  visit_roots(root_list, [&] (ptr<> const& root, bool weak) {
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
    for (object_header* o : g->incoming_arcs)
      if (object_generation(o) > max_generation
          && object_color(o) == color::white) {
        assert(object_generation(o) > g->generation_number);
        trace(o);
      }

  while (!stack.empty()) {
    ptr<> top = stack.back();
    stack.pop_back();

    assert(object_color(top) != color::white);
    if (object_color(top) == color::grey) {
      trace(top.header());
      set_object_color(top, color::black);
    }
  }
}

static void
find_new_arcs_to_nursery(std::vector<object_header*> const& objects,
                         nursery_generation& nursery) {
  std::vector<object_header*> arcs;

  for (object_header* o : objects) {
    assert(is_alive(o));
    bool pushed = false;
    visit_members(o, [&] (ptr<> const& member, bool) {
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

  for (object_header* arc : arcs)
    nursery.incoming_arcs.emplace(arc);
}

static void
move_survivors(dense_space& from, dense_space& to, generation to_gen,
               std::vector<object_header*>* moved_objects) {
  from.for_all([&] (object_header* o) {
    type_descriptor const& type = object_type(o);

    assert(object_color(o) != color::grey);
    if (object_color(o) == color::black) {
      ptr<> target = move_object(o, to);
      set_forwarding_address(o, target);
      set_object_generation(target.header(), to_gen);

      if (moved_objects)
        moved_objects->push_back(target.header());
    } else
      set_forwarding_address(o, nullptr);

    type.destroy(o);
  });
}

static void
promote_large(large_space& large,
              std::vector<object_header*>* moved_objects,
              auto&& move = [] (std::size_t, object_header*) { }) {
  for (std::size_t i = 0; i < large.object_count(); ++i) {
    auto* o = reinterpret_cast<object_header*>(large.get(i));
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
promote_large(large_space& large,
              std::vector<object_header*>* moved_objects) {
  promote_large(large, moved_objects,
                [] (std::size_t, object_header*) { });
}

// Move live objects from one generation to another. Returns a space containing
// all the dead objects and forwarding addresses to moved objects.
[[nodiscard]]
static dense_space
promote(auto& from, auto& to,
        std::vector<object_header*>* moved_objects) {
  move_survivors(from.small, to.small, to.generation_number, moved_objects);
  promote_large(from.large, moved_objects,
                [&] (std::size_t i, object_header* o) {
                  from.large.move(i, to.large);
                  set_object_generation(o, to.generation_number);
                });

  return std::move(from.small);
}

// Move living mature objects to a new space. Returns the old space, with dead
// objects and forwrding addresses for moved objects.
[[nodiscard]]
static dense_space
purge_mature(mature_generation& mature,
             std::vector<object_header*>* moved_objects) {
  dense_space temp{mature.small.allocator()};
  move_survivors(mature.small, temp, generation::mature, moved_objects);
  promote_large(mature.large, moved_objects);

  std::swap(mature.small, temp);
  return temp;
}

static void
move_incoming_arcs(std::unordered_set<object_header*> const& arcs,
                   nursery_generation& to) {
  for (object_header* o : arcs)
    if (is_alive(o)) {
      assert(object_generation(o) > to.generation_number);
      to.incoming_arcs.emplace(o);
    } else if (!is_alive(o) && forwarding_address(o) != nullptr) {
      assert(object_generation(forwarding_address(o))
             > to.generation_number);
      to.incoming_arcs.emplace(forwarding_address(o));
    }
}

void
detail::update_ptr(ptr<> const& p, ptr<> new_value) {
  p.value_ = new_value.value_;
}

static void
update_member(ptr<> const& member, [[maybe_unused]] bool weak) {
  if (member && is_object_ptr(member) && !is_alive(member)) {
    assert(is_alive(forwarding_address(member)) || weak);
    detail::update_ptr(member, forwarding_address(member));
  }
}

static void
update_members(object_header* o) {
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
      auto* o = reinterpret_cast<object_header*>(storage);
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
  for (object_header* x : incoming)
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
  g.small.for_all([&] (object_header* o) {
    assert(object_generation(o) == g.generation_number);
  });

  for (std::size_t i = 0; i < g.large.object_count(); ++i)
    assert(
      object_generation(
        reinterpret_cast<object_header*>(g.large.get(i))
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

  trace(roots_, generations_.nursery_1, generations_.nursery_2, max_generation);

  std::unordered_set<object_header*> old_n1_incoming
    = std::move(generations_.nursery_1.incoming_arcs);
  generations_.nursery_1.incoming_arcs.clear();

  std::unordered_set<object_header*> old_n2_incoming
    = std::move(generations_.nursery_2.incoming_arcs);
  generations_.nursery_2.incoming_arcs.clear();

  std::vector<object_header*> new_mature_objects;
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

void
free_store::reset_colors(generation max_generation) {
  if (max_generation < generation::mature) {
    for (nursery_generation* g : {&generations_.nursery_1,
                                  &generations_.nursery_2})
      for (object_header* o : g->incoming_arcs)
        set_object_color(o, color::white);
  }

#ifndef NDEBUG
  if (max_generation == generation::mature) {
    for (nursery_generation* g : {&generations_.nursery_1,
                                  &generations_.nursery_2})
      for (object_header* o : g->incoming_arcs)
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
