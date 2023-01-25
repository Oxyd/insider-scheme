#ifndef INSIDER_MEMORY_FREE_STORE_HPP
#define INSIDER_MEMORY_FREE_STORE_HPP

#include "memory/page_allocator.hpp"
#include "memory/root_list.hpp"
#include "object.hpp"

#include <cassert>
#include <cstddef>
#include <list>
#include <memory>
#include <optional>
#include <string>
#include <unordered_set>
#include <vector>

#ifdef small
#undef small
#endif

namespace insider {

using word_type = std::uint64_t;

bool
is_alive(ptr<>);

inline bool
is_valid(ptr<> o) {
  return !is_object_ptr(o) || is_alive(o);
}

class dense_space {
public:
  struct page {
    page_allocator::page storage;
    std::size_t used = 0;

    template <typename F>
    void
    for_all(F const& f) {
      std::size_t i = 0;
      while (i < used) {
        auto* object_storage
          = reinterpret_cast<abstract_object_storage*>(storage.get() + i);
        std::size_t size = storage_size(object_storage);
        f(object_storage);
        i += size;
      }
    }

    template <typename F>
    void
    for_all(F const& f) const {
      std::size_t i = 0;
      while (i < used) {
        auto* object_storage
          = reinterpret_cast<abstract_object_storage*>(storage.get() + i);
        std::size_t size = storage_size(object_storage);
        f(object_storage);
        i += size;
      }
    }
  };

  dense_space() = default;

  explicit
  dense_space(page_allocator&);

  ~dense_space();

  dense_space(dense_space const&) = delete;
  dense_space(dense_space&&) noexcept;

  void operator = (dense_space const&) = delete;
  dense_space& operator = (dense_space&&) noexcept;

  std::byte*
  allocate(std::size_t);

  void
  clear();

  bool
  has_preallocated_storage(std::size_t) const;

  bool
  empty() const { return total_used_ == 0; }

  template <typename F>
  void
  for_all(F const& f) {
    for (page const& p : pages_)
      p.for_all(f);
  }

  template <typename F>
  void
  for_all(F const& f) const {
    for (page const& p : pages_)
      p.for_all(f);
  }

  page&
  take(page_allocator::page, std::size_t used);

  page_allocator&
  allocator() { return *allocator_; }

  std::size_t
  pages_used() const { return pages_.size(); }

  std::size_t
  bytes_used() const { return total_used_; }

private:
  page_allocator* allocator_ = nullptr;
  std::list<page> pages_;
  std::size_t total_used_ = 0;
};

class large_space {
public:
  std::byte*
  allocate(std::size_t);

  std::size_t
  object_count() const { return allocations_.size(); }

  bool
  empty() const { return allocations_.empty(); }

  std::size_t
  bytes_used() const { return bytes_used_; }

  std::byte*
  get(std::size_t i) const { return allocations_[i].get(); }

  void
  move(std::size_t i, large_space& to);

  void
  stage_for_deallocation(std::size_t i);

  void
  deallocate_staged();

  template <typename F>
  void
  for_all(F const& f) {
    for (auto const& storage : allocations_) {
      auto* o = reinterpret_cast<abstract_object_storage*>(storage.get());
      f(o);
    }
  }

private:
  std::vector<std::unique_ptr<std::byte[]>> allocations_;
  std::vector<std::size_t> to_deallocate_;
  std::size_t bytes_used_ = 0;

  void
  compact();
};

inline bool
operator < (generation lhs, generation rhs) {
  return static_cast<word_type>(lhs) < static_cast<word_type>(rhs);
}

inline bool
operator == (generation lhs, generation rhs) {
  return static_cast<word_type>(lhs) == static_cast<word_type>(rhs);
}

inline bool
operator <= (generation lhs, generation rhs) {
  return lhs < rhs || lhs == rhs;
}

inline bool
operator > (generation lhs, generation rhs) {
  return !(lhs <= rhs);
}

inline bool
operator >= (generation lhs, generation rhs) {
  return lhs > rhs || lhs == rhs;
}

struct nursery_generation {
  generation  generation_number;
  dense_space small;
  large_space large;
  std::unordered_set<abstract_object_storage*> incoming_arcs;

  nursery_generation(page_allocator& allocator, generation generation_number)
    : generation_number{generation_number}
    , small{allocator}
  { }
};

struct mature_generation {
  static constexpr generation generation_number = generation::mature;

  dense_space small;
  large_space large;

  mature_generation(page_allocator& allocator)
    : small{allocator}
  { }
};

namespace detail {
  template <typename T, typename... Args>
  std::size_t
  allocation_size([[maybe_unused]] Args&&... args) {
    if constexpr (T::is_dynamic_size) {
      std::size_t elements = T::extra_elements(args...);
      return detail::round_to_words(sizeof(object_storage<T>) +
                                    elements * sizeof(typename T::element_type));
    } else {
      static_assert(sizeof(object_storage<T>) % sizeof(word_type) == 0);
      return sizeof(object_storage<T>);
    }
  }
}

// Garbage-collected storage for Scheme objects.
class free_store {
public:
  struct generations {
    nursery_generation nursery_1;
    nursery_generation nursery_2;
    mature_generation  mature;
  };

  bool verbose_collection = false;

  free_store();
  free_store(free_store const&) = delete;
  void operator = (free_store const&) = delete;
  ~free_store();

  template <typename T, typename... Args>
  ptr<T>
  make(Args&&... args) {
    static_assert(sizeof(T) % sizeof(word_type) == 0);

    auto* storage = allocate_storage<T>(detail::allocation_size<T>(args...));
    new (&storage->payload_storage) T(std::forward<Args>(args)...);

    return ptr_cast<T>(ptr<>{storage});
  }

  void
  notify_arc(ptr<> from, ptr<> to) {
    if (to && is_object_ptr(to)
        && object_generation(from) > object_generation(to)) {
      switch (object_generation(to)) {
      case generation::nursery_1:
        generations_.nursery_1.incoming_arcs.emplace(from.storage());
        break;

      case generation::nursery_2:
        generations_.nursery_2.incoming_arcs.emplace(from.storage());
        break;

      default:
        assert(!"Incoming arc to mature generation");
        break;
      }
    }
  }

  void
  make_permanent_arc(ptr<> from);

  void
  transfer_to_nursery(page_allocator::page, std::size_t used);

  insider::root_list&
  root_list() { return roots_; }

  void
  collect_garbage(bool major = false);

  // Check whether a collection should be performed and, if so, do a collection.
  void
  update() {
    if (requested_collection_level_)
      collect_garbage(*requested_collection_level_ == generation::mature);
  }

  void
  disable_gc() { ++disable_level_; }

  void
  enable_gc() {
    --disable_level_;
    if (disable_level_ == 0)
      update();
  }

  page_allocator&
  allocator() { return allocator_; }

private:
  static constexpr std::size_t large_threshold = 256;

  page_allocator allocator_;
  generations    generations_{{allocator_, generation::nursery_1},
                              {allocator_, generation::nursery_2},
                              {allocator_}};

  std::size_t target_nursery_pages_ = 0;
  std::size_t target_nursery_bytes_ = 0;
  std::size_t collection_number_ = 0;

  insider::root_list roots_;

  unsigned disable_level_ = 0;
  std::optional<generation> requested_collection_level_;

  hash_generator next_hash_;

  // Allocate storage for the given payload type and initialise its header.
  template <typename T>
  object_storage<T>*
  allocate_storage(std::size_t size) {
    std::byte* buffer = nullptr;
    if (size >= large_threshold)
      buffer = generations_.nursery_1.large.allocate(size);
    else
      buffer = generations_.nursery_1.small.allocate(size);

    check_nursery_size();

    auto* storage = new (buffer) object_storage<T>;
    storage->header = make_object_header(T::type_index, next_hash_(),
                                         generation::nursery_1);

    return storage;
  }

  void
  reset_colors(generation max_generation);

  void
  check_nursery_size();

  void
  request_collection();
};

class gc_disabler {
public:
  explicit
  gc_disabler(free_store& fs) : fs_{&fs} { fs_->disable_gc(); }

  ~gc_disabler() { enable_gc(); }

  gc_disabler(gc_disabler const&) = delete;
  void operator = (gc_disabler const&) = delete;

  void
  enable_gc() {
    if (fs_) {
      fs_->enable_gc();
      fs_ = nullptr;
    }
  }

private:
  free_store* fs_;
};

} // namespace insider

#endif
