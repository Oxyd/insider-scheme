#ifndef INSIDER_FREE_STORE_HPP
#define INSIDER_FREE_STORE_HPP

#include "call_stack.hpp"
#include "object.hpp"
#include "ptr.hpp"

#include <cassert>
#include <cstddef>
#include <memory>
#include <optional>
#include <string>
#include <unordered_set>
#include <vector>

namespace insider {

using word_type = std::uint64_t;

// Is a given object an instance of the given Scheme type?
template <typename T>
bool
is(ptr<> x) {
  assert(x);
  return is_object_ptr(x) && object_type_index(x) == T::type_index;
}

template <>
inline bool
is<integer>(ptr<> x) {
  return is_fixnum(x);
}

bool
is_alive(ptr<>);

inline bool
is_valid(ptr<> o) {
  return !is_object_ptr(o) || is_alive(o);
}

class page_allocator {
public:
  using page = std::unique_ptr<std::byte[]>;

  page
  allocate();

  void
  deallocate(page);

  void
  keep_at_most(std::size_t);

  std::size_t
  reserve_pages() const { return reserve_.size(); }

  std::size_t
  allocated_pages() const { return allocated_pages_; }

  std::size_t
  deallocated_pages() const { return deallocated_pages_; }

  void
  reset_stats() {
    allocated_pages_ = 0;
    deallocated_pages_ = 0;
  }

private:
  std::vector<page> reserve_;
  std::size_t allocated_pages_ = 0;
  std::size_t deallocated_pages_ = 0;
};

class dense_space {
  struct page {
    std::unique_ptr<std::byte[]> storage;
    std::size_t used = 0;
  };

public:
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
    for (page const& p : pages_) {
      std::size_t i = 0;
      while (i < p.used) {
        std::byte* storage = p.storage.get() + i;
        ptr<> o{reinterpret_cast<object*>(storage + sizeof(word_type))};
        std::size_t size = object_size(o);

        f(o);

        i += size + sizeof(word_type);
      }
    }
  }

  template <typename F>
  void
  for_all(F const& f) const {
    for (page const& p : pages_) {
      std::size_t i = 0;
      while (i < p.used) {
        std::byte* storage = p.storage.get() + i;
        ptr<> o{reinterpret_cast<object*>(storage + sizeof(word_type))};
        std::size_t size = object_size(o);

        f(o);

        i += size + sizeof(word_type);
      }
    }
  }

  page_allocator&
  allocator() { return *allocator_; }

  std::size_t
  pages_used() const { return pages_.size(); }

  std::size_t
  bytes_used() const { return total_used_; }

private:
  page_allocator* allocator_ = nullptr;
  std::vector<page> pages_;
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
  deallocate(std::size_t i);

  void
  remove_empty();

private:
  std::vector<std::unique_ptr<std::byte[]>> allocations_;
  std::size_t bytes_used_ = 0;
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
  std::unordered_set<ptr<>> incoming_arcs;

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
  allocation_size(Args&&... args) {
    if constexpr (T::is_dynamic_size) {
      std::size_t elements = T::extra_elements(args...);
      return detail::round_to_words(sizeof(T) + elements * sizeof(typename T::element_type));
    } else {
      static_assert(sizeof(T) % sizeof(word_type) == 0);
      return sizeof(T);
    }
  }
}

// Garbage-collected storage for Scheme objects.
class free_store {
public:
  static constexpr std::size_t large_threshold = 256;

  struct generations {
    stack_cache        stack;
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

    std::byte* storage = allocate_object(detail::allocation_size<T>(args...),
                                         T::type_index);
    ptr<> result = new (storage) T(std::forward<Args>(args)...);

    return ptr_cast<T>(result);
  }

  void
  notify_arc(ptr<> from, ptr<> to) {
    assert(!object_type(from).permanent_root);

    if (to && is_object_ptr(to) && object_generation(from) > object_generation(to)) {
      switch (object_generation(to)) {
      case generation::nursery_1:
        generations_.nursery_1.incoming_arcs.emplace(from);
        break;

      case generation::nursery_2:
        generations_.nursery_2.incoming_arcs.emplace(from);
        break;

      default:
        assert(!"Incoming arc to mature generation");
        break;
      }
    }
  }

  generic_tracked_ptr*
  root_list() { return roots_; }

  generic_weak_ptr*
  weak_root_list() { return weak_roots_; }

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

  stack_cache&
  stack() { return generations_.stack; }

private:
  page_allocator allocator_;
  generations    generations_{{},
                              {allocator_, generation::nursery_1},
                              {allocator_, generation::nursery_2},
                              {allocator_}};

  std::size_t target_nursery_pages_ = 0;
  std::size_t target_nursery_bytes_ = 0;
  std::size_t collection_number_ = 0;

  // Two doubly-linked lists with head.
  generic_tracked_ptr* roots_      = &root_head_;
  generic_tracked_ptr  root_head_;
  generic_weak_ptr*    weak_roots_ = &weak_head_;
  generic_weak_ptr     weak_head_;

  std::vector<ptr<>> permanent_roots_;

  unsigned disable_level_ = 0;
  std::optional<generation> requested_collection_level_;

  // Allocate storage of the given payload size for an object of the given
  // type. size does not include the size of the header. The object is not
  // constructed in the storage.
  std::byte*
  allocate_object(std::size_t size, word_type type);

  void
  update_roots();

  void
  update_permanent_roots();

  void
  reset_colors(generation max_generation);

  void
  check_nursery_size();

  void
  request_collection();
};

inline generic_tracked_ptr*
generic_tracked_ptr::root_list(free_store& fs) noexcept {
  return fs.root_list();
}

inline generic_weak_ptr*
generic_weak_ptr::root_list(free_store& fs) noexcept {
  return fs.weak_root_list();
}

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

  void
  force_update() {
    if (fs_) {
      fs_->enable_gc();
      fs_->disable_gc();
    }
  }

private:
  free_store* fs_;
};

} // namespace insider

#endif
