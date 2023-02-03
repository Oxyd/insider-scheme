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
#include <type_traits>
#include <unordered_set>
#include <vector>

#ifdef small
#undef small
#endif

namespace insider {

using word_type = std::uint64_t;

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

using object_list = std::vector<object_header*>;

// Garbage-collected storage for Scheme objects.
class free_store {
public:
  bool verbose_collection = false;

  free_store() = default;
  free_store(free_store const&) = delete;
  void operator = (free_store const&) = delete;
  ~free_store();

  template <typename T, typename... Args>
  ptr<T>
  make(Args&&... args) {
    static_assert(std::is_standard_layout_v<object_storage<T>>);
    auto* storage = allocate_storage<T>(detail::allocation_size<T>(args...));
    new (&storage->payload_storage) T(std::forward<Args>(args)...);
    all_objects_.push_back(&storage->header);
    return ptr<T>{storage};
  }

  void
  notify_arc(ptr<>, ptr<>) { }

  insider::root_list&
  root_list() { return roots_; }

  void
  collect_garbage(bool major = false);

  // Check whether a collection should be performed and, if so, do a collection.
  void
  update() {
    collect_garbage();
  }

  void
  disable_gc() { ++disable_level_; }

  void
  enable_gc() {
    --disable_level_;
    if (disable_level_ == 0)
      update();
  }

private:
  static constexpr std::size_t large_threshold = 256;

  object_list all_objects_;
  insider::root_list roots_;

  unsigned disable_level_ = 0;

  hash_generator next_hash_;

  // Allocate storage for the given payload type and initialise its header.
  template <typename T>
  object_storage<T>*
  allocate_storage(std::size_t size) {
    auto* buffer = new std::byte[size];
    auto* storage = new (buffer) object_storage<T>;
    storage->header = make_object_header(T::type_index, next_hash_(),
                                         generation::nursery_1);

    return storage;
  }
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
