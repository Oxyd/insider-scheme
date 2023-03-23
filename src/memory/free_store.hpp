#ifndef INSIDER_MEMORY_FREE_STORE_HPP
#define INSIDER_MEMORY_FREE_STORE_HPP

#include "memory/allocator.hpp"
#include "memory/root_provider.hpp"
#include "object.hpp"
#include "type_indexes.hpp"

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

// A container that holds a weak reference to an object. If the referred-to
// object is only referred by weak boxes, it is reclaimed by the collector and
// the weak boxes are reset to nullptr.
class weak_box : public composite_object<weak_box> {
public:
  static constexpr char const* scheme_name = "insider::weak_box";
  static constexpr word_type static_type_index = type_indexes::weak_box;

  explicit
  weak_box(ptr<> value) : value_{value} { }

  ptr<>
  get() const { return value_; }

  void
  set(free_store& store, ptr<> value);

  void
  reset() {
    value_ = nullptr;
  }

  void
  visit_members(member_visitor const&) const {
    // Don't visit the only member: This box doesn't contribute a reference to
    // it, and the store treats weak_boxes specially so it will reset this box
    // if value_ becomes garbage.
  }

private:
  ptr<> value_;
};

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

    std::size_t size = detail::allocation_size<T>(args...);
    auto* storage = allocate_storage<T>(size);
    increase_alloc_size(size);

    ptr<T> result{
      new (&storage->payload_storage) T(std::forward<Args>(args)...)
    };

    if constexpr (std::is_same_v<T, weak_box>)
      weak_boxes_.push_back(result);
    else
      nursery_.push_back(&storage->header);
    return result;
  }

  void
  notify_arc(ptr<> from, ptr<> to) {
    if (to && is_object_ptr(to) && !object_remembered(from)) {
      if (object_age(from) == mature_age && object_age(to) < mature_age) {
        remembered_set_.push_back(from.header());
        mark_object_remembered(from);
      } else if (object_age(from) > object_age(to)) {
        mark_needs_scan_on_promote(from);
      }
    }
  }

  insider::root_list&
  root_list() { return roots_; }

  void
  collect_garbage(bool force_major = false);

  // Check whether a collection should be performed and, if so, do a collection.
  void
  update() {
    if (want_collection_)
      collect_garbage();
  }

private:
  static constexpr std::size_t min_alloc_size = 32ull * 1024 * 1024;
  static constexpr std::size_t nursery_threshold = 8ull * 1024 * 1024;

  allocator                  alloc_;
  object_list                nursery_;
  object_list                mature_;
  std::vector<ptr<weak_box>> weak_boxes_;
  insider::root_list         roots_;
  object_list                remembered_set_;

  std::size_t                alloc_size_ = 0;
  std::size_t                threshold_alloc_size_ = min_alloc_size;
  std::size_t                nursery_alloc_size_ = 0;
  bool                       want_collection_ = false;

  // Allocate storage for the given payload type and initialise its header.
  template <typename T>
  object_storage<T>*
  allocate_storage(std::size_t size) {
    std::byte* buffer = alloc_.allocate(size);
    auto* storage = new (buffer) object_storage<T>;
    storage->header = make_header(T::type_index);

    return storage;
  }

  void
  increase_alloc_size(std::size_t growth) {
    alloc_size_ += growth;
    nursery_alloc_size_ += growth;

    if (alloc_size_ > threshold_alloc_size_
        || nursery_alloc_size_ > nursery_threshold)
      want_collection_ = true;
  }

  void
  collect_major();

  void
  collect_minor();

  void
  check_consistency();
};

} // namespace insider

#endif
