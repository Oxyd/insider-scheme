#ifndef INSIDER_FREE_STORE_HPP
#define INSIDER_FREE_STORE_HPP

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
enum class generation : word_type;

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

inline bool
is_object_ptr(ptr<> o) {
  return !(reinterpret_cast<word_type>(o.value()) & 1);
}

inline bool
is_fixnum(ptr<> o) { return !is_object_ptr(o); }

inline word_type&
header_word(ptr<> o) {
  assert(is_object_ptr(o));
  return *reinterpret_cast<word_type*>(reinterpret_cast<std::byte*>(o.value()) - sizeof(word_type));
}

inline word_type
type_index(word_type header) { return header >> type_shift; }

inline word_type
object_type_index(ptr<> o) { return type_index(header_word(o)); }

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

using member_visitor = std::function<void(ptr<>&)>;

struct type_descriptor {
  char const* name;
  void (*destroy)(ptr<>);
  ptr<> (*move)(ptr<>, std::byte*);
  void (*visit_members)(ptr<>, member_visitor const&);
  std::size_t (*hash)(ptr<>);

  bool constant_size;
  std::size_t size = 0;
  std::size_t (*get_size)(ptr<>) = nullptr;
  bool permanent_root = false;
};

// Base for any garbage-collectable Scheme object.
struct alignas(sizeof(word_type)) object {
  static constexpr bool is_dynamic_size = false;
};

inline std::vector<type_descriptor>&
types() {
  static std::vector<type_descriptor> value;
  return value;
}

word_type
new_type(type_descriptor);

inline std::string
type_name(word_type index) { return types()[index].name; }

inline type_descriptor const&
object_type(word_type header) { return types()[type_index(header)]; }

inline type_descriptor const&
object_type(ptr<> o) { return object_type(header_word(o)); }

inline word_type
tagged_payload(ptr<> o) {
  assert(!is_object_ptr(o));
  return reinterpret_cast<word_type>(o.value());
}

inline ptr<>
immediate_to_ptr(word_type w) noexcept {
  assert(w & 1);
  return ptr<>{reinterpret_cast<object*>(w)};
}

constexpr char const* integer_type_name = "insider::fixnum";

inline std::string
object_type_name(ptr<> o) {
  return is_object_ptr(o) ? type_name(object_type_index(o)) : integer_type_name;
}

std::size_t
object_size(ptr<>);

template <typename T>
std::string
type_name() {
  return type_name(T::type_index);
}

template <>
inline std::string
type_name<integer>() {
  return integer_type_name;
}

bool
is_alive(ptr<>);

inline bool
is_valid(ptr<> o) {
  return !is_object_ptr(o) || is_alive(o);
}

generation
object_generation(ptr<>);

namespace detail {
  constexpr std::size_t
  round_to_words(std::size_t s) {
    return (s + sizeof(word_type) - 1) & -sizeof(word_type);
  }

  template <typename T>
  void
  destroy(ptr<> o) { static_cast<T*>(o.value())->~T(); }

  template <typename T>
  ptr<>
  move(ptr<> o, std::byte* storage) {
    return new (storage) T(std::move(*static_cast<T*>(o.value())));
  }

  template <typename T>
  void
  visit_members(ptr<> o, member_visitor const& f) {
    static_cast<T*>(o.value())->visit_members(f);
  }

  template <typename T>
  std::size_t
  hash(ptr<> o) {
    return static_cast<T const*>(o.value())->hash();
  }

  template <typename T, typename U>
  std::size_t
  size(ptr<> o) {
    return sizeof(T) + detail::round_to_words(static_cast<T*>(o.value())->size() * sizeof(U));
  }
}

// Object with no Scheme subobjects.
template <typename Derived>
struct leaf_object : object {
  static word_type const type_index;
};

template <typename Derived>
word_type const leaf_object<Derived>::type_index = new_type(type_descriptor{
  Derived::scheme_name,
  detail::destroy<Derived>,
  detail::move<Derived>,
  [] (ptr<>, member_visitor const&) { },
  detail::hash<Derived>,
  true,
  detail::round_to_words(sizeof(Derived)),
  nullptr
});

// Object with a constant number of Scheme subobjects.
template <typename Derived>
struct composite_object : object {
  static word_type const type_index;
};

template <typename Derived>
word_type const composite_object<Derived>::type_index = new_type(type_descriptor{
  Derived::scheme_name,
  detail::destroy<Derived>,
  detail::move<Derived>,
  detail::visit_members<Derived>,
  detail::hash<Derived>,
  true,
  detail::round_to_words(sizeof(Derived)),
  nullptr
});

// Object that is allocated directly in the mature generation and is always
// considered a source of roots for all generations.
template <typename Derived>
struct composite_root_object : object {
  static word_type const type_index;
};

template <typename Derived>
word_type const composite_root_object<Derived>::type_index = new_type(type_descriptor{
  Derived::scheme_name,
  detail::destroy<Derived>,
  detail::move<Derived>,
  detail::visit_members<Derived>,
  detail::hash<Derived>,
  true,
  detail::round_to_words(sizeof(Derived)),
  nullptr,
  true
});

// Object whose size is determined at instantiation time.
template <typename Derived, typename T>
struct alignas(T) alignas(object) dynamic_size_object : object {
  using element_type = T;
  static constexpr bool is_dynamic_size = true;
  static word_type const type_index;

protected:
  T&
  storage_element(std::size_t i) {
    return reinterpret_cast<T*>(reinterpret_cast<std::byte*>(this) + sizeof(Derived))[i];
  }

  T const&
  storage_element(std::size_t i) const {
    return reinterpret_cast<T const*>(reinterpret_cast<std::byte const*>(this) + sizeof(Derived))[i];
  }
};

template <typename Derived, typename T>
word_type const dynamic_size_object<Derived, T>::type_index = new_type(type_descriptor{
  Derived::scheme_name,
  detail::destroy<Derived>,
  detail::move<Derived>,
  detail::visit_members<Derived>,
  detail::hash<Derived>,
  false,
  0,
  detail::size<Derived, T>
});

template <typename T>
bool
is(generic_tracked_ptr const& x) {
  return is<T>(x.get());
}

struct page {
  std::unique_ptr<std::byte[]> storage;
  std::size_t used = 0;
};

struct space {
  std::vector<page> pages;
  std::size_t       current = 0;
};

class page_allocator {
public:
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

enum class generation : word_type {
  nursery_1 = 0,
  nursery_2 = 1,
  mature    = 2
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

static constexpr std::size_t num_generations = 3;

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
  std::enable_if_t<!T::is_dynamic_size, ptr<T>>
  make(Args&&... args) {
    static_assert(sizeof(T) % sizeof(word_type) == 0);

    std::byte* storage = allocate_object(sizeof(T), T::type_index);
    ptr<> result = new (storage) T(std::forward<Args>(args)...);

    return ptr_cast<T>(result);
  }

  template <typename T, typename... Args>
  std::enable_if_t<T::is_dynamic_size, ptr<T>>
  make(Args&&... args) {
    std::size_t elements = T::extra_elements(args...);
    std::size_t size = detail::round_to_words(sizeof(T) + elements * sizeof(typename T::element_type));
    std::byte* storage = allocate_object(size, T::type_index);
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

private:
  page_allocator allocator_;
  generations    generations_{{allocator_, generation::nursery_1},
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
