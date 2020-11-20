#ifndef INSIDER_FREE_STORE_HPP
#define INSIDER_FREE_STORE_HPP

#include <array>
#include <cassert>
#include <cstddef>
#include <functional>
#include <memory>
#include <string>
#include <unordered_set>
#include <utility>
#include <vector>

namespace insider {

class generic_tracked_ptr;
class integer;
struct object;

using word_type = std::uint64_t;

class tracing_context {
public:
  tracing_context(std::vector<object*>& stack, word_type max_generation)
    : stack_{stack}
    , max_generation_{max_generation}
  { }

  void
  trace(object* o);

private:
  std::vector<object*>& stack_;
  word_type             max_generation_;
};

struct type_descriptor {
  char const* name;
  void (*destroy)(object*);
  object* (*move)(object*, std::byte*);
  void (*trace)(object*, tracing_context&);
  void (*update_references)(object*);
  std::size_t (*hash)(object*);

  bool constant_size;
  std::size_t size = 0;
  std::size_t (*get_size)(object*) = nullptr;
  bool permanent_root = false;
};

// Base for any garbage-collectable Scheme object.
struct alignas(sizeof(word_type)) object {
  static constexpr bool is_dynamic_size = false;
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

inline std::vector<type_descriptor>&
types() {
  static std::vector<type_descriptor> value;
  return value;
}

inline bool
is_object_ptr(object* o) {
  return !(reinterpret_cast<word_type>(o) & 1);
}

inline bool
is_fixnum(object* o) { return !is_object_ptr(o); }

inline word_type&
header_word(object* o) {
  assert(is_object_ptr(o));
  return *reinterpret_cast<word_type*>(reinterpret_cast<std::byte*>(o) - sizeof(word_type));
}

word_type
new_type(type_descriptor);

inline std::string
type_name(word_type index) { return types()[index].name; }

inline word_type
type_index(word_type header) { return header >> type_shift; }

inline type_descriptor const&
object_type(word_type header) { return types()[type_index(header)]; }

inline type_descriptor const&
object_type(object* o) { return object_type(header_word(o)); }

inline word_type
object_type_index(object* o) { return type_index(header_word(o)); }

inline word_type
fixnum_payload(object* o) {
  assert(!is_object_ptr(o));
  return reinterpret_cast<word_type>(o) >> 1;
}

inline object*
fixnum_to_ptr(word_type w) noexcept {
  return reinterpret_cast<object*>((w << 1) | 1);
}

constexpr char const* integer_type_name = "insider::fixnum";

inline std::string
object_type_name(object* o) {
  return is_object_ptr(o) ? type_name(object_type_index(o)) : integer_type_name;
}

std::size_t
object_size(object*);

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
is_alive(object*);

word_type
object_generation(object*);

object*
forwarding_address(object*);

template <typename T>
void
update_reference(T*& ref) {
  if (ref && is_object_ptr(ref) && !is_alive(ref))
    ref = static_cast<T*>(forwarding_address(ref));
}

template <typename T>
T*
update_reference_copy(T* ref) {
  if (ref && is_object_ptr(ref) && !is_alive(ref))
    return static_cast<T*>(forwarding_address(ref));
  else
    return ref;
}

namespace detail {
  constexpr std::size_t
  round_to_words(std::size_t s) {
    return (s + sizeof(word_type) - 1) & -sizeof(word_type);
  }

  template <typename T>
  void
  destroy(object* o) { static_cast<T*>(o)->~T(); }

  template <typename T>
  object*
  move(object* o, std::byte* storage) {
    return new (storage) T(std::move(*static_cast<T*>(o)));
  }

  template <typename T>
  void
  trace(object* o, tracing_context& tc) {
    static_cast<T*>(o)->trace(tc);
  }

  template <typename T>
  void
  update_references(object* o) {
    static_cast<T*>(o)->update_references();
  }

  template <typename T>
  std::size_t
  hash(object* o) {
    return static_cast<T*>(o)->hash();
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
  [] (object*, tracing_context&) { },
  [] (object*) { },
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
  detail::trace<Derived>,
  detail::update_references<Derived>,
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
  detail::trace<Derived>,
  detail::update_references<Derived>,
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
  detail::trace<Derived>,
  detail::update_references<Derived>,
  detail::hash<Derived>,
  false,
  0,
  [] (object* o) { return sizeof(Derived) + detail::round_to_words(static_cast<Derived*>(o)->size() * sizeof(T)); }
});

class free_store;

namespace detail {
  template <typename Derived>
  class tracked_ptr_base {
  public:
    tracked_ptr_base() noexcept = default;

    tracked_ptr_base(free_store& fs, object* value) noexcept
      : value_{value}
      , store_{&fs}
    {
      link();
    }

    tracked_ptr_base(tracked_ptr_base const& other) noexcept
      : value_{other.value_}
      , store_{other.store_}
    {
      if (store_)
        link();
    }

    ~tracked_ptr_base() {
      if (store_)
        unlink();
    }

    tracked_ptr_base&
    operator = (tracked_ptr_base const& other) noexcept {
      if (this == &other)
        return *this;

      if (store_ && store_ != other.store_)
        unlink();

      value_ = other.value_;
      store_ = other.store_;

      if (store_ && !prev_)
        link();

      return *this;
    }

    void
    reset() noexcept { value_ = nullptr; }

    object&
    operator * () const noexcept { return *get(); }

    object*
    operator -> () const noexcept { return get(); }

    object*
    get() const noexcept { return value_; }

    explicit
    operator bool () const { return value_ != nullptr; }

    Derived*
    next() const noexcept { return next_; }

    Derived*
    prev() const noexcept { return prev_; }

    free_store&
    store() const noexcept { assert(store_); return *store_; }

  protected:
    friend class insider::free_store;

    object*     value_ = nullptr;
    free_store* store_ = nullptr;
    Derived*    prev_  = nullptr;
    Derived*    next_  = nullptr;

    void
    link() noexcept {
      assert(!prev_);
      assert(!next_);
      assert(store_);

      Derived* root = Derived::root_list(*store_);
      Derived* root_next = root->next_;

      prev_ = root;
      root->next_ = static_cast<Derived*>(this);
      next_ = root_next;

      if (next_)
        next_->prev_ = static_cast<Derived*>(this);

      assert(prev_);
    }

    void
    unlink() noexcept {
      assert(prev_);

      prev_->next_ = next_;
      if (next_)
        next_->prev_ = prev_;

      prev_ = nullptr;
      next_ = nullptr;
    }
  };
} // namespace detail

// Untyped pointer to a Scheme object, registered with the garbage collector as a GC root.
class generic_tracked_ptr : public detail::tracked_ptr_base<generic_tracked_ptr> {
public:
  using tracked_ptr_base::tracked_ptr_base;

  explicit
  generic_tracked_ptr(word_type payload) noexcept {
    value_ = fixnum_to_ptr(payload);
  }

  generic_tracked_ptr&
  operator = (generic_tracked_ptr const& other) noexcept = default;

private:
  friend class tracked_ptr_base;

  static generic_tracked_ptr*
  root_list(free_store& fs) noexcept;
};

// Like generic_ptr, but does not keep an object alive.
class generic_weak_ptr : public detail::tracked_ptr_base<generic_weak_ptr> {
public:
  using tracked_ptr_base::tracked_ptr_base;

  generic_weak_ptr(generic_weak_ptr const& other) noexcept : tracked_ptr_base{other} { }

  generic_weak_ptr&
  operator = (generic_weak_ptr const& other) noexcept = default;

  generic_tracked_ptr
  lock(free_store& fs) const noexcept { return {fs, value_}; }

private:
  friend class tracked_ptr_base;

  static generic_weak_ptr*
  root_list(free_store& fs) noexcept;
};

template <typename Derived>
bool
operator == (detail::tracked_ptr_base<Derived> const& lhs, detail::tracked_ptr_base<Derived> const& rhs) noexcept {
  return lhs.get() == rhs.get();
}

template <typename Derived>
bool
operator != (detail::tracked_ptr_base<Derived> const& lhs, detail::tracked_ptr_base<Derived> const& rhs) noexcept {
  return !operator == (lhs, rhs);
}

// Typed pointer to a garbage-collectable object.
template <typename T>
class tracked_ptr : public generic_tracked_ptr {
public:
  using generic_tracked_ptr::generic_tracked_ptr;

  tracked_ptr&
  operator = (tracked_ptr const& other) noexcept = default;

  T&
  operator * () const noexcept { return *get(); }

  T*
  operator -> () const noexcept { return get(); }

  T*
  get() const noexcept { return static_cast<T*>(generic_tracked_ptr::get()); }
};

// Typed weak pointer to a garbage-collectable object.
template <typename T>
class weak_ptr : public generic_weak_ptr {
public:
  using generic_weak_ptr::generic_weak_ptr;

  weak_ptr(tracked_ptr<T> const& other) noexcept
    : weak_ptr{other.store(), other.get()}
  { }

  weak_ptr&
  operator = (weak_ptr const& other) noexcept = default;

  T&
  operator * () const noexcept { return *get(); }

  T*
  operator -> () const noexcept { return get(); }

  T*
  get() const noexcept { return static_cast<T*>(generic_weak_ptr::get()); }

  tracked_ptr<T>
  lock() const noexcept { return {*store_, get()}; }
};

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
        object* o = reinterpret_cast<object*>(storage + sizeof(word_type));
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
        object* o = reinterpret_cast<object*>(storage + sizeof(word_type));
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

struct generation {
  static constexpr word_type nursery_1 = 0;
  static constexpr word_type nursery_2 = 1;
  static constexpr word_type mature    = 2;

  static constexpr std::size_t num_generations = 3;

  word_type   generation_number;
  dense_space small;
  large_space large;
  std::unordered_set<object*> incoming_arcs;

  explicit
  generation(page_allocator& allocator, word_type generation_number)
    : generation_number{generation_number}
    , small{allocator}
  { }
};

using generation_list = std::array<generation, generation::num_generations>;

// Garbage-collected storage for Scheme objects.
class free_store {
public:
  bool verbose_collection = false;

  free_store();
  free_store(free_store const&) = delete;
  void operator = (free_store const&) = delete;
  ~free_store();

  template <typename T, typename... Args>
  std::enable_if_t<!T::is_dynamic_size, T*>
  make(Args&&... args) {
    static_assert(sizeof(T) % sizeof(word_type) == 0);

    std::byte* storage = allocate_object(sizeof(T), T::type_index);
    object* result = new (storage) T(std::forward<Args>(args)...);

    if (object_type(result).permanent_root)
      permanent_roots_.push_back(result);

    return static_cast<T*>(result);
  }

  template <typename T, typename... Args>
  std::enable_if_t<T::is_dynamic_size, T*>
  make(Args&&... args) {
    std::size_t elements = T::extra_elements(args...);
    std::size_t size = detail::round_to_words(sizeof(T) + elements * sizeof(typename T::element_type));
    std::byte* storage = allocate_object(size, T::type_index);
    object* result = new (storage) T(std::forward<Args>(args)...);

    if (object_type(result).permanent_root)
      permanent_roots_.push_back(result);

    return static_cast<T*>(result);
  }

  void
  notify_arc(object* from, object* to) {
    assert(!object_type(from).permanent_root);

    if (is_object_ptr(to)
        && object_generation(from) == generation::mature
        && object_generation(to) < generation::mature)
      generations_[object_generation(to)].incoming_arcs.emplace(from);
  }

  generic_tracked_ptr*
  root_list() { return roots_; }

  generic_weak_ptr*
  weak_root_list() { return weak_roots_; }

  void
  collect_garbage(bool major = false);

  // Check whether a collection should be performed and, if so, do a collection.
  void
  update();

private:
  page_allocator allocator_;
  generation_list generations_{generation{allocator_, generation::nursery_1},
                               generation{allocator_, generation::nursery_2},
                               generation{allocator_, generation::mature}};

  std::size_t target_nursery_pages_ = 0;
  std::size_t target_nursery_bytes_ = 0;
  std::size_t collection_number_ = 0;

  // Two doubly-linked lists with head.
  generic_tracked_ptr*      roots_ = &root_head_;
  generic_tracked_ptr       root_head_;
  generic_weak_ptr* weak_roots_ = &weak_head_;
  generic_weak_ptr  weak_head_;

  std::vector<object*> permanent_roots_;

  unsigned disable_level_ = 0;
  std::optional<word_type> requested_collection_level_;

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
  reset_colors(word_type max_generation);

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

} // namespace insider

#endif
