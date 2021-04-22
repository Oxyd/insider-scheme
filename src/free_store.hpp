#ifndef INSIDER_FREE_STORE_HPP
#define INSIDER_FREE_STORE_HPP

#include <array>
#include <cassert>
#include <cstddef>
#include <functional>
#include <memory>
#include <optional>
#include <string>
#include <unordered_set>
#include <utility>
#include <vector>

namespace insider {

class generic_tracked_ptr;
class integer;
struct object;

using word_type = std::uint64_t;
enum class generation : word_type;

template <typename = void>
class ptr;

template <>
class ptr<> {
public:
  ptr() = default;

  ptr(object* value) : value_{value} { }

  ptr(std::nullptr_t) { }

  explicit
  operator bool () const { return value_ != nullptr; }

  void
  reset(object* new_value) { value_ = new_value; }

  object*
  value() const { return value_; }

protected:
  object* value_ = nullptr;
};

template <typename T>
class ptr : public ptr<> {
public:
  ptr() = default;

  ptr(T* value) : ptr<>(value) { }

  ptr(std::nullptr_t) { }

  T*
  operator -> () const { return static_cast<T*>(value_); }

  T&
  operator * () const { return *static_cast<T*>(value_); }

  T*
  value() const { return static_cast<T*>(value_); }
};

template <typename T, typename U>
bool
operator == (ptr<T> lhs, ptr<U> rhs) { return lhs.value() == rhs.value(); }

template <typename T>
bool
operator == (ptr<T> lhs, std::nullptr_t) { return lhs.value() == nullptr; }

template <typename T>
bool
operator == (std::nullptr_t, ptr<T> rhs) { return rhs.value() == nullptr; }

template <typename T, typename U>
bool
operator != (ptr<T> lhs, ptr<U> rhs) { return lhs.value() != rhs.value(); }

template <typename T>
bool
operator != (ptr<T> lhs, std::nullptr_t) { return lhs.value() != nullptr; }

template <typename T>
bool
operator != (std::nullptr_t, ptr<T> rhs) { return rhs.value() != nullptr; }

template <typename T>
bool
operator < (ptr<T> lhs, ptr<T> rhs) { return std::less<T*>{}(lhs.value(), rhs.value()); }

} // namespace insider

namespace std {
  template <typename T>
  struct hash<insider::ptr<T>> {
    auto
    operator () (insider::ptr<T> value) const { return std::hash<insider::object*>{}(value.value()); }
  };
} // namespace std

namespace insider {

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

template <typename T>
ptr<T>
ptr_cast(ptr<> value) {
  assert(!value || is<T>(value));
  return ptr<T>{static_cast<T*>(value.value())};
}

template <>
inline ptr<>
ptr_cast<void>(ptr<> value) { return value; }

class tracing_context {
public:
  tracing_context(std::vector<ptr<>>& stack, generation max_generation)
    : stack_{stack}
    , max_generation_{max_generation}
  { }

  void
  trace(ptr<> o);

private:
  std::vector<ptr<>>& stack_;
  generation            max_generation_;
};

struct type_descriptor {
  char const* name;
  void (*destroy)(ptr<>);
  ptr<> (*move)(ptr<>, std::byte*);
  void (*trace)(ptr<>, tracing_context&);
  void (*update_references)(ptr<>);
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

ptr<>
forwarding_address(ptr<>);

template <typename T>
void
update_reference(ptr<T>& ref) {
  if (ref && is_object_ptr(ref) && !is_alive(ref)) {
    ref = ptr_cast<T>(forwarding_address(ref));
    assert(is_alive(ref));
  }
}

template <typename T>
T*
update_reference_copy(T* ref) {
  update_reference(ref);
  return ref;
}

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
  trace(ptr<> o, tracing_context& tc) {
    static_cast<T const*>(o.value())->trace(tc);
  }

  template <typename T>
  void
  update_references(ptr<> o) {
    static_cast<T*>(o.value())->update_references();
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
  [] (ptr<>, tracing_context&) { },
  [] (ptr<>) { },
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
  detail::size<Derived, T>
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

    tracked_ptr_base(free_store& fs, ptr<> value) noexcept
      : tracked_ptr_base(fs, value.value())
    { }

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

    ptr<>
    operator -> () const noexcept { return get(); }

    ptr<>
    get() const noexcept { return ptr<>{value_}; }

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

  generic_tracked_ptr&
  operator = (generic_tracked_ptr const& other) noexcept = default;

private:
  friend class detail::tracked_ptr_base<generic_tracked_ptr>;

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
  friend class detail::tracked_ptr_base<generic_weak_ptr>;

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
  operator -> () const noexcept { return get().value(); }

  ptr<T>
  get() const noexcept { return ptr_cast<T>(generic_tracked_ptr::get()); }
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

  ptr<T>
  get() const noexcept { return ptr_cast<T>(generic_weak_ptr::get()); }

  tracked_ptr<T>
  lock() const noexcept { return {*store_, get()}; }
};

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

  explicit
  mature_generation(page_allocator& allocator)
    : small{allocator}
  { }
};

// Garbage-collected storage for Scheme objects.
class free_store {
public:
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

    if (object_type(result).permanent_root)
      permanent_roots_.push_back(result);

    return ptr_cast<T>(result);
  }

  template <typename T, typename... Args>
  std::enable_if_t<T::is_dynamic_size, ptr<T>>
  make(Args&&... args) {
    std::size_t elements = T::extra_elements(args...);
    std::size_t size = detail::round_to_words(sizeof(T) + elements * sizeof(typename T::element_type));
    std::byte* storage = allocate_object(size, T::type_index);
    ptr<> result = new (storage) T(std::forward<Args>(args)...);

    if (object_type(result).permanent_root)
      permanent_roots_.push_back(result);

    return ptr_cast<T>(result);
  }

  void
  notify_arc(ptr<> from, ptr<> to) {
    assert(!object_type(from).permanent_root);

    if (to && is_object_ptr(to) && object_generation(from) > object_generation(to)) {
      switch (object_generation(to)) {
      case generation::nursery_1:
        nursery_1_.incoming_arcs.emplace(from);
        break;

      case generation::nursery_2:
        nursery_2_.incoming_arcs.emplace(from);
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
  nursery_generation nursery_1_{allocator_, generation::nursery_1};
  nursery_generation nursery_2_{allocator_, generation::nursery_2};
  mature_generation  mature_{allocator_};

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
