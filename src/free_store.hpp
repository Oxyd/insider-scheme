#ifndef SCHEME_FREE_STORE_HPP
#define SCHEME_FREE_STORE_HPP

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

class generic_ptr;
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
  std::string name;
  void (*destroy)(object*);
  object* (*move)(object*, std::byte*);
  void (*trace)(object*, tracing_context&);
  void (*update_references)(object*);

  bool constant_size;
  std::size_t size = 0;
  std::size_t (*get_size)(object*) = nullptr;
};

// Base for any garbage-collectable Scheme object.
struct alignas(sizeof(word_type)) object {
  static constexpr bool is_dynamic_size = false;
};

word_type
new_type(type_descriptor);

type_descriptor const&
object_type(object*);

word_type
object_type_index(object*);

std::string
type_name(word_type);

std::size_t
object_size(object*);

template <typename T>
std::string
type_name() {
  return type_name(T::type_index);
}

constexpr char const* integer_type_name = "fixnum";

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

bool
is_object_ptr(object*);

inline bool
is_fixnum(object* o) { return !is_object_ptr(o); }

word_type
fixnum_payload(object*);

object*
fixnum_to_ptr(word_type);

inline std::string
object_type_name(object* o) {
  return is_object_ptr(o) ? type_name(object_type_index(o)) : integer_type_name;
}

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

  std::string
  demangle(char const* name);
}

// Object with no Scheme subobjects.
template <typename Derived>
struct leaf_object : object {
  static word_type const type_index;
};

template <typename Derived>
word_type const leaf_object<Derived>::type_index = new_type(type_descriptor{
  detail::demangle(typeid(Derived).name()),
  detail::destroy<Derived>,
  detail::move<Derived>,
  [] (object*, tracing_context&) { },
  [] (object*) { },
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
  detail::demangle(typeid(Derived).name()),
  detail::destroy<Derived>,
  detail::move<Derived>,
  detail::trace<Derived>,
  detail::update_references<Derived>,
  true,
  detail::round_to_words(sizeof(Derived)),
  nullptr
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
  detail::demangle(typeid(Derived).name()),
  detail::destroy<Derived>,
  detail::move<Derived>,
  detail::trace<Derived>,
  detail::update_references<Derived>,
  false,
  0,
  [] (object* o) { return sizeof(Derived) + detail::round_to_words(static_cast<Derived*>(o)->size() * sizeof(T)); }
});

class free_store;

namespace detail {
  template <typename Derived>
  class generic_ptr_base {
  public:
    generic_ptr_base() = default;

    generic_ptr_base(free_store& fs, object* value)
      : value_{value}
      , store_{&fs}
    {
      link();
    }

    generic_ptr_base(generic_ptr_base const& other)
      : value_{other.value_}
      , store_{other.store_}
    {
      if (store_)
        link();
    }

    ~generic_ptr_base() {
      if (store_)
        unlink();
    }

    generic_ptr_base&
    operator = (generic_ptr_base const& other) {
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
    reset() { value_ = nullptr; }

    object&
    operator * () const { return *get(); }

    object*
    operator -> () const { return get(); }

    object*
    get() const { return value_; }

    explicit
    operator bool () const { return value_ != nullptr; }

    Derived*
    next() const { return next_; }

    Derived*
    prev() const { return prev_; }

    free_store&
    store() const { assert(store_); return *store_; }

  protected:
    friend class insider::free_store;

    object*     value_ = nullptr;
    free_store* store_ = nullptr;
    Derived*    prev_  = nullptr;
    Derived*    next_  = nullptr;

    void
    link() {
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
    unlink() {
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
class generic_ptr : public detail::generic_ptr_base<generic_ptr> {
public:
  using generic_ptr_base::generic_ptr_base;

  explicit
  generic_ptr(word_type payload) {
    value_ = fixnum_to_ptr(payload);
  }

  generic_ptr&
  operator = (generic_ptr const& other) = default;

private:
  friend class generic_ptr_base;

  static generic_ptr*
  root_list(free_store& fs);
};

// Like generic_ptr, but does not keep an object alive.
class generic_weak_ptr : public detail::generic_ptr_base<generic_weak_ptr> {
public:
  using generic_ptr_base::generic_ptr_base;

  generic_weak_ptr(generic_weak_ptr const& other) : generic_ptr_base{other} { }

  generic_weak_ptr&
  operator = (generic_weak_ptr const& other) = default;

  generic_ptr
  lock(free_store& fs) const { return {fs, value_}; }

private:
  friend class generic_ptr_base;

  static generic_weak_ptr*
  root_list(free_store& fs);
};

template <typename Derived>
bool
operator == (detail::generic_ptr_base<Derived> const& lhs, detail::generic_ptr_base<Derived> const& rhs) {
  return lhs.get() == rhs.get();
}

template <typename Derived>
bool
operator != (detail::generic_ptr_base<Derived> const& lhs, detail::generic_ptr_base<Derived> const& rhs) {
  return !operator == (lhs, rhs);
}

// Typed pointer to a garbage-collectable object.
template <typename T>
class ptr : public generic_ptr {
public:
  using generic_ptr::generic_ptr;

  ptr&
  operator = (ptr const& other) = default;

  T&
  operator * () const { return *get(); }

  T*
  operator -> () const { return get(); }

  T*
  get() const { return static_cast<T*>(generic_ptr::get()); }
};

// Typed weak pointer to a garbage-collectable object.
template <typename T>
class weak_ptr : public generic_weak_ptr {
public:
  using generic_weak_ptr::generic_weak_ptr;

  weak_ptr(ptr<T> const& other)
    : weak_ptr{other.store(), other.get()}
  { }

  weak_ptr&
  operator = (weak_ptr const& other) = default;

  T&
  operator * () const { return *get(); }

  T*
  operator -> () const { return get(); }

  T*
  get() const { return static_cast<T*>(generic_weak_ptr::get()); }

  ptr<T>
  lock() const { return {*store_, get()}; }
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

private:
  std::vector<page> reserve_;
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

using large_space = std::vector<std::unique_ptr<std::byte[]>>;

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

  free_store() = default;
  free_store(free_store const&) = delete;
  void operator = (free_store const&) = delete;
  ~free_store();

  template <typename T, typename... Args>
  std::enable_if_t<!T::is_dynamic_size, ptr<T>>
  make(Args&&... args) {
#ifdef GC_DEBUG
    collect_garbage();
#endif

    static_assert(sizeof(T) % sizeof(word_type) == 0);

    std::byte* storage = allocate_object(sizeof(T), T::type_index);
    object* result = new (storage) T(std::forward<Args>(args)...);
    return {*this, result};
  }

  template <typename T, typename... Args>
  std::enable_if_t<T::is_dynamic_size, ptr<T>>
  make(Args&&... args) {
#ifdef GC_DEBUG
    collect_garbage();
#endif

    std::size_t elements = T::extra_elements(args...);
    std::size_t size = detail::round_to_words(sizeof(T) + elements * sizeof(typename T::element_type));
    std::byte* storage = allocate_object(size, T::type_index);
    object* result = new (storage) T(std::forward<Args>(args)...);
    return {*this, result};
  }

  void
  notify_arc(object* from, object* to) {
    if (is_object_ptr(to)
        && object_generation(from) == generation::mature
        && object_generation(to) < generation::mature)
      generations_[object_generation(to)].incoming_arcs.emplace(from);
  }

  generic_ptr*
  root_list() { return roots_; }

  generic_weak_ptr*
  weak_root_list() { return weak_roots_; }

  void
  register_callback(std::function<void()> cb) { post_gc_callbacks_.emplace_back(std::move(cb)); }

  void
  collect_garbage(bool major = false);

  void
  disable_collection();

  void
  enable_collection();

private:
  page_allocator allocator_;
  generation_list generations_{generation{allocator_, generation::nursery_1},
                               generation{allocator_, generation::nursery_2},
                               generation{allocator_, generation::mature}};

  // Two doubly-linked lists with head.
  generic_ptr*      roots_ = &root_head_;
  generic_ptr       root_head_;
  generic_weak_ptr* weak_roots_ = &weak_head_;
  generic_weak_ptr  weak_head_;

  std::vector<std::function<void()>> post_gc_callbacks_;

  unsigned disable_level_ = 0;
  std::optional<word_type> requested_collection_level_;

  // Allocate storage of the given payload size for an object of the given
  // type. size does not include the size of the header. The object is not
  // constructed in the storage.
  std::byte*
  allocate_object(std::size_t size, word_type type);

  void
  update_roots();
};

inline generic_ptr*
generic_ptr::root_list(free_store& fs) {
  return fs.root_list();
}

inline generic_weak_ptr*
generic_weak_ptr::root_list(free_store& fs) {
  return fs.weak_root_list();
}

class disable_collection {
public:
  explicit
  disable_collection(free_store& fs)
    : fs_{fs}
  {
    fs_.disable_collection();
  }

  ~disable_collection() {
    fs_.enable_collection();
  }

  disable_collection(disable_collection const&) = delete;
  void operator = (disable_collection const&) = delete;

private:
  free_store& fs_;
};

} // namespace insider

#endif
