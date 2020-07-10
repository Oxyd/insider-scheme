#ifndef SCHEME_FREE_STORE_HPP
#define SCHEME_FREE_STORE_HPP

#include <cstddef>
#include <functional>
#include <memory>
#include <string>
#include <unordered_set>
#include <utility>
#include <vector>

namespace insider {

class generic_ptr;
struct object;

using word_type = std::uint64_t;

class tracing_context {
public:
  explicit
  tracing_context(std::vector<object*>& stack)
    : stack_{stack}
  { }

  void
  trace(object* o);

private:
  std::vector<object*>& stack_;
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

bool
is_alive(object*);

object*
forwarding_address(object*);

template <typename T>
void
update_reference(T*& ref) {
  if (ref && !is_alive(ref))
    ref = static_cast<T*>(forwarding_address(ref));
}

template <typename T>
T*
update_reference_copy(T* ref) {
  if (ref && !is_alive(ref))
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
  class generic_ptr_base {
  public:
    generic_ptr_base() = default;

    generic_ptr_base(free_store& store, object* value)
      : value_{value}
      , store_{&store}
    { }

    void
    reset() { value_ = nullptr; }

    object&
    operator * () const { return *get(); }

    object*
    operator -> () const { return get(); }

    object*
    get() const { return value_; }

    free_store&
    store() const { return *store_; }

    explicit
    operator bool () const { return value_ != nullptr; }

  protected:
    friend class insider::free_store;

    object* value_     = nullptr;
    free_store* store_ = nullptr;
  };
} // namespace detail

// Untyped pointer to a Scheme object, registered with the garbage collector as a GC root.
class generic_ptr : public detail::generic_ptr_base {
public:
  generic_ptr() = default;
  generic_ptr(free_store&, object* value);
  generic_ptr(generic_ptr const& other);
  ~generic_ptr();
  generic_ptr&
  operator = (generic_ptr const&);
};


// Like generic_ptr, but does not keep an object alive.
class generic_weak_ptr : public detail::generic_ptr_base {
public:
  generic_weak_ptr() = default;
  generic_weak_ptr(free_store&, object* value);
  generic_weak_ptr(generic_weak_ptr const& other);
  ~generic_weak_ptr();

  generic_weak_ptr&
  operator = (generic_weak_ptr const& other);

  generic_weak_ptr&
  operator = (object* value);

  generic_ptr
  lock() const;
};

inline bool
operator == (detail::generic_ptr_base const& lhs, detail::generic_ptr_base const& rhs) {
  return lhs.get() == rhs.get();
}

inline bool
operator != (detail::generic_ptr_base const& lhs, detail::generic_ptr_base const& rhs) {
  return !operator == (lhs, rhs);
}

// Typed pointer to a garbage-collectable object.
template <typename T>
class ptr : public generic_ptr {
public:
  using generic_ptr::generic_ptr;

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

  T&
  operator * () const { return *get(); }

  T*
  operator -> () const { return get(); }

  T*
  get() const { return static_cast<T*>(generic_weak_ptr::get()); }

  ptr<T>
  lock() const { return {store(), get()}; }
};

struct page {
  std::unique_ptr<std::byte[]> storage;
  std::size_t used = 0;
};

struct space {
  std::vector<page> pages;
  std::size_t       current = 0;
};

// Garbage-collected storage for Scheme objects.
class free_store {
public:
  free_store();
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
  register_root(generic_ptr*);
  void
  unregister_root(generic_ptr*);

  void
  register_weak(generic_weak_ptr*);
  void
  unregister_weak(generic_weak_ptr*);

  void
  register_callback(std::function<void()> cb) { post_gc_callbacks_.emplace_back(std::move(cb)); }

  void
  collect_garbage();

private:
  space nursery_fromspace_;
  space nursery_tospace_;

  std::unordered_set<generic_ptr*> roots_;
  std::unordered_set<generic_weak_ptr*> weak_roots_;
  std::vector<std::function<void()>> post_gc_callbacks_;

  // Allocate storage of the given payload size for an object of the given
  // type. size does not include the size of the header. The object is not
  // constructed in the storage.
  std::byte*
  allocate_object(std::size_t size, word_type type);

  void
  update_roots();
};

} // namespace insider

#endif
