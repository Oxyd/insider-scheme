#ifndef SCHEME_FREE_STORE_HPP
#define SCHEME_FREE_STORE_HPP

#include <array>
#include <cstddef>
#include <functional>
#include <memory>
#include <type_traits>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

namespace insider {

class generic_ptr;
struct object;

using word_type = std::uint64_t;

namespace detail {
  enum class color : word_type {
    black = 0,
    white = 1
  };
}

class tracing_context {
public:
  tracing_context(std::vector<object*>& stack, detail::color current_color)
    : stack_{stack}
    , current_color_{current_color}
  { }

  void
  trace(object* o);

private:
  std::vector<object*>& stack_;
  detail::color         current_color_;
};

struct type_descriptor {
  void (*destroy)(object*);
  void (*trace)(object*, tracing_context&);

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

void
init_object_header(std::byte* storage, word_type type);

word_type
object_type_index(object*);

namespace detail {
  constexpr std::size_t
  round_to_words(std::size_t s) {
    return (s + sizeof(word_type) - 1) & -sizeof(word_type);
  }

  template <typename T>
  void
  destroy(object* o) { static_cast<T*>(o)->~T(); }
}

// Object with no Scheme subobjects.
template <typename Derived>
struct leaf_object : object {
  static constexpr type_descriptor descriptor{
    detail::destroy<Derived>,
    [] (object*, tracing_context&) { },
    true,
    detail::round_to_words(sizeof(Derived)),
    nullptr
  };
  static word_type const type_index;
};

template <typename Derived>
word_type const leaf_object<Derived>::type_index = new_type(leaf_object::descriptor);

// Object with a constant number of Scheme subobjects.
template <typename Derived>
struct composite_object : object {
  static constexpr type_descriptor descriptor{
    detail::destroy<Derived>,
    [] (object* self, tracing_context& tc) { static_cast<Derived*>(self)->trace(tc); },
    true,
    detail::round_to_words(sizeof(Derived)),
    nullptr
  };
  static word_type const type_index;
};

template <typename Derived>
word_type const composite_object<Derived>::type_index = new_type(composite_object::descriptor);

// Object whose size is determined at instantiation time.
template <typename Derived, typename T>
struct alignas(T) alignas(object) dynamic_size_object : object {
  using element_type = T;
  static constexpr bool is_dynamic_size = true;
  static constexpr type_descriptor descriptor{
    detail::destroy<Derived>,
    [] (object* self, tracing_context& tc) { static_cast<Derived*>(self)->trace(tc); },
    false,
    0,
    [] (object* o) { return sizeof(Derived) + detail::round_to_words(static_cast<Derived*>(o)->size() * sizeof(T)); }
  };
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
word_type const dynamic_size_object<Derived, T>::type_index = new_type(dynamic_size_object::descriptor);

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

// Garbage-collected storage for Scheme objects.
class free_store {
public:
  free_store() = default;
  free_store(free_store const&) = delete;
  void operator = (free_store const&) = delete;
  ~free_store();

  template <typename T, typename... Args>
  std::enable_if_t<!T::is_dynamic_size, ptr<T>>
  make(Args&&... args) {
#ifndef NDEBUG
    collect_garbage();
#endif

    static_assert(sizeof(T) % sizeof(word_type) == 0);

    auto storage = std::make_unique<std::byte[]>(sizeof(word_type) + sizeof(T));
    init_object_header(storage.get(), T::type_index);
    object* result = new (storage.get() + sizeof(word_type)) T(std::forward<Args>(args)...);
    return {*this, add(std::move(storage), result)};
  }

  template <typename T, typename... Args>
  std::enable_if_t<T::is_dynamic_size, ptr<T>>
  make(Args&&... args) {
#ifndef NDEBUG
    collect_garbage();
#endif

    std::size_t elements = T::extra_elements(args...);
    std::size_t size = detail::round_to_words(sizeof(word_type)
                                              + sizeof(T)
                                              + elements * sizeof(typename T::element_type));

    auto storage = std::make_unique<std::byte[]>(size);
    init_object_header(storage.get(), T::type_index);
    object* result = new (storage.get() + sizeof(word_type)) T(std::forward<Args>(args)...);
    return {*this, add(std::move(storage), result)};
  }

  void
  register_root(generic_ptr*);
  void
  unregister_root(generic_ptr*);

  void
  register_weak(generic_weak_ptr*, object*);
  void
  unregister_weak(generic_weak_ptr*, object*);

  void
  collect_garbage();

private:
  // We need to support dynamically-sized objects, which are objects like Scheme
  // vector that have an additional storage allocated right after the C++ object
  // (so that the storage essentially is a part of the object itself). To that
  // end, we allocate all memory as new std::byte[size] and construct the object
  // manually inside that storage. For dynamic-sized objects we require that the
  // dynamic storage be an array of trivial objects so that we don't have to
  // construct or destruct objects within that storage.
  //
  // Non-dynamically-sized objects could be constructed directly as new T, but
  // that would require storing dynamically-sized and non-dynamically sized
  // objects in different containers, so that we know whether to deallocate them
  // with delete or with explicit destructor call + delete[].
  //
  // TODO: We should use smarter allocation strategy.

  std::vector<object*> objects_;
  std::unordered_set<generic_ptr*> roots_;
  std::unordered_multimap<object*, generic_weak_ptr*> weak_ptrs_;
  detail::color current_color_ = detail::color::black;

#ifndef NDEBUG
  bool collecting_ = false;
#endif

  object*
  add(std::unique_ptr<std::byte[]> storage, object* result);
};

} // namespace insider

#endif
