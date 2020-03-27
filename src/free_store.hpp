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

namespace scm {

class generic_ptr;

// Base for any garbage-collectable Scheme object.
class object {
public:
  static constexpr bool is_dynamic_size = false;

  virtual
  ~object() = default;

  virtual void
  for_each_subobject(std::function<void(object*)> const&) { }

  virtual std::size_t
  hash() const { return std::hash<std::uintptr_t>{}(reinterpret_cast<std::uintptr_t>(this)); }

  virtual bool
  eqv(generic_ptr const& other) const;

  bool mark;
};

// Object that has a fixed number of Scheme subobjects.
template <std::size_t N>
class compound_object : public object {
public:
  void
  for_each_subobject(std::function<void(object*)> const& f) override {
    for (object* o : subobjects_)
      f(o);
  }

protected:
  std::array<object*, N> subobjects_{};

  compound_object() = default;
  compound_object(std::array<object*, N> subobjects)
    : subobjects_(subobjects)
  { }
};

// Helper for Scheme objects with extra dynamic-sized storage allocated after them.
template <typename Derived, typename T>
class alignas(T) alignas(object) dynamic_size_object : public object {
public:
  static constexpr bool is_dynamic_size = true;

protected:
  T*
  dynamic_storage() { return reinterpret_cast<T*>(static_cast<Derived*>(this) + 1); }

  T const*
  dynamic_storage() const { return reinterpret_cast<T const*>(static_cast<Derived const*>(this) + 1); }
};

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

    auto storage = std::make_unique<std::byte[]>(sizeof(T));
    object* result = new (storage.get()) T(std::forward<Args>(args)...);
    return {*this, add(std::move(storage), result)};
  }

  template <typename T, typename... Args>
  std::enable_if_t<T::is_dynamic_size, ptr<T>>
  make(Args&&... args) {
#ifndef NDEBUG
    collect_garbage();
#endif

    auto storage = std::make_unique<std::byte[]>(sizeof(T) + T::extra_storage_size(args...));
    object* result = new (storage.get()) T(std::forward<Args>(args)...);
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
  bool current_mark_ = false;

  object*
  add(std::unique_ptr<std::byte[]> storage, object* result) {
    try {
      result->mark = current_mark_;
      objects_.push_back(result);
    } catch (...) {
      result->~object();
      throw;
    }

    storage.release();
    return result;
  }
};

} // namespace scm

#endif
