#ifndef SCHEME_SCHEME_HPP
#define SCHEME_SCHEME_HPP

#include "bytecode.hpp"

#include <array>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <functional>
#include <memory>
#include <optional>
#include <stdexcept>
#include <type_traits>
#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace game::scm {

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
class alignas(T) dynamic_size_object : public object {
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
    explicit
    generic_ptr_base(object* value) : value_{value} { }

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

  protected:
    object* value_ = nullptr;
  };
} // namespace detail

// Untyped pointer to a Scheme object, registered with the garbage collector as a GC root.
class generic_ptr : public detail::generic_ptr_base {
public:
  generic_ptr(object* value = nullptr);
  explicit
  generic_ptr(free_store&, object* value = nullptr);
  generic_ptr(generic_ptr const& other);
  ~generic_ptr();
  generic_ptr&
  operator = (generic_ptr const&) = default;
};

struct generic_ptr_hash {
  std::size_t
  operator () (generic_ptr const& p) const { return p->hash(); }
};

inline bool
eqv(generic_ptr const& x, generic_ptr const& y) { return x->eqv(y); }

struct eqv_compare {
  bool
  operator () (generic_ptr const& x, generic_ptr const& y) const { return x->eqv(y); }
};

template <typename Value>
using eqv_unordered_map = std::unordered_map<generic_ptr, Value, generic_ptr_hash, eqv_compare>;

// Like generic_ptr, but does not keep an object alive.
class generic_weak_ptr : public detail::generic_ptr_base {
public:
  generic_weak_ptr(object* value = nullptr);
  explicit
  generic_weak_ptr(free_store&, object* value = nullptr);
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
    : weak_ptr{other.get()}
  { }

  T&
  operator * () const { return *get(); }

  T*
  operator -> () const { return get(); }

  T*
  get() const { return static_cast<T*>(generic_weak_ptr::get()); }

  ptr<T>
  lock() const { return {get()}; }
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
    return add(std::move(storage), result);
  }

  template <typename T, typename... Args>
  std::enable_if_t<T::is_dynamic_size, ptr<T>>
  make(Args&&... args) {
#ifndef NDEBUG
    collect_garbage();
#endif

    auto storage = std::make_unique<std::byte[]>(sizeof(T) + T::extra_storage_size(args...));
    object* result = new (storage.get()) T(std::forward<Args>(args)...);
    return add(std::move(storage), result);
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

// The empty list. There should only be exactly one instance of this type per
// evaluation context.
struct null_type : object { };

// The empty value. Like null_type, there should only be exactly one instance
// per evaluation context. This is used when no other meaningful value can be
// had -- such as the result of evaluating (if #f anything).
struct void_type : object { };

class boolean;
class module;
class symbol;

// Per-thread evaluation context.
class context {
public:
  struct constants {
    ptr<scm::null_type> null;
    ptr<scm::void_type> void_;
    ptr<boolean>        t, f;     // #t and #f.
    ptr<module>         internal; // (insider internal) module.
  };

  struct statics_list {
    operand
      null,
      void_,
      t, f,
      zero,
      one
      ;
  };

  free_store                 store;
  std::unique_ptr<constants> constants;
  statics_list               statics;

  ptr<symbol>
  intern(std::string const&);

  operand::representation_type
  intern_static(generic_ptr const&);

  generic_ptr
  get_static(operand::representation_type) const;

private:
  std::unordered_map<std::string, weak_ptr<symbol>> interned_symbols_;
  std::vector<generic_ptr> statics_;
  eqv_unordered_map<std::size_t> statics_cache_;

  // Only make_context is allowed to make context instances because
  // initialisation needs to be performed in two steps: First we make the object
  // itself and set the thread-local pointer to it, then we initialise
  // constants.

  friend void make_context();
  friend void destroy_context();
  context() = default;
  context(context const&) = delete;
  void
  operator = (context const&) = delete;

  void
  init();

  void
  deinit();
};

// Make a thread-local context.
void
make_context();

// Destroy the thread-local context. It is an error if no context is associated
// with this thread.
void
destroy_context();

// Get the context associated with this thread. It is an error if no context is
// associcated with this thread.
context&
thread_context();

// Access the thread-local context's null instance.
inline ptr<null_type>
null() {
  return thread_context().constants->null;
}

// Create an instance of an object using the thread-local context's free store.
template <typename T, typename... Args>
ptr<T>
make(Args&&... args) {
  return thread_context().store.make<T>(std::forward<Args>(args)...);
}

// Scope-bound context manager. Creates a new thread-local context in the
// constructor and destroys it in the destructor. It is an error if there
// already is a context when an instance of this type is constructed.
class unique_context {
public:
  unique_context()  { make_context(); }
  ~unique_context() { destroy_context(); }

  unique_context(unique_context const&) = delete;
  void operator = (unique_context const&) = delete;
};

// A signed, fixed size integer.
class integer : public object {
public:
  using storage_type = std::uint64_t;
  using value_type = std::int64_t;

  integer() = default;
  integer(std::uint64_t value) : value_{value} { }
  integer(std::int64_t value) : value_{static_cast<std::uint64_t>(value)} { }
  integer(int value) : value_{static_cast<std::uint64_t>(value)} { }

  std::int64_t
  value() const { return static_cast<std::int64_t>(value_); }

  std::size_t
  hash() const override { return static_cast<std::size_t>(value_); }

  bool
  eqv(generic_ptr const& other) const override;

private:
  storage_type value_ = 0;
};

ptr<integer>
add(ptr<integer> const&, ptr<integer> const&);
generic_ptr
add(std::vector<generic_ptr> const&);

ptr<integer>
subtract(ptr<integer> const&, ptr<integer> const&);
generic_ptr
subtract(std::vector<generic_ptr> const&);

ptr<integer>
multiply(ptr<integer> const&, ptr<integer> const&);
generic_ptr
multiply(std::vector<generic_ptr> const&);

ptr<integer>
divide(ptr<integer> const&, ptr<integer> const&);
generic_ptr
divide(std::vector<generic_ptr> const&);

ptr<boolean>
arith_equal(ptr<integer> const&, ptr<integer> const&);
generic_ptr
arith_equal(std::vector<generic_ptr> const&);

ptr<boolean>
less(ptr<integer> const&, ptr<integer> const&);
generic_ptr
less(std::vector<generic_ptr> const&);

ptr<boolean>
greater(ptr<integer> const&, ptr<integer> const&);
generic_ptr
greater(std::vector<generic_ptr> const&);

// A boolean value.
class boolean : public object {
public:
  explicit
  boolean(bool value) : value_{value} { }

  bool
  value() const { return value_; }

  std::size_t
  hash() const override { return value_; }

private:
  bool value_;
};

// A cons pair containing two other Scheme values, car and cdr.
class pair : public compound_object<2> {
public:
  pair(generic_ptr const& car, generic_ptr const& cdr)
    : compound_object{{car.get(), cdr.get()}}
  { }

  generic_ptr
  car() const { return {subobjects_[0]}; }
  generic_ptr
  cdr() const { return {subobjects_[1]}; }

  void
  set_car(generic_ptr p) { subobjects_[0] = p.get(); }
  void
  set_cdr(generic_ptr p) { subobjects_[1] = p.get(); }

  std::size_t
  hash() const override;
};

// Is the given object a list? A list is either the null value or a pair whose
// cdr is a list.
bool
is_list(generic_ptr);

std::size_t
list_length(generic_ptr);

generic_ptr
cadr(ptr<pair> const&);

generic_ptr
caddr(ptr<pair> const&);

// Make a list out of given objects.
template <typename... Ts>
generic_ptr
make_list(Ts... ts) {
  constexpr std::size_t n = sizeof...(Ts);
  std::array<generic_ptr, n> elements{std::move(ts)...};

  generic_ptr result = null();
  for (std::size_t i = n; i > 0; --i)
    result = make<pair>(elements[i - 1], result);

  return result;
}

// An array of a fixed, dynamic size. Elements are allocated as a part of this
// object, which requires cooperation from the allocator. From the C++ point of
// view, there is an array of object* allocated right after the vector object.
class vector : public dynamic_size_object<vector, object*> {
public:
  static std::size_t
  extra_storage_size(std::size_t size) { return size * sizeof(object*); }

  explicit
  vector(std::size_t);

  void
  for_each_subobject(std::function<void(object*)> const& f) override;

  generic_ptr
  ref(std::size_t) const;

  void
  set(std::size_t, generic_ptr);

  std::size_t
  size() const { return size_; }

  std::size_t
  hash() const override;

private:
  std::size_t size_;
};

// An immutable string, used for identifying Scheme objects.
class symbol : public object {
public:
  explicit
  symbol(std::string value) : value_{std::move(value)} { }

  std::string const&
  value() const { return value_; }

private:
  std::string value_;
};

// Mutable container for a single element. Essentially a pointer.
class box : public compound_object<1> {
public:
  explicit
  box(generic_ptr const&);

  generic_ptr
  get() const;

  void
  set(generic_ptr const&);
};

// If the given string has been interned previously in the thread-local context,
// return the pre-existing symbol. Otherwise, create a new symbol object and
// return that. This means that two interned symbols can be compared for
// equality using pointer comparison.
inline ptr<symbol>
intern(std::string const& s) { return thread_context().intern(s); }

// Callable bytecode container. Contains all the information necessary to create
// a call frame inside the VM.
class procedure : public object {
public:
  scm::bytecode bytecode;
  unsigned      locals_size;
  unsigned      num_args;

  procedure(scm::bytecode bc, unsigned locals_size, unsigned num_args);
};

// A procedure plus a list of captured objects.
class closure : public dynamic_size_object<closure, object*> {
public:
  static std::size_t
  extra_storage_size(ptr<scm::procedure> const&, std::vector<generic_ptr> const& captures) {
    return captures.size() * sizeof(object*);
  }

  closure(ptr<scm::procedure> const&, std::vector<generic_ptr> const&);

  ptr<scm::procedure>
  procedure() const { return procedure_; }

  generic_ptr
  ref(std::size_t) const;

  void
  for_each_subobject(std::function<void(object*)> const&) override;

private:
  scm::procedure* procedure_;
  std::size_t size_;
};

// Like procedure, but when invoked, it calls a C++ function.
class native_procedure : public object {
public:
  using target_type = std::function<generic_ptr(std::vector<generic_ptr> const&)>;
  target_type target;

  explicit
  native_procedure(target_type f) : target{std::move(f)} { }
};

// A collection of objects, some of which may be exported for other modules to
// use, and a list of bindings. Bindings assign indices to objects -- the objects
// may belong to this module or some other, imported, module.
//
// Objects are indexed, and bindings refer to them through this index. The index
// refers to the object table of the object's parent module -- not necessarily
// the current module. Bindings themselves are also indexed, and the VM refers
// to the bindings through the global scope. This double indirection is so that
// a binding may refer to an object in a different module, and so that the
// owning module can change the object it points to by reassigning the
// corresponding entry in the objects vector.
//
// Bindings also carry a tag that allows the compiler to recognise special
// Scheme built-ins (such as arithmetic procedures like + and *). Some bindings
// may even only have a tag but no object index -- this is the case with special
// forms such as if or let.
//
// In addition to all of that, a module keeps a symbol table, and a list of
// exports and imports. These are indexed by symbols and refer to a particular
// binding by its index. The symbol and import tables are used by the compiler
// to translate symbolic names into bindings. A symbol may be imported under a
// different name than it was exported under.
//
// The symbol and import tables are separate so that locally-defined names can
// shadow imported names.
class module : public object {
public:
  enum class binding_tag {
    value,       // Scheme value with no special meaning
    plus,
    minus,
    times,
    divide,
    arith_equal,
    less_than,
    greater_than,
  };

  struct binding {
    std::optional<std::size_t> index;
    binding_tag                tag;
    module*                    parent;
  };

  std::vector<object*>                     objects;
  std::vector<binding>                     bindings;
  std::unordered_map<symbol*, std::size_t> symbols; // Bindings defined in this module.
  std::unordered_map<symbol*, std::size_t> exports; // Subset of `symbols' that are exported.
  std::unordered_map<symbol*, std::size_t> imports; // Bindings defined in other modules.

  void
  for_each_subobject(std::function<void(object*)> const&) override;
};

// Import all symbols from the source module into the destination module with no
// renaming.
void
import_all(ptr<module> const& dest, ptr<module> const& source);

// Is a given object an instance of the given Scheme type?
template <typename T>
bool
is(generic_ptr const& x) {
  assert(x);
  return typeid(*x) == typeid(T);
}

struct type_error : std::runtime_error {
  // TODO: Show the actual and expected types in the error message.
  type_error() : std::runtime_error{"Invalid type"} { }
};

// Expect an object to be of given type and return the apropriate typed pointer
// to the object. Throws type_error if the object isn't of the required type.
template <typename T>
ptr<T>
expect(generic_ptr const& x) {
  if (is<T>(x))
    return {static_cast<T*>(x.get())};
  else
    throw type_error{};
}

// Assert that an object is of a given type and return the appropriate typed
// pointer. It is undefined behaviour if the actual type doesn't match the
// specified type.
template <typename T>
ptr<T>
assume(generic_ptr const& x) {
  assert(is<T>(x));
  return {static_cast<T*>(x.get())};
}

// If an object is of the given type, return the typed pointer to it; otherwise,
// return null.
template <typename T>
ptr<T>
match(generic_ptr const& x) {
  if (is<T>(x))
    return {static_cast<T*>(x.get())};
  else
    return {nullptr};
}

} // namespace game::scm

#endif
