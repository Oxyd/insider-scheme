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
#include <variant>
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

// The empty list. There should only be exactly one instance of this type per
// evaluation context.
struct null_type : object { };

// The empty value. Like null_type, there should only be exactly one instance
// per evaluation context. This is used when no other meaningful value can be
// had -- such as the result of evaluating (if #f anything).
struct void_type : object { };

class boolean;
class module;
class port;
class symbol;

// Some top-level values are tagged to let the compiler understand them and
// optimise them.
enum class special_top_level_tag {
  plus,
  minus,
  times,
  divide,
  arith_equal,
  less_than,
  greater_than
};

// Evaluation context.
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

  free_store   store;
  constants    constants;
  statics_list statics;
  ptr<port>    stdout;

  context();
  context(context const&) = delete;
  void
  operator = (context const&) = delete;

  // If the given string has been interned previously in the context, return the
  // pre-existing symbol. Otherwise, create a new symbol object and return
  // that. This means that two interned symbols can be compared for equality
  // using pointer comparison.
  ptr<symbol>
  intern(std::string const&);

  operand::representation_type
  intern_static(generic_ptr const&);

  generic_ptr
  get_static(operand::representation_type) const;

  generic_ptr
  get_top_level(operand::representation_type i) const { return top_level_objects_[i]; }

  void
  set_top_level(operand::representation_type i, generic_ptr const&);

  operand::representation_type
  add_top_level(generic_ptr const&);

  void
  tag_top_level(operand::representation_type, special_top_level_tag);

  std::optional<special_top_level_tag>
  find_tag(operand::representation_type) const;

private:
  std::unordered_map<std::string, weak_ptr<symbol>> interned_symbols_;
  std::vector<generic_ptr> statics_;
  eqv_unordered_map<std::size_t> statics_cache_;
  std::vector<generic_ptr> top_level_objects_;
  std::unordered_map<operand::representation_type, special_top_level_tag> top_level_tags_;
};

// Create an instance of an object using the context's free store.
template <typename T, typename... Args>
ptr<T>
make(context& ctx, Args&&... args) {
  return ctx.store.make<T>(std::forward<Args>(args)...);
}

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
add(context&, ptr<integer> const&, ptr<integer> const&);
generic_ptr
add(context&, std::vector<generic_ptr> const&);

ptr<integer>
subtract(context&, ptr<integer> const&, ptr<integer> const&);
generic_ptr
subtract(context&, std::vector<generic_ptr> const&);

ptr<integer>
multiply(context&, ptr<integer> const&, ptr<integer> const&);
generic_ptr
multiply(context&, std::vector<generic_ptr> const&);

ptr<integer>
divide(context&, ptr<integer> const&, ptr<integer> const&);
generic_ptr
divide(context&, std::vector<generic_ptr> const&);

ptr<boolean>
arith_equal(context&, ptr<integer> const&, ptr<integer> const&);
generic_ptr
arith_equal(context&, std::vector<generic_ptr> const&);

ptr<boolean>
less(context&, ptr<integer> const&, ptr<integer> const&);
generic_ptr
less(context&, std::vector<generic_ptr> const&);

ptr<boolean>
greater(context&, ptr<integer> const&, ptr<integer> const&);
generic_ptr
greater(context&, std::vector<generic_ptr> const&);

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

// Character. TODO: Support Unicode.
class character : public object {
public:
  explicit
  character(char c) : value_{c} { }

  char
  value() const { return value_; }

private:
  char value_;
};

// Fixed-length string. TODO: Support Unicode.
class string : public dynamic_size_object<string, char> {
public:
  static std::size_t
  extra_storage_size(std::size_t size) { return size; }

  explicit
  string(std::size_t size) : size_{size} { }

  void
  set(std::size_t i, char c);

  std::string
  value() const;

  std::size_t
  size() const { return size_; }

private:
  std::size_t size_;
};

ptr<string>
make_string(context&, std::string const& value);

// I/O port or a string port. Can be read or write, binary or text.
class port : public object {
public:
  port(FILE*, bool input, bool output, bool should_close = true);
  port(std::string, bool input, bool output);

  void
  write_string(std::string const&);
  void
  write_char(char c);

  std::optional<char>
  peek_char();
  std::optional<char>
  read_char();

  std::string
  get_string() const;

private:
  struct string_buffer {
    std::string data;
    std::size_t read_index = 0;
  };

  std::variant<FILE*, string_buffer> buffer_;
  bool input_ = false;
  bool output_ = false;
  bool should_close_ = false;
};

// A cons pair containing two other Scheme values, car and cdr.
class pair : public compound_object<2> {
public:
  pair(generic_ptr const& car, generic_ptr const& cdr)
    : compound_object{{car.get(), cdr.get()}}
  { }

  generic_ptr
  car(free_store& store) const { return {store, subobjects_[0]}; }
  generic_ptr
  cdr(free_store& store) const { return {store, subobjects_[1]}; }

  void
  set_car(generic_ptr const& p) { subobjects_[0] = p.get(); }
  void
  set_cdr(generic_ptr const& p) { subobjects_[1] = p.get(); }

  std::size_t
  hash() const override;
};

// Is the given object a list? A list is either the null value or a pair whose
// cdr is a list.
bool
is_list(context&, generic_ptr);

std::size_t
list_length(context&, generic_ptr);

inline generic_ptr
car(ptr<pair> const& x) { return x->car(x.store()); }

inline generic_ptr
cdr(ptr<pair> const& x) { return x->cdr(x.store()); }

generic_ptr
cadr(ptr<pair> const&);

generic_ptr
caddr(ptr<pair> const&);

generic_ptr
cadddr(ptr<pair> const&);

generic_ptr
cddr(ptr<pair> const&);

generic_ptr
cdddr(ptr<pair> const&);

// Make a list out of given objects.
template <typename... Ts>
generic_ptr
make_list(context& ctx, Ts... ts) {
  constexpr std::size_t n = sizeof...(Ts);
  std::array<generic_ptr, n> elements{std::move(ts)...};

  generic_ptr result = ctx.constants.null;
  for (std::size_t i = n; i > 0; --i)
    result = make<pair>(ctx, elements[i - 1], result);

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
  ref(free_store& store, std::size_t) const;

  void
  set(std::size_t, generic_ptr);

  std::size_t
  size() const { return size_; }

  std::size_t
  hash() const override;

private:
  std::size_t size_;
};

inline generic_ptr
vector_ref(ptr<vector> const& v, std::size_t i) { return v->ref(v.store(), i); }

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
  get(free_store& store) const;

  void
  set(generic_ptr const&);
};

inline generic_ptr
unbox(ptr<box> const& b) { return b->get(b.store()); }

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
  procedure(free_store& store) const { return {store, procedure_}; }

  generic_ptr
  ref(free_store& store, std::size_t) const;

  void
  for_each_subobject(std::function<void(object*)> const&) override;

private:
  scm::procedure* procedure_;
  std::size_t size_;
};

inline ptr<procedure>
closure_procedure(ptr<closure> const& c) { return c->procedure(c.store()); }

inline generic_ptr
closure_ref(ptr<closure> const& c, std::size_t i) { return c->ref(c.store(), i); }

// Like procedure, but when invoked, it calls a C++ function.
class native_procedure : public object {
public:
  using target_type = std::function<generic_ptr(context&, std::vector<generic_ptr> const&)>;
  target_type target;

  explicit
  native_procedure(target_type f) : target{std::move(f)} { }
};

// A module is a map from symbols to top-level variable indices. It also
// contains a top-level procedure which contains the code to be run when the
// module is loaded.
class module : public object {
public:
  using index_type = operand::representation_type;

  std::optional<index_type>
  find(std::string const&) const;

  void
  add(std::string, index_type);

  void
  export_(std::string);

  void
  import(std::string, index_type);

  std::unordered_set<std::string> const&
  exports() const { return exports_; }

  ptr<procedure>
  top_level_procedure(free_store& store) const { return {store, proc_}; }

  void
  set_top_level_procedure(ptr<procedure> const& p) { proc_ = p.get(); }

  void
  for_each_subobject(std::function<void(object*)> const&) override;

private:
  std::unordered_map<std::string, index_type> bindings_; // Bindings defined in this module.
  std::unordered_map<std::string, index_type> imports_;  // Bindings imported from other modules.
  std::unordered_set<std::string> exports_; // Bindings available for export to other modules.
  procedure* proc_;
};

void
import_all(ptr<module> const& to, ptr<module> const& from);

inline ptr<procedure>
module_top_level_procedure(ptr<module> const& m) { return m->top_level_procedure(m.store()); }

void
define_top_level(context&, ptr<module> const&, std::string const& name, generic_ptr const& object,
                 bool export_ = false);

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
    return {x.store(), static_cast<T*>(x.get())};
  else
    throw type_error{};
}

// Same as expect, but throws a runtime_error with the given message if the
// actual type isn't the expected one.
template <typename T>
ptr<T>
expect(generic_ptr const& x, std::string const& message) {
  if (is<T>(x))
    return {x.store(), static_cast<T*>(x.get())};
  else
    throw std::runtime_error{message};
}

// Assert that an object is of a given type and return the appropriate typed
// pointer. It is undefined behaviour if the actual type doesn't match the
// specified type.
template <typename T>
ptr<T>
assume(generic_ptr const& x) {
  assert(is<T>(x));
  return {x.store(), static_cast<T*>(x.get())};
}

// If an object is of the given type, return the typed pointer to it; otherwise,
// return null.
template <typename T>
ptr<T>
match(generic_ptr const& x) {
  if (is<T>(x))
    return {x.store(), static_cast<T*>(x.get())};
  else
    return {};
}

} // namespace scm

#endif
