#ifndef SCHEME_SCHEME_HPP
#define SCHEME_SCHEME_HPP

#include "bytecode.hpp"
#include "free_store.hpp"
#include "syntax.hpp"

#include <array>
#include <cassert>
#include <cstddef>
#include <filesystem>
#include <functional>
#include <iterator>
#include <map>
#include <memory>
#include <optional>
#include <stdexcept>
#include <type_traits>
#include <unordered_map>
#include <unordered_set>
#include <variant>
#include <vector>

namespace insider {

std::size_t
hash(generic_ptr const& x);

struct generic_ptr_hash {
  std::size_t
  operator () (generic_ptr const& p) const { return hash(p); }
};

bool
eqv(generic_ptr const& x, generic_ptr const& y);

bool
equal(generic_ptr const&, generic_ptr const&);

struct eqv_compare {
  bool
  operator () (generic_ptr const& x, generic_ptr const& y) const { return eqv(x, y); }
};

template <typename Value>
using eqv_unordered_map = std::unordered_map<generic_ptr, Value, generic_ptr_hash, eqv_compare>;

// The empty list. There should only be exactly one instance of this type per
// evaluation context.
struct null_type : leaf_object<null_type> { };

// The empty value. Like null_type, there should only be exactly one instance
// per evaluation context. This is used when no other meaningful value can be
// had -- such as the result of evaluating (if #f anything).
struct void_type : leaf_object<void_type> { };

// Dummy value used to represent core forms.
struct core_form_type : leaf_object<core_form_type> { };

class boolean;
class context;
class module;
class port;
class procedure;
class symbol;
class transformer;

class environment : public composite_object<environment> {
public:
  explicit
  environment(ptr<environment> const& parent)
    : parent_{parent.get()}
  { }

  void
  add(std::string const& name, std::shared_ptr<variable>);

  void
  add_transformer(std::string const& name, ptr<transformer> const&);

  ptr<environment>
  parent(free_store& fs) const { return {fs, parent_}; }

  std::shared_ptr<variable>
  lookup(std::string const&) const;

  ptr<transformer>
  lookup_transformer(free_store&, std::string const& name) const;

  bool
  has(std::string const& name) const { return bindings_.count(name); }

  void
  trace(tracing_context& tc);

private:
  using value_type = std::variant<std::shared_ptr<variable>, transformer*>;

  environment* parent_;
  std::unordered_map<std::string, value_type> bindings_;
};

inline ptr<environment>
environment_parent(ptr<environment> const& e) { return e->parent(e.store()); }

inline ptr<transformer>
environment_lookup_transformer(ptr<environment> const& e, std::string const& name) {
  return e->lookup_transformer(e.store(), name);
}

// A module is a map from symbols to top-level variable indices. It also
// contains a top-level procedure which contains the code to be run when the
// module is loaded.
class module {
public:
  using index_type = operand::representation_type;

  explicit
  module(context&);

  std::optional<index_type>
  find(std::string const&) const;

  void
  add(std::string, index_type);

  void
  export_(std::string);

  std::unordered_set<std::string> const&
  exports() const { return exports_; }

  ptr<procedure>
  top_level_procedure() const { return proc_; }

  void
  set_top_level_procedure(ptr<procedure> const& p) { proc_ = p; }

  ptr<insider::environment>
  environment() const { return env_; }

  bool
  active() const { return active_; }

  void
  mark_active() { active_ = true; }

private:
  ptr<insider::environment>       env_;
  std::unordered_set<std::string> exports_; // Bindings available for export to other modules.
  ptr<procedure>                  proc_;
  bool                            active_ = false;
};

void
import_all(context&, module& to, module& from);

// Given a protomodule, go through all of its import declarations and perform
// them in the given module.
void
perform_imports(context&, module& to, protomodule const& import_declarations);

void
define_top_level(context&, module&, std::string const& name, generic_ptr const& object,
                 bool export_ = false);

// Recursively activate all dependencies of the given module, execute the
// module's body and return the result of the last expression in its body.
generic_ptr
execute(context&, module&);

// Interface for module providers. A module provider is used when a library is
// requested that isn't currently known in the given context. The registered
// providers are then tried in order until one of them successfully provides the
// library.
class module_provider {
public:
  virtual
  ~module_provider() = default;

  // Try to provide the module with the given name. This function must only
  // return either nullopt or a library with the specified name.
  virtual std::optional<std::vector<generic_ptr>>
  find_module(context&, module_name const&) = 0;
};

// Module provider that looks for files within a given directory and its
// subdirectories for libraries. Library (foo bar baz) must be located in a file
// called foo/bar/baz.{sld,scm} relative to the directory given to this
// provider.
class filesystem_module_provider : public module_provider {
public:
  explicit
  filesystem_module_provider(std::filesystem::path root) : root_{std::move(root)} { }

  std::optional<std::vector<generic_ptr>>
  find_module(context&, module_name const&) override;

private:
  std::filesystem::path root_;
};

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
    ptr<insider::null_type> null;
    ptr<insider::void_type> void_;
    ptr<boolean>        t, f;     // #t and #f.
    ptr<core_form_type> let, set, lambda, if_, box, unbox, box_set, define, define_syntax, begin,
                        quote, quasiquote, unquote, unquote_splicing, expand_quote;
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
  ptr<port>                  output_port;
  module                     internal_module; // (insider internal)
  std::unordered_map<std::string, std::string> type_names;

  context();
  ~context();
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

  void
  load_library_module(std::vector<generic_ptr> const&);

  module*
  find_module(module_name const&);

  void
  prepend_module_provider(std::unique_ptr<module_provider>);

  void
  append_module_provider(std::unique_ptr<module_provider>);

private:
  std::unordered_map<std::string, weak_ptr<symbol>> interned_symbols_;
  std::vector<generic_ptr> statics_;
  eqv_unordered_map<std::size_t> statics_cache_;
  std::vector<generic_ptr> top_level_objects_;
  std::unordered_map<operand::representation_type, special_top_level_tag> top_level_tags_;
  std::map<module_name, protomodule> protomodules_;
  std::map<module_name, std::unique_ptr<module>> modules_;
  std::vector<std::unique_ptr<module_provider>> module_providers_;
};

// Create an instance of an object using the context's free store.
template <typename T, typename... Args>
ptr<T>
make(context& ctx, Args&&... args) {
  return ctx.store.make<T>(std::forward<Args>(args)...);
}

// A boolean value.
class boolean : public leaf_object<boolean> {
public:
  explicit
  boolean(bool value) : value_{value} { }

  bool
  value() const { return value_; }

private:
  bool value_;
};

inline std::size_t
boolean_hash(ptr<boolean> const& b) { return b->value(); }

// Character. TODO: Support Unicode.
class character : public leaf_object<character> {
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
  extra_elements(std::size_t size) { return size; }

  explicit
  string(std::size_t size) : size_{size} { }

  void
  set(std::size_t i, char c);

  std::string
  value() const;

  std::size_t
  size() const { return size_; }

  void
  trace(tracing_context&) { }

private:
  std::size_t size_;
};

ptr<string>
make_string(context&, std::string const& value);

// I/O port or a string port. Can be read or write, binary or text.
class port : public leaf_object<port> {
public:
  port(FILE*, bool input, bool output, bool should_close = true);
  port(std::string, bool input, bool output);
  port(port&&);
  ~port();

  port&
  operator = (port const&) = delete;
  port&
  operator = (port&&) = delete;

  void
  write_string(std::string const&);
  void
  write_char(char c);

  std::optional<char>
  peek_char();
  std::optional<char>
  read_char();

  void
  put_back(char);

  std::string
  get_string() const;

  void
  rewind();

private:
  struct string_buffer {
    std::string data;
    std::size_t read_index = 0;
  };

  std::variant<FILE*, string_buffer> buffer_;
  std::vector<char> put_back_buffer_;
  bool input_ = false;
  bool output_ = false;
  bool should_close_ = false;

  void
  destroy();
};

// A cons pair containing two other Scheme values, car and cdr.
class pair : public composite_object<pair> {
public:
  pair(generic_ptr const& car, generic_ptr const& cdr)
    : car_{car.get()}
    , cdr_{cdr.get()}
  { }

  generic_ptr
  car(free_store& store) const { return {store, car_}; }
  generic_ptr
  cdr(free_store& store) const { return {store, cdr_}; }

  void
  set_car(generic_ptr const& p) { car_ = p.get(); }
  void
  set_cdr(generic_ptr const& p) { cdr_ = p.get(); }

  void
  trace(tracing_context& tc) { tc.trace(car_); tc.trace(cdr_); }

private:
  object* car_;
  object* cdr_;
};

std::size_t
pair_hash(ptr<pair> const&);

inline ptr<pair>
cons(context& ctx, generic_ptr const& car, generic_ptr const& cdr) {
  return make<pair>(ctx, car, cdr);
}

// Is the given object a list? A list is either the null value or a pair whose
// cdr is a list.
bool
is_list(generic_ptr);

std::size_t
list_length(generic_ptr);

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

  generic_ptr result = ctx.constants->null;
  for (std::size_t i = n; i > 0; --i)
    result = make<pair>(ctx, elements[i - 1], result);

  return result;
}

generic_ptr
make_list_from_vector(context&, std::vector<generic_ptr> const& values);

// Concatenate a number of lists. If there are 0 lists, return the empty
// list. If there is 1 list, return it. Otherwise, return a new list whose
// elements are the elements of the given lists. The last argument doesn't have
// to be a list -- if it isn't, the result is an improper list.
generic_ptr
append(context&, std::vector<generic_ptr> const&);

// An array of a fixed, dynamic size. Elements are allocated as a part of this
// object, which requires cooperation from the allocator. From the C++ point of
// view, there is an array of object* allocated right after the vector object.
class vector : public dynamic_size_object<vector, object*> {
public:
  static std::size_t
  extra_elements(std::size_t size) { return size; }

  explicit
  vector(std::size_t);

  void
  trace(tracing_context& tc);

  generic_ptr
  ref(free_store& store, std::size_t) const;

  void
  set(std::size_t, generic_ptr);

  std::size_t
  size() const { return size_; }

private:
  std::size_t size_;
};

std::size_t
vector_hash(ptr<vector> const&);

inline generic_ptr
vector_ref(ptr<vector> const& v, std::size_t i) { return v->ref(v.store(), i); }

ptr<vector>
make_vector(context&, std::vector<generic_ptr> const&);

ptr<vector>
list_to_vector(context&, generic_ptr const& lst);

ptr<vector>
vector_append(context&, std::vector<generic_ptr> const& vs);

// An immutable string, used for identifying Scheme objects.
class symbol : public leaf_object<symbol> {
public:
  explicit
  symbol(std::string value) : value_{std::move(value)} { }

  std::string const&
  value() const { return value_; }

private:
  std::string value_;
};

// Mutable container for a single element. Essentially a pointer.
class box : public composite_object<box> {
public:
  explicit
  box(generic_ptr const&);

  generic_ptr
  get(free_store& store) const;

  void
  set(generic_ptr const&);

  void
  trace(tracing_context& tc) { tc.trace(value_); }

private:
  object* value_;
};

inline generic_ptr
unbox(ptr<box> const& b) { return b->get(b.store()); }

// Callable bytecode container. Contains all the information necessary to create
// a call frame inside the VM.
class procedure : public leaf_object<procedure> {
public:
  insider::bytecode bytecode;
  unsigned          locals_size;
  unsigned          num_args;

  procedure(insider::bytecode bc, unsigned locals_size, unsigned num_args);
};

// A procedure plus a list of captured objects.
class closure : public dynamic_size_object<closure, object*> {
public:
  static std::size_t
  extra_elements(ptr<insider::procedure> const&, std::vector<generic_ptr> const& captures) {
    return captures.size();
  }

  closure(ptr<insider::procedure> const&, std::vector<generic_ptr> const&);

  ptr<insider::procedure>
  procedure(free_store& store) const { return {store, procedure_}; }

  generic_ptr
  ref(free_store& store, std::size_t) const;

  std::size_t
  size() const { return size_; }

  void
  trace(tracing_context&);

private:
  insider::procedure* procedure_;
  std::size_t size_;
};

inline ptr<procedure>
closure_procedure(ptr<closure> const& c) { return c->procedure(c.store()); }

inline generic_ptr
closure_ref(ptr<closure> const& c, std::size_t i) { return c->ref(c.store(), i); }

// Like procedure, but when invoked, it calls a C++ function.
class native_procedure : public leaf_object<native_procedure> {
public:
  using target_type = std::function<generic_ptr(context&, std::vector<generic_ptr> const&)>;
  target_type target;

  explicit
  native_procedure(target_type f) : target{std::move(f)} { }
};

bool
is_callable(generic_ptr const& x);

generic_ptr
expect_callable(generic_ptr const& x);

// Wrapper for C++ values that don't contain references to any Scheme objects.
template <typename T>
class opaque_value : public leaf_object<opaque_value<T>> {
public:
  T value;

  template <typename... Args>
  explicit
  opaque_value(Args&&... args)
    : value(std::forward<Args>(args)...)
  { }
};

// An expression together with an environment and a module in which to look up
// the names used in the expression. Some names may be explicitly marked as
// free, and they will be looked up in the environment in which the syntactic
// closure is being used.
class syntactic_closure : public dynamic_size_object<syntactic_closure, object*> {
public:
  static std::size_t
  extra_elements(ptr<insider::environment>,
                 generic_ptr const& expr, generic_ptr const& free);

  syntactic_closure(ptr<insider::environment>,
                    generic_ptr const& expr, generic_ptr const& free);

  generic_ptr
  expression(free_store& store) const { return {store, expression_}; }

  ptr<insider::environment>
  environment(free_store& store) const { return {store, env_}; }

  std::vector<ptr<symbol>>
  free(free_store&) const;

  std::size_t
  size() const { return free_size_; }

  void
  trace(tracing_context&);

private:
  object*               expression_;
  insider::environment* env_;
  std::size_t           free_size_;
};

inline generic_ptr
syntactic_closure_expression(ptr<syntactic_closure> const& sc) { return sc->expression(sc.store()); }

inline ptr<environment>
syntactic_closure_environment(ptr<syntactic_closure> const& sc) { return sc->environment(sc.store()); }

inline auto
syntactic_closure_free(ptr<syntactic_closure> const& sc) { return sc->free(sc.store()); }

// A procedure together with the environment it was defined in.
class transformer : public composite_object<transformer> {
public:
  transformer(ptr<insider::environment> env, generic_ptr const& callable)
    : env_{env.get()}
    , callable_{callable.get()}
  { }

  ptr<insider::environment>
  environment(free_store& store) const { return {store, env_}; }

  generic_ptr
  callable(free_store& store) const { return {store, callable_}; }

  void
  trace(tracing_context&);

private:
  insider::environment* env_;
  insider::object*      callable_;
};

inline ptr<environment>
transformer_environment(ptr<transformer> const& t) { return t->environment(t.store()); }

inline generic_ptr
transformer_callable(ptr<transformer> const& t) { return expect_callable(t->callable(t.store())); }

// Is a given object an instance of the given Scheme type?
template <typename T>
bool
is(generic_ptr const& x) {
  assert(x);
  return object_type_index(x.get()) == T::type_index;
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

// Iterator over Scheme lists. Will throw an exception if the list turns out to
// be improper (dotted list).
class list_iterator {
public:
  using difference_type = std::ptrdiff_t;
  using value_type = generic_ptr;
  using pointer = value_type;
  using reference = value_type;
  using iterator_category = std::forward_iterator_tag;

  list_iterator() = default;

  explicit
  list_iterator(generic_ptr const& x) {
    if (is<pair>(x))
      current_ = assume<pair>(x);
    else if (!is<null_type>(x))
      throw std::runtime_error{"Expected list"};
  }

  reference
  operator * () const { return car(current_); }

  pointer
  operator -> () const { return car(current_); }

  list_iterator&
  operator ++ () {
    generic_ptr next = cdr(current_);
    if (is<pair>(next))
      current_ = assume<pair>(next);
    else if (is<null_type>(next))
      current_ = {};
    else
      throw std::runtime_error{"Expected list"};

    return *this;
  }

  list_iterator
  operator ++ (int) { list_iterator result{*this}; operator ++ (); return result; }

  bool
  operator == (list_iterator const& other) const { return current_ == other.current_; }

  bool
  operator != (list_iterator const& other) { return !operator == (other); }

private:
  ptr<pair> current_{};
};

// Helper to allow range-based for iteration over a Scheme list.
class in_list {
public:
  explicit
  in_list(generic_ptr const& lst) : head_{lst} { }

  list_iterator
  begin() const { return list_iterator{head_}; }

  list_iterator
  end() const { return {}; }

private:
  generic_ptr head_;
};

} // namespace insider

#endif
