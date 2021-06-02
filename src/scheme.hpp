#ifndef INSIDER_SCHEME_HPP
#define INSIDER_SCHEME_HPP

#include "bytecode.hpp"
#include "expression.hpp"
#include "free_store.hpp"
#include "numeric.hpp"
#include "object_span.hpp"
#include "vm.hpp"

#include <fmt/format.h>

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

#ifdef INSIDER_VM_PROFILER
#include <chrono>
#endif

namespace insider {

std::size_t
hash(ptr<> x);

struct generic_ptr_hash {
  std::size_t
  operator () (generic_tracked_ptr const& p) const { return hash(p.get()); }
};

bool
eqv(context&, ptr<> x, ptr<> y);

bool
equal(context&, ptr<>, ptr<>);

class error : public std::runtime_error {
public:
  // Format an error message using fmtlib and append the action stack to it.
  template <typename... Args>
  error(std::string_view fmt, Args&&... args)
    : std::runtime_error{fmt::format(fmt, std::forward<Args>(args)...)}
  { }
};

std::string
format_error(context& ctx, std::runtime_error const&);

template <typename Expected>
error
make_type_error(ptr<> actual) {
  throw error{"Invalid type: expected {}, got {}", type_name<Expected>(), object_type_name(actual)};
}

namespace detail {
  template <typename T>
  struct expect_helper {
    static ptr<T>
    expect(ptr<> x, std::string_view message) {
      if (is<T>(x))
        return ptr_cast<T>(x);
      else
        throw !message.empty() ? error{message} : make_type_error<T>(x);
    }

    static tracked_ptr<T>
    expect(generic_tracked_ptr const& x, std::string_view message) {
      return {x.store(), expect(x.get(), message)};
    }
  };

  template <>
  struct expect_helper<integer> {
    static integer
    expect(ptr<> x, std::string_view message) {
      if (is<integer>(x))
        return ptr_to_integer(x);
      else
        throw !message.empty() ? error{message} : make_type_error<integer>(x);
    }

    static integer
    expect(generic_tracked_ptr const& x, std::string_view message) {
      return expect(x.get(), message);
    }
  };
}

// Expect an object to be of given type and return the apropriate typed pointer
// to the object. Throws type_error if the object isn't of the required type.
template <typename T>
auto
expect(ptr<> x) {
  return detail::expect_helper<T>::expect(x, {});
}

template <typename T>
auto
expect(generic_tracked_ptr const& x) {
  return detail::expect_helper<T>::expect(x, {});
}

// Same as expect, but throws a runtime_error with the given message if the
// actual type isn't the expected one.
template <typename T>
auto
expect(ptr<> x, std::string_view message) {
  return detail::expect_helper<T>::expect(x, message);
}

template <typename T>
auto
expect(generic_tracked_ptr const& x, std::string_view message) {
  return detail::expect_helper<T>::expect(x, message);
}

namespace detail {
  template <typename T>
  struct assume_helper {
    static ptr<T>
    assume(ptr<> x) {
      assert(is<T>(x));
      return ptr_cast<T>(x);
    }

    static tracked_ptr<T>
    assume(generic_tracked_ptr const& x) {
      assert(is<T>(x));
      return {x.store(), static_cast<T*>(x.get())};
    }
  };

  template <>
  struct assume_helper<integer> {
    static integer
    assume(ptr<> x) {
      assert(is<integer>(x));
      return ptr_to_integer(x);
    }

    static integer
    assume(generic_tracked_ptr const& x) {
      assert(is<integer>(x));
      return ptr_to_integer(x.get());
    }
  };
}

// Assert that an object is of a given type and return the appropriate typed
// pointer. It is undefined behaviour if the actual type doesn't match the
// specified type.
template <typename T>
auto
assume(ptr<> x) {
  return detail::assume_helper<T>::assume(x);
}

template <typename T>
auto
assume(generic_tracked_ptr const& x) {
  return detail::assume_helper<T>::assume(x);
}

namespace detail {
  template <typename T>
  struct match_helper {
    static ptr<T>
    match(ptr<> x) {
      if (is<T>(x))
        return ptr_cast<T>(x);
      else
        return {};
    }

    static tracked_ptr<T>
    match(generic_tracked_ptr const& x) {
      return {x.store(), match(x.get())};
    }
  };

  template <>
  struct match_helper<integer> {
    static std::optional<integer>
    match(ptr<> x) {
      if (is<integer>(x))
        return ptr_to_integer(x);
      else
        return std::nullopt;
    }

    static std::optional<integer>
    match(generic_tracked_ptr const& x) {
      return match(x.get());
    }
  };
}

// If an object is of the given type, return the typed pointer to it; otherwise,
// return null.
template <typename T>
auto
match(ptr<> x) {
  return detail::match_helper<T>::match(x);
}

template <typename T>
auto
match(generic_tracked_ptr const& x) {
  return detail::match_helper<T>::match(x);
}

class eqv_compare {
public:
  explicit
  eqv_compare(context& ctx) : ctx_{ctx} { }

  bool
  operator () (generic_tracked_ptr const& x, generic_tracked_ptr const& y) const {
    return eqv(ctx_, x.get(), y.get());
  }

private:
  context& ctx_;
};

template <typename Value>
using eqv_unordered_map = std::unordered_map<generic_tracked_ptr, Value, generic_ptr_hash, eqv_compare>;

// The empty list. There should only be exactly one instance of this type per
// evaluation context.
struct null_type : leaf_object<null_type> {
  static constexpr char const* scheme_name = "insider::null_type";

  std::size_t
  hash() const { return 0; }
};

// The empty value. Like null_type, there should only be exactly one instance
// per evaluation context. This is used when no other meaningful value can be
// had -- such as the result of evaluating (if #f anything).
struct void_type : leaf_object<void_type> {
  static constexpr char const* scheme_name = "insider::void_type";

  std::size_t
  hash() const { return 0; }
};

// Dummy value used to represent core forms.
struct core_form_type : leaf_object<core_form_type> {
  static constexpr char const* scheme_name = "insider::core_form_type";

  std::string name;

  explicit
  core_form_type(std::string n) : name{std::move(n)} { }

  std::size_t
  hash() const { return 0; }
};

// Dummy value used for implementing tail-calls from native procedures.
struct tail_call_tag_type : leaf_object<tail_call_tag_type> {
  static constexpr char const* scheme_name = "insider::tail_call_tag_type";

  std::size_t
  hash() const { return 0; }
};

class boolean;
class context;
class scope;
class integer;
class module;
class port;
class procedure;
class symbol;
class transformer;

bool
is_identifier(ptr<>);

std::string
identifier_name(ptr<syntax> x);

using scope_set = std::vector<ptr<scope>>;

bool // Was the scope actually added?
add_scope(scope_set&, ptr<scope>);

void
remove_scope(scope_set&, ptr<scope>);

bool // Was the scope added?
flip_scope(scope_set&, ptr<scope>);

bool
scope_sets_subseteq(scope_set const& lhs, scope_set const& rhs);

bool
scope_sets_equal(scope_set const& lhs, scope_set const& rhs);

class scope : public composite_object<scope> {
public:
  static constexpr char const* scheme_name = "insider::scope";

  using value_type = std::variant<std::shared_ptr<variable>, ptr<transformer>>;
  using binding = std::tuple<ptr<syntax>, value_type>;

  explicit
  scope(std::string desc, bool use_site = false)
    : description_{std::move(desc)}
    , use_site_{use_site}
  { }

  void
  add(free_store& store, ptr<syntax> identifier, std::shared_ptr<variable>);

  void
  add(free_store& store, ptr<syntax> identifier, ptr<transformer>);

  void
  add(free_store& store, ptr<syntax> identifier, value_type const&);

  std::vector<binding>
  find_candidates(ptr<symbol> name, scope_set const& scopess) const;

  std::vector<std::string>
  bound_names() const;

  std::string const&
  description() const { return description_; }

  bool
  is_use_site() const { return use_site_; }

  auto
  begin() const { return bindings_.begin(); }

  auto
  end() const { return bindings_.end(); }

  void
  visit_members(member_visitor const& f);

  std::size_t
  hash() const;

private:
  std::vector<binding> bindings_;
  std::string          description_;
  bool                 use_site_;

  bool
  is_redefinition(ptr<syntax>, value_type const& intended_value) const;
};

std::optional<scope::value_type>
lookup(ptr<symbol> id, scope_set const& envs);

std::optional<scope::value_type>
lookup(ptr<syntax> id);

// A module is a map from symbols to top-level variable indices. It also
// contains a top-level procedure which contains the code to be run when the
// module is loaded.
class module {
public:
  using binding_type = insider::scope::value_type;

  explicit
  module(context&);

  std::optional<binding_type>
  find(ptr<symbol>) const;

  void
  import_(context&, ptr<symbol>, binding_type);

  void
  export_(ptr<symbol> name);

  std::unordered_set<std::string> const&
  exports() const { return exports_; }

  ptr<procedure>
  top_level_procedure() const { return proc_.get(); }

  void
  set_top_level_procedure(tracked_ptr<procedure> const& p) { proc_ = p; }

  ptr<insider::scope>
  scope() const { return env_.get(); }

  std::vector<std::string>
  top_level_names() const { return env_->bound_names(); }

  bool
  active() const { return active_; }

  void
  mark_active() { active_ = true; }

private:
  tracked_ptr<insider::scope>     env_;
  std::unordered_set<std::string> exports_; // Bindings available for export to other modules.
  tracked_ptr<procedure>          proc_;
  bool                            active_ = false;
};

// Turn a protomodule into a module. First instantiate all uninstantiated
// dependencies of the protomodule, then compile its body.
std::unique_ptr<module>
instantiate(context&, protomodule const&);

// Import all exports from one module to another.
void
import_all_exported(context&, module& to, module& from);

// Import all top-level bindings (whether exported or not) from one module to another.
void
import_all_top_level(context&, module& to, module& from);

// Given a protomodule, go through all of its import declarations and perform
// them in the given module.
void
perform_imports(context&, module& to, protomodule const& import_declarations);

operand
define_top_level(context&, std::string const& name, module&, bool export_, ptr<> object);

// Recursively activate all dependencies of the given module, execute the
// module's body and return the result of the last expression in its body.
//
// Causes garbage collection.
generic_tracked_ptr
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
  virtual std::optional<std::vector<tracked_ptr<syntax>>>
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

  std::optional<std::vector<tracked_ptr<syntax>>>
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
  less_or_equal,
  greater_than,
  greater_or_equal,
  vector_set,
  vector_ref
};

// Evaluation context.
class context {
public:
  struct constants {
    tracked_ptr<insider::null_type> null;
    tracked_ptr<insider::void_type> void_;
    tracked_ptr<boolean>        t, f;     // #t and #f.
    tracked_ptr<tail_call_tag_type> tail_call_tag;
    tracked_ptr<core_form_type>
      let, letrec_star, set, lambda, if_, box, unbox, box_set, define, define_syntax,
      begin, begin_for_syntax, quote, quasiquote, unquote, unquote_splicing, expand_quote,
      syntax, quasisyntax, unsyntax, unsyntax_splicing, syntax_trap, syntax_error,
      let_syntax, letrec_syntax;
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

  free_store                       store;
  std::unique_ptr<constants>       constants;
  statics_list                     statics;
  tracked_ptr<port>                output_port;
  module                           internal_module; // (insider internal)
  std::string                      error_backtrace; // Built from actions during stack unwinding.
  bytecode                         program;
  std::unique_ptr<execution_state> current_execution;

#ifdef INSIDER_VM_PROFILER
  std::vector<std::size_t> instruction_counts;
  std::vector<std::chrono::high_resolution_clock::duration> instruction_times;
#endif

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

  operand
  intern_static(generic_tracked_ptr const&);

  ptr<>
  get_static(operand i) const {
    assert(i < statics_.size());
    return statics_[i].get();
  }

  ptr<>
  get_static_checked(operand) const;

  ptr<>
  get_top_level(operand i) const { return top_level_objects_[i].get(); }

  ptr<>
  get_top_level_checked(operand) const;

  void
  set_top_level(operand i, ptr<>);

  operand
  add_top_level(ptr<>, std::string name);

  std::string
  get_top_level_name(operand) const;

  void
  tag_top_level(operand, special_top_level_tag);

  std::optional<special_top_level_tag>
  find_tag(operand) const;

  void
  load_library_module(std::vector<tracked_ptr<syntax>> const&);

  module*
  find_module(module_name const&);

  void
  prepend_module_provider(std::unique_ptr<module_provider>);

  void
  append_module_provider(std::unique_ptr<module_provider>);

private:
  std::unordered_map<std::string, weak_ptr<symbol>> interned_symbols_;
  std::vector<generic_tracked_ptr> statics_;
  eqv_unordered_map<std::size_t> statics_cache_;
  std::vector<generic_tracked_ptr> top_level_objects_;
  std::vector<std::string> top_level_binding_names_;
  std::unordered_map<operand, special_top_level_tag> top_level_tags_;
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

template <typename T, typename... Args>
tracked_ptr<T>
make_tracked(context& ctx, Args&&... args) {
  return tracked_ptr<T>{ctx.store, make<T>(ctx, std::forward<Args>(args)...)};
}

inline generic_tracked_ptr
track(context& ctx, ptr<> o) { return {ctx.store, o}; }

template <typename T>
tracked_ptr<T>
track(context& ctx, ptr<T> o) { return {ctx.store, o}; }

// A boolean value.
class boolean : public leaf_object<boolean> {
public:
  static constexpr char const* scheme_name = "insider::boolean";

  explicit
  boolean(bool value) : value_{value} { }

  bool
  value() const { return value_; }

  std::size_t
  hash() const { return value_; }

private:
  bool value_;
};

// Character. TODO: Support Unicode.
class character : public leaf_object<character> {
public:
  static constexpr char const* scheme_name = "insider::character";

  explicit
  character(char c) : value_{c} { }

  char
  value() const { return value_; }

  std::size_t
  hash() const { return value_; }

private:
  char value_;
};

// Fixed-length string. TODO: Support Unicode.
class string : public dynamic_size_object<string, char> {
public:
  static constexpr char const* scheme_name = "insider::string";

  static std::size_t
  extra_elements(std::size_t size) { return size; }

  explicit
  string(std::size_t size) : size_{size} { }

  string(string&& other);

  void
  set(std::size_t i, char c);

  std::string
  value() const;

  std::size_t
  size() const { return size_; }

  void
  visit_members(member_visitor const&) { }

  std::size_t
  hash() const;

private:
  std::size_t size_;
};

ptr<string>
make_string(context&, std::string_view value);

// I/O port or a string port. Can be read or write, binary or text.
class port : public leaf_object<port> {
public:
  static constexpr char const* scheme_name = "insider::port";

  port(FILE*, std::string name, bool input, bool output, bool should_close = true);
  port(std::string value, bool input, bool output);
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

  std::string const&
  name() const { return name_; }

  std::size_t
  hash() const;

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
  std::string name_;

  void
  destroy();
};

// A cons pair containing two other Scheme values, car and cdr.
class pair : public composite_object<pair> {
public:
  static constexpr char const* scheme_name = "insider::pair";

  pair(ptr<> car, ptr<> cdr)
    : car_{car}
    , cdr_{cdr}
  { }

  ptr<>
  car() const { return car_; }
  ptr<>
  cdr() const { return cdr_; }

  void
  set_car(free_store& store, ptr<> p) { car_ = p; store.notify_arc(this, p); }
  void
  set_cdr(free_store& store, ptr<> p) { cdr_ = p; store.notify_arc(this, p); }

  void
  visit_members(member_visitor const& f) { f(car_); f(cdr_); }

  std::size_t
  hash() const { return 3 * insider::hash(car_) ^ insider::hash(cdr_); }

private:
  ptr<> car_;
  ptr<> cdr_;
};

inline ptr<pair>
cons(context& ctx, ptr<> car, ptr<> cdr) {
  return make<pair>(ctx, car, cdr);
}

// Is the given object a list? A list is either the null value or a pair whose
// cdr is a list.
bool
is_list(ptr<>);

std::size_t
list_length(ptr<>);

inline ptr<>
car(ptr<pair> x) { return x->car(); }

inline generic_tracked_ptr
car(tracked_ptr<pair> const& x) { return {x.store(), car(x.get())}; }

inline ptr<>
cdr(ptr<pair> x) { return x->cdr(); }

inline generic_tracked_ptr
cdr(tracked_ptr<pair> const& x) { return {x.store(), cdr(x.get())}; }

inline void
set_car(tracked_ptr<pair> const& p, ptr<> x) { p->set_car(p.store(), x); }

inline void
set_cdr(tracked_ptr<pair> const& p, ptr<> x) { p->set_cdr(p.store(), x); }

ptr<>
cadr(ptr<pair>);

ptr<>
caddr(ptr<pair>);

ptr<>
cadddr(ptr<pair>);

ptr<>
cddr(ptr<pair>);

ptr<>
cdddr(ptr<pair>);

// Make a list out of given objects.
template <typename... Ts>
ptr<>
make_list(context& ctx, Ts... ts) {
  constexpr std::size_t n = sizeof...(Ts);
  std::array<ptr<>, n> elements{std::move(ts)...};

  ptr<> result = ctx.constants->null.get();
  for (std::size_t i = n; i > 0; --i)
    result = make<pair>(ctx, elements[i - 1], result);

  return result;
}

template <typename Container, typename Converter>
ptr<>
make_list_from_vector(context& ctx, Container const& values, Converter const& convert) {
  ptr<> head = ctx.constants->null.get();

  for (auto elem = values.rbegin(); elem != values.rend(); ++elem)
    head = cons(ctx, convert(*elem), head);

  return head;
}

inline ptr<>
make_list_from_vector(context& ctx, std::vector<ptr<>> const& values) {
  return make_list_from_vector(ctx, values, [] (ptr<> x) { return x; });
}

// Concatenate a number of lists. If there are 0 lists, return the empty
// list. If there is 1 list, return it. Otherwise, return a new list whose
// elements are the elements of the given lists. The last argument doesn't have
// to be a list -- if it isn't, the result is an improper list.
ptr<>
append(context&, object_span);

// An array of a fixed, dynamic size. Elements are allocated as a part of this
// object, which requires cooperation from the allocator. From the C++ point of
// view, there is an array of ptr<> allocated right after the vector object.
class vector : public dynamic_size_object<vector, ptr<>> {
public:
  static constexpr char const* scheme_name = "insider::vector";

  static std::size_t
  extra_elements(context&, std::size_t size) { return size; }

  vector(context&, std::size_t);

  vector(vector&&);

  void
  visit_members(member_visitor const&);

  ptr<>
  ref(std::size_t) const;

  void
  set(free_store&, std::size_t, ptr<>);

  std::size_t
  size() const { return size_; }

  std::size_t
  hash() const;

private:
  std::size_t size_;
};

inline void
vector_set(tracked_ptr<vector> const& v, std::size_t i, ptr<> value) { v->set(v.store(), i, value); }

ptr<vector>
make_vector(context&, std::vector<ptr<>> const&);

template <typename Container, typename Converter>
ptr<>
make_vector(context& ctx, Container const& values, Converter const& convert) {
  auto result = make<vector>(ctx, ctx, values.size());

  for (std::size_t i = 0; i < values.size(); ++i)
    result->set(ctx.store, i, convert(values[i]));

  return result;
}

ptr<vector>
list_to_vector(context&, ptr<> lst);

std::vector<ptr<>>
list_to_std_vector(ptr<>);

ptr<>
vector_to_list(context&, ptr<vector>);

ptr<vector>
vector_append(context&, object_span vs);

// An immutable string, used for identifying Scheme objects.
class symbol : public leaf_object<symbol> {
public:
  static constexpr char const* scheme_name = "insider::symbol";

  explicit
  symbol(std::string value) : value_{std::move(value)} { }

  std::string
  value() const { return value_; }

  std::size_t
  hash() const { return std::hash<std::string>{}(value_); }

private:
  std::string value_;
};

// Mutable container for a single element. Essentially a pointer.
class box : public composite_object<box> {
public:
  static constexpr char const* scheme_name = "insider::box";

  explicit
  box(ptr<>);

  ptr<>
  get() const { return value_; }

  void
  set(free_store& store, ptr<> value) { value_ = value; store.notify_arc(this, value); }

  void
  visit_members(member_visitor const& f) { f(value_); }

  std::size_t
  hash() const { return insider::hash(value_); }

private:
  ptr<> value_;
};

inline void
box_set(tracked_ptr<box> const& b, ptr<> value) { b->set(b.store(), value); }

// Callable bytecode container. Contains all the information necessary to create
// a call frame inside the VM.
class procedure : public leaf_object<procedure> {
public:
  static constexpr char const* scheme_name = "insider::procedure";

  integer::value_type        entry_pc;
  std::size_t                bytecode_size;
  unsigned                   locals_size;
  unsigned                   min_args;
  bool                       has_rest;
  std::optional<std::string> name;

  procedure(integer::value_type entry_pc, std::size_t bytecode_size, unsigned locals_size,
            unsigned min_args, bool has_rest = false, std::optional<std::string> name = {});

  std::size_t
  hash() const;
};

ptr<procedure>
make_procedure(context& ctx, bytecode const& bc, unsigned locals_size,
               unsigned min_args = 0, bool has_rest = false, std::optional<std::string> name = {});

// A procedure plus a list of captured objects.
class closure : public dynamic_size_object<closure, ptr<>> {
public:
  static constexpr char const* scheme_name = "insider::closure";

  static std::size_t
  extra_elements(ptr<insider::procedure>, std::size_t num_captures) {
    return num_captures;
  }

  closure(ptr<insider::procedure>, std::size_t num_captures);

  closure(closure&&);

  ptr<insider::procedure>
  procedure() const { return procedure_; }

  ptr<>
  ref(std::size_t) const;

  void
  set(free_store& store, std::size_t, ptr<>);

  std::size_t
  size() const { return size_; }

  void
  visit_members(member_visitor const&);

  std::size_t
  hash() const { return insider::hash(procedure_) ^ size_; }

private:
  ptr<insider::procedure> procedure_;
  std::size_t size_;
};

inline void
closure_set(tracked_ptr<closure> const& c, std::size_t i, ptr<> v) { c->set(c.store(), i, v); }

// Like procedure, but when invoked, it calls a C++ function.
struct native_procedure : public leaf_object<native_procedure> {
  static constexpr char const* scheme_name = "insider::native_procedure";

  using target_type = std::function<ptr<>(context&, object_span)>;
  target_type target;
  char const* name;

  explicit
  native_procedure(target_type f, char const* name = "<native procedure>")
    : target{std::move(f)}
    , name{name}
  { }

  std::size_t
  hash() const {
    return std::hash<std::string_view>{}(name);
  }
};

// Captured part of the call stack.
class continuation : public composite_object<continuation> {
public:
  static constexpr char const* scheme_name = "insider::continuation";

  ptr<stack_frame> frame;

  explicit
  continuation(ptr<stack_frame> f) : frame{f} { }

  void
  visit_members(member_visitor const& f) { f(frame); }

  std::size_t
  hash() const { return frame->hash(); }
};

template <typename T>
bool
is(ptr<> x);

template <typename T>
auto
match(ptr<> x);

template <typename T>
auto
match(generic_tracked_ptr const& x);

bool
is_callable(ptr<> x);

ptr<>
expect_callable(ptr<> x);

// Wrapper for C++ values that don't contain references to any Scheme objects.
template <typename T, typename Hash = std::hash<T>>
class opaque_value : public leaf_object<opaque_value<T>> {
public:
  static constexpr char const* scheme_name = "insider::opaque_value";

  T value;

  template <typename... Args>
  explicit
  opaque_value(Args&&... args)
    : value(std::forward<Args>(args)...)
  { }

  std::size_t
  hash() const {
    return Hash{}(value);
  }
};

struct source_location {
  static source_location const unknown;

  std::string file_name;
  unsigned    line;
  unsigned    column;
};

inline source_location const source_location::unknown{"<unknown>", 0, 0};

std::string
format_location(source_location const&);

class input_stream {
public:
  explicit
  input_stream(insider::ptr<port>);

  std::optional<char>
  peek_char();

  std::optional<char>
  read_char();

  void
  put_back(char);

  std::optional<char>
  advance_and_peek_char();

  source_location
  current_location() const;

private:
  insider::ptr<port> port_;
  unsigned           line_   = 1;
  unsigned           column_ = 1;
};

// Part of the programs' source code. It is an S-expression together with
// information about the source code location.
class syntax : public composite_object<syntax> {
public:
  static constexpr char const* scheme_name = "insider::syntax";

  syntax(ptr<> expr, source_location loc)
    : expression_{expr}
    , location_{std::move(loc)}
  {
    assert(expr);
  }

  syntax(ptr<> expr, scope_set envs)
    : expression_{expr}
    , scopes_{std::move(envs)}
  { }

  syntax(ptr<> expr, source_location loc, scope_set scopes)
    : expression_{expr}
    , location_{std::move(loc)}
    , scopes_{std::move(scopes)}
  { }

  ptr<>
  expression() const { return expression_; }

  source_location const&
  location() const { return location_; }

  scope_set const&
  scopes() const { return scopes_; }

  void
  add_scope(free_store&, ptr<scope>);

  void
  remove_scope(ptr<scope>);

  void
  flip_scope(free_store&, ptr<scope>);

  void
  visit_members(member_visitor const&);

  std::size_t
  hash() const { return insider::hash(expression_) ^ std::hash<std::string>{}(format_location(location_)); }

private:
  ptr<>           expression_;
  source_location location_;
  scope_set       scopes_;
};

template <typename T>
bool
syntax_is(ptr<syntax> stx) {
  return is<T>(stx->expression());
}

template <typename T>
auto
syntax_match(ptr<syntax> stx) {
  return match<T>(stx->expression());
}

template <typename T>
auto
syntax_expect(ptr<syntax> stx) {
  return expect<T>(stx->expression());
}

template <typename T>
auto
syntax_assume(ptr<syntax> stx) {
  return assume<T>(stx->expression());
}

template <typename T>
auto
semisyntax_is(ptr<> x) {
  if (auto stx = match<syntax>(x))
    return is<T>(stx->expression());
  else
    return is<T>(x);
}

template <typename T>
auto
semisyntax_match(ptr<> x) {
  if (auto stx = match<syntax>(x))
    return match<T>(stx->expression());
  else
    return match<T>(x);
}

template <typename T>
auto
semisyntax_expect(ptr<> x) {
  if (auto stx = match<syntax>(x))
    return expect<T>(stx->expression());
  else
    return expect<T>(x);
}

template <typename T>
auto
semisyntax_assume(ptr<> x) {
  if (auto stx = match<syntax>(x))
    return assume<T>(stx->expression());
  else
    return assume<T>(x);
}

ptr<>
syntax_to_datum(context&, ptr<syntax>);

ptr<syntax>
datum_to_syntax(context&, ptr<syntax>, ptr<>);

ptr<>
syntax_to_list(context&, ptr<>);

ptr<syntax>
copy_syntax(context&, ptr<syntax>);

// A procedure for transforming syntax to syntax.
class transformer : public composite_object<transformer> {
public:
  static constexpr char const* scheme_name = "insider::transformer";

  explicit
  transformer(ptr<> callable)
    : callable_{callable}
  { }

  ptr<>
  callable() const { return callable_; }

  void
  visit_members(member_visitor const&);

  std::size_t
  hash() const { return insider::hash(callable_); }

private:
  insider::ptr<> callable_;
};

namespace detail {
  template <typename T, typename SimilarTo>
  struct pointer_like;

  template <typename T>
  struct pointer_like<T, ptr<>> {
    using type = ptr<T>;
  };

  template <typename T>
  struct pointer_like<T, generic_tracked_ptr> {
    using type = tracked_ptr<T>;
  };

  template <typename T, typename SimilarTo>
  using pointer_like_t = typename pointer_like<T, SimilarTo>::type;
}

// Iterator over Scheme lists. Will throw an exception if the list turns out to
// be improper (dotted list).
template <typename Pointer>
class list_iterator {
public:
  using difference_type = std::ptrdiff_t;
  using value_type = Pointer;
  using pointer = value_type;
  using reference = value_type;
  using iterator_category = std::forward_iterator_tag;

  list_iterator() = default;

  explicit
  list_iterator(Pointer x) {
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
    Pointer next = cdr(current_);
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
  detail::pointer_like_t<pair, Pointer> current_{};
};

// Helper to allow range-based for iteration over a Scheme list.
template <typename Pointer>
class in_list {
public:
  explicit
  in_list(Pointer lst) : head_{lst} { }

  list_iterator<Pointer>
  begin() const { return list_iterator{head_}; }

  list_iterator<Pointer>
  end() const { return {}; }

private:
  Pointer head_;
};

} // namespace insider

#endif
