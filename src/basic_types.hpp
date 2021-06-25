#ifndef INSIDER_BASIC_TYPES_HPP
#define INSIDER_BASIC_TYPES_HPP

#include "bytecode.hpp"
#include "compare.hpp"
#include "context.hpp"
#include "free_store.hpp"
#include "object.hpp"

namespace insider {

// The empty list. There should only be exactly one instance of this type per
// evaluation context.
class null_type : public leaf_object<null_type> {
public:
  static constexpr char const* scheme_name = "insider::null_type";

  std::size_t
  hash() const { return 0; }
};

// The empty value. Like null_type, there should only be exactly one instance
// per evaluation context. This is used when no other meaningful value can be
// had -- such as the result of evaluating (if #f anything).
class void_type : public leaf_object<void_type> {
public:
  static constexpr char const* scheme_name = "insider::void_type";

  std::size_t
  hash() const { return 0; }
};

// Dummy value for identifying parameters.
class parameter_tag : public leaf_object<parameter_tag> {
public:
  static constexpr char const* scheme_name = "insider::parameter_tag";

  std::size_t
  hash() const { return 0; }
};

// Dummy value used to represent core forms.
class core_form_type : public leaf_object<core_form_type> {
public:
  static constexpr char const* scheme_name = "insider::core_form_type";

  std::string name;

  explicit
  core_form_type(std::string n) : name{std::move(n)} { }

  std::size_t
  hash() const { return 0; }
};

// Dummy value used for implementing tail-calls from native procedures.
class tail_call_tag_type : public leaf_object<tail_call_tag_type> {
public:
  static constexpr char const* scheme_name = "insider::tail_call_tag_type";

  std::size_t
  hash() const { return 0; }
};

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

inline tracked_ptr<>
car(tracked_ptr<pair> const& x) { return {x.store(), car(x.get())}; }

inline ptr<>
cdr(ptr<pair> x) { return x->cdr(); }

inline tracked_ptr<>
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
match(tracked_ptr<> const& x);

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

class uncaught_exception : public composite_object<uncaught_exception> {
public:
  static constexpr char const* scheme_name = "insider::uncaught_exception";

  ptr<> inner_exception;

  explicit
  uncaught_exception(ptr<> e)
    : inner_exception{e}
  { }

  void
  visit_members(member_visitor const& f);

  std::size_t
  hash() const;
};

} // namespace insider

#endif
