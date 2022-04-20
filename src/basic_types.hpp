#ifndef INSIDER_BASIC_TYPES_HPP
#define INSIDER_BASIC_TYPES_HPP

#include "bytecode.hpp"
#include "compare.hpp"
#include "context.hpp"
#include "free_store.hpp"
#include "object.hpp"

#include <utility>

namespace insider {

// The empty list. There should only be exactly one instance of this type per
// evaluation context.
class null_type : public leaf_object<null_type> {
public:
  static constexpr char const* scheme_name = "insider::null_type";
};

// The empty value. Like null_type, there should only be exactly one instance
// per evaluation context. This is used when no other meaningful value can be
// had -- such as the result of evaluating (if #f anything).
class void_type : public leaf_object<void_type> {
public:
  static constexpr char const* scheme_name = "insider::void_type";
};

// End of file sentinel value. Dummy value returned from I/O operations at the
// end of input.
class eof_type : public leaf_object<eof_type> {
public:
  static constexpr char const* scheme_name = "insider::eof_type";
};

// Dummy value for identifying parameters.
class parameter_tag : public leaf_object<parameter_tag> {
public:
  static constexpr char const* scheme_name = "insider::parameter_tag";
};

// Dummy value used to represent core forms.
class core_form_type : public leaf_object<core_form_type> {
public:
  static constexpr char const* scheme_name = "insider::core_form_type";

  std::string name;

  explicit
  core_form_type(std::string n) : name{std::move(n)} { }
};

// Dummy value used for implementing tail-calls from native procedures.
class tail_call_tag_type : public leaf_object<tail_call_tag_type> {
public:
  static constexpr char const* scheme_name = "insider::tail_call_tag_type";
};

// A boolean value.
class boolean : public leaf_object<boolean> {
public:
  static constexpr char const* scheme_name = "insider::boolean";

  explicit
  boolean(bool value) : value_{value} { }

  bool
  value() const { return value_; }

private:
  bool value_;
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

  // vector-like interface for uniform handling of pairs and vectors.

  ptr<>
  ref(std::size_t i) const {
    assert(i == 0 || i == 1);

    if (i == 0)
      return car_;
    else
      return cdr_;
  }

  void
  set(free_store& store, std::size_t i, ptr<> p) {
    assert(i == 0 || i == 1);

    if (i == 0)
      car_ = p;
    else
      cdr_ = p;

    store.notify_arc(this, p);
  }

  std::size_t
  size() const { return 2; }

  void
  visit_members(member_visitor const& f) { f(car_); f(cdr_); }

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

template <typename T>
ptr<>
make_list_from_vector(context& ctx, std::vector<T> const& values) {
  return make_list_from_vector(ctx, values, [] (T x) { return x; });
}

// Concatenate a number of lists. If there are 0 lists, return the empty
// list. If there is 1 list, return it. Otherwise, return a new list whose
// elements are the elements of the given lists. The last argument doesn't have
// to be a list -- if it isn't, the result is an improper list.
ptr<>
append(context&, object_span);

bool
memq(ptr<> element, ptr<> list);

namespace detail {

  template <typename F>
  ptr<pair>
  map_non_empty_list(context& ctx, ptr<pair> head, F&& f) {
    ptr<pair> tail{};
    ptr<pair> new_head{};
    ptr<> current = head;
    while (is<pair>(current)) {
      auto p = assume<pair>(current);
      auto new_current = cons(ctx, f(car(p)), cdr(p));

      if (tail)
        tail->set_cdr(ctx.store, new_current);

      if (!new_head)
        new_head = new_current;

      tail = new_current;
      current = cdr(new_current);
    }

    if (current != ctx.constants->null.get())
      tail->set_cdr(ctx.store, f(current));

    return new_head;
  }

} // namespace detail

template <typename F>
ptr<>
map(context& ctx, ptr<> list, F&& f) {
  if (list == ctx.constants->null.get())
    return ctx.constants->null.get();
  else
    return detail::map_non_empty_list(ctx, assume<pair>(list), std::forward<F>(f));
}

// An array of a fixed, dynamic size. Elements are allocated as a part of this
// object, which requires cooperation from the allocator. From the C++ point of
// view, there is an array of ptr<> allocated right after the vector object.
class vector : public dynamic_size_object<vector, ptr<>> {
public:
  static constexpr char const* scheme_name = "insider::vector";

  static std::size_t
  extra_elements(std::size_t size, ptr<>) { return size; }

  vector(std::size_t, ptr<> fill);

  vector(vector&&);

  void
  visit_members(member_visitor const&);

  ptr<>
  ref(std::size_t) const;

  void
  set(free_store&, std::size_t, ptr<>);
};

inline void
vector_set(tracked_ptr<vector> const& v, std::size_t i, ptr<> value) { v->set(v.store(), i, value); }

template <typename It>
ptr<vector>
make_vector(context& ctx, It begin, It end) {
  auto result = make<vector>(ctx, end - begin, ctx.constants->void_.get());
  for (It elem = begin; elem != end; ++elem)
    result->set(ctx.store, elem - begin, *elem);

  return result;
}

inline ptr<vector>
make_vector(context& ctx, std::vector<ptr<>> const& elems) {
  return make_vector(ctx, elems.begin(), elems.end());
}

template <typename Container, typename Converter>
ptr<>
make_vector(context& ctx, Container const& values, Converter const& convert) {
  auto result = make<vector>(ctx, values.size(), ctx.constants->void_.get());

  for (std::size_t i = 0; i < values.size(); ++i)
    result->set(ctx.store, i, convert(values[i]));

  return result;
}

std::vector<ptr<>>
list_to_std_vector(ptr<>);

// A sequence of bytes.
class bytevector : public dynamic_size_object<bytevector, std::uint8_t> {
public:
  static constexpr char const* scheme_name = "insider::bytevector";

  using element_type = std::uint8_t;

  static std::size_t
  extra_elements(std::size_t size) { return size; }

  bytevector(std::size_t size);

  bytevector(bytevector&&);

  void
  set(std::size_t index, element_type value) { storage_element(index) = value; }

  element_type
  ref(std::size_t index) const { return storage_element(index); }

  element_type const*
  begin() const { return &storage_element(0); }

  element_type const*
  end() const { return &storage_element(0) + size(); }

  void
  visit_members(member_visitor const&);
};

ptr<bytevector>
make_bytevector_from_std_vector(context&, std::vector<std::uint8_t>);

bool
bytevector_eqv(ptr<bytevector>, ptr<bytevector>);

std::vector<std::uint8_t>
bytevector_data(ptr<bytevector>);

// An immutable string, used for identifying Scheme objects.
class symbol : public leaf_object<symbol> {
public:
  static constexpr char const* scheme_name = "insider::symbol";

  explicit
  symbol(std::string value) : value_{std::move(value)} { }

  std::string
  value() const { return value_; }

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

  void
  visit_members(member_visitor const&);

private:
  ptr<insider::procedure> procedure_;
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
template <typename T>
class opaque_value : public leaf_object<opaque_value<T>> {
public:
  static constexpr char const* scheme_name = "insider::opaque_value";

  T value;

  template <typename... Args>
  explicit
  opaque_value(Args&&... args)
    : value(std::forward<Args>(args)...)
  { }
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
};

// Scheme error object, as created by error.
class error : public composite_object<error> {
public:
  static constexpr char const* scheme_name = "insider::error";

  error(ptr<string> message, ptr<> irritants)
    : message_{message}
    , irritants_{irritants}
  { }

  ptr<string>
  message(context&) const { return message_; }

  ptr<>
  irritants(context&) const { return irritants_; }

  void
  visit_members(member_visitor const&);

private:
  ptr<string> message_;
  ptr<>       irritants_;
};

class file_error : public leaf_object<file_error> {
public:
  static constexpr char const* scheme_name = "insider::file_error";

  explicit
  file_error(std::string msg) : message_{std::move(msg)} { }

  std::string
  message() const { return message_; }

private:
  std::string message_;
};

class values_tuple : public dynamic_size_object<values_tuple, ptr<>> {
public:
  static constexpr char const* scheme_name = "insider::values_tuple";

  static std::size_t
  extra_elements(object_span values) { return values.size(); }

  template <typename... Ts>
  static std::size_t
  extra_elements(ptr<Ts>...) { return sizeof...(Ts); }

  explicit
  values_tuple(object_span values);

  template <typename... Ts>
  values_tuple(ptr<Ts>... elems)
    : dynamic_size_object{sizeof...(elems)}
  {
    assign_elements(std::index_sequence_for<Ts...>{}, elems...);
  }

  values_tuple(values_tuple&& other);

  ptr<>
  ref(std::size_t i) const;

  void
  visit_members(member_visitor const& f);

private:
  template <typename... Ts, std::size_t... Is>
  void
  assign_elements(std::index_sequence<Is...>, ptr<Ts>... elems) {
    ((storage_element(Is) = elems), ...);
  }
};

} // namespace insider

#endif
