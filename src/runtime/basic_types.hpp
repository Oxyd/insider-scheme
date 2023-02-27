#ifndef INSIDER_RUNTIME_BASIC_TYPES_HPP
#define INSIDER_RUNTIME_BASIC_TYPES_HPP

#include "compiler/debug_info.hpp"
#include "context.hpp"
#include "memory/free_store.hpp"
#include "memory/member_ptr.hpp"
#include "object.hpp"
#include "type_indexes.hpp"
#include "util/object_span.hpp"
#include "vm/bytecode.hpp"

#include <memory>
#include <ranges>
#include <utility>

namespace insider {

class native_procedure;
class vm;

// The empty list. There should only be exactly one instance of this type per
// evaluation context.
class null_type : public leaf_object<null_type> {
public:
  static constexpr char const* scheme_name = "insider::null_type";
  static constexpr word_type static_type_index = type_indexes::null;
};

// The empty value. Like null_type, there should only be exactly one instance
// per evaluation context. This is used when no other meaningful value can be
// had -- such as the result of evaluating (if #f anything).
class void_type : public leaf_object<void_type> {
public:
  static constexpr char const* scheme_name = "insider::void_type";
  static constexpr word_type static_type_index = type_indexes::void_;
};

// End of file sentinel value. Dummy value returned from I/O operations at the
// end of input.
class eof_type : public leaf_object<eof_type> {
public:
  static constexpr char const* scheme_name = "insider::eof_type";
  static constexpr word_type static_type_index = type_indexes::eof;
};

// The value passed when an optional parameter is left unspecified.
class default_value_type : public leaf_object<default_value_type> {
public:
  static constexpr char const* scheme_name = "insider::default_value_type";
  static constexpr word_type static_type_index = type_indexes::default_value;
};

inline bool
is_default_value(ptr<> x) { return is<default_value_type>(x); }

// Dummy value for identifying parameters.
class parameter_tag : public leaf_object<parameter_tag> {
public:
  static constexpr char const* scheme_name = "insider::parameter_tag";
  static constexpr word_type static_type_index = type_indexes::parameter_tag;
};

// Dummy value used to represent core forms.
class core_form_type : public leaf_object<core_form_type> {
public:
  static constexpr char const* scheme_name = "insider::core_form_type";
  static constexpr word_type static_type_index = type_indexes::core_form;

  std::string name;

  explicit
  core_form_type(std::string n) : name{std::move(n)} { }
};

// Symbol-like object but with different read syntax and used for different
// syntactic purposes.
class keyword : public leaf_object<keyword> {
public:
  static constexpr char const* scheme_name = "insider::keyword";
  static constexpr word_type static_type_index = type_indexes::keyword;

  explicit
  keyword(std::string value) : value_{std::move(value)} { }

  std::string
  value() const { return value_; }

private:
  std::string value_;
};

// Dummy value used for implementing tail-calls from native procedures.
class tail_call_tag_type : public leaf_object<tail_call_tag_type> {
public:
  static constexpr char const* scheme_name = "insider::tail_call_tag_type";
  static constexpr word_type static_type_index = type_indexes::tail_call_tag;
};

// A boolean value.
class boolean : public leaf_object<boolean> {
public:
  static constexpr char const* scheme_name = "insider::boolean";
  static constexpr word_type static_type_index = type_indexes::boolean;

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
  static constexpr word_type static_type_index = type_indexes::pair;

  pair(ptr<> car, ptr<> cdr)
    : car_{member_ptr<>::initialise(car)}
    , cdr_{member_ptr<>::initialise(cdr)}
  { }

  ptr<>
  car() const { return car_; }
  ptr<>
  cdr() const { return cdr_; }

  void
  set_car(free_store& store, ptr<> p) { 
    car_.assign(store, this, p);
  }
  void
  set_cdr(free_store& store, ptr<> p) { 
    cdr_.assign(store, this, p);
  }

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
      car_.assign(store, this, p);
    else
      cdr_.assign(store, this, p);
  }

  static std::size_t
  size() { return 2; }

  void
  visit_members(member_visitor const& f) const { 
    car_.visit_members(f);
    cdr_.visit_members(f);
  }

private:
  member_ptr<> car_;
  member_ptr<> cdr_;
};

inline ptr<pair>
cons(context& ctx, ptr<> car, ptr<> cdr) {
  return make<pair>(ctx, car, cdr);
}

ptr<>
list(context&, ptr<native_procedure>, object_span);

// Is the given object a list? A list is either the null value or a pair whose
// cdr is a list.
bool
is_list(ptr<>);

std::size_t
list_length(ptr<>);

inline ptr<>
car(ptr<pair> x) { return x->car(); }

inline ptr<>
cdr(ptr<pair> x) { return x->cdr(); }

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

  ptr<> result = ctx.constants->null;
  for (std::size_t i = n; i > 0; --i)
    result = make<pair>(ctx, elements[i - 1], result);

  return result;
}

template <std::ranges::bidirectional_range Range, typename Converter>
ptr<>
make_list_from_range(context& ctx, Range const& range,
                     Converter const& convert) {
  ptr<> head = ctx.constants->null;

  for (auto&& elem : range | std::views::reverse)
    head = cons(ctx, to_scheme(ctx, convert(elem)), head);

  return head;
}

template <std::ranges::forward_range Range, typename Converter>
ptr<>
make_list_from_range(context& ctx, Range const& range,
                     Converter const& convert) {
  if (std::ranges::empty(range))
    return ctx.constants->null;

  auto elem = std::ranges::begin(range);
  auto end = std::ranges::end(range);
  ptr<pair> head = cons(ctx, convert(*elem++), ctx.constants->null);
  ptr<pair> current = head;

  while (elem != end) {
    ptr<pair> p = cons(ctx, convert(*elem++), ctx.constants->null);
    current->set_cdr(ctx.store, p);
    current = p;
  }

  return head;
}

template <typename Range>
ptr<>
make_list_from_range(context& ctx, Range const& values) {
  return make_list_from_range(ctx, values, [] (auto&& x) { return x; });
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

    if (current != ctx.constants->null)
      tail->set_cdr(ctx.store, f(current));

    return new_head;
  }

} // namespace detail

template <typename F>
ptr<>
map(context& ctx, ptr<> list, F&& f) {
  if (list == ctx.constants->null)
    return ctx.constants->null;
  else
    return detail::map_non_empty_list(ctx, assume<pair>(list),
                                      std::forward<F>(f));
}

// An array of a fixed, dynamic size. Elements are allocated as a part of this
// object, which requires cooperation from the allocator. From the C++ point of
// view, there is an array of ptr<> allocated right after the vector object.
class vector : public dynamic_size_object<vector, member_ptr<>> {
public:
  static constexpr char const* scheme_name = "insider::vector";
  static constexpr word_type static_type_index = type_indexes::vector;

  static std::size_t
  extra_elements(std::size_t size, ptr<>) { return size; }

  vector(std::size_t, ptr<> fill);

  vector(vector&&) noexcept;

  void
  visit_members(member_visitor const&) const;

  ptr<>
  ref(std::size_t) const;

  void
  set(free_store&, std::size_t, ptr<>);
};

inline void
vector_set(context& ctx, ptr<vector> v, std::size_t i, ptr<> o) {
  v->set(ctx.store, i, o);
}

template <typename It>
ptr<vector>
make_vector(context& ctx, It begin, It end) {
  auto result = make<vector>(ctx, end - begin, ctx.constants->void_);
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
  auto result = make<vector>(ctx, values.size(), ctx.constants->void_);

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
  static constexpr word_type static_type_index = type_indexes::bytevector;

  using element_type = std::uint8_t;

  static std::size_t
  extra_elements(std::size_t size) { return size; }

  bytevector(std::size_t size);

  bytevector(bytevector&&) noexcept;

  void
  set(std::size_t index, element_type value) { storage_element(index) = value; }

  element_type
  ref(std::size_t index) const { return storage_element(index); }

  element_type const*
  begin() const { return &storage_element(0); }

  element_type const*
  end() const { return &storage_element(0) + size(); }

  void
  visit_members(member_visitor const&) const;
};

ptr<bytevector>
make_bytevector_from_std_vector(context&, std::vector<std::uint8_t>);

bool
bytevector_eqv(ptr<bytevector>, ptr<bytevector>);

std::vector<std::uint8_t>
bytevector_data(ptr<bytevector>);

// Mutable container for a single element. Essentially a pointer.
class box : public composite_object<box> {
public:
  static constexpr char const* scheme_name = "insider::box";
  static constexpr word_type static_type_index = type_indexes::box;

  explicit
  box(ptr<>);

  ptr<>
  get() const { return value_; }

  void
  set(free_store& store, ptr<> value) {
    value_.assign(store, this, value);
  }

  void
  visit_members(member_visitor const& f) const {
    value_.visit_members(f);
  }

private:
  member_ptr<> value_;
};

inline ptr<box>
make_box(context& ctx, ptr<> value) {
  return make<box>(ctx, value);
}

inline ptr<>
unbox(ptr<box> b) {
  return b->get();
}

inline void
box_set(context& ctx, ptr<box> b, ptr<> value) {
  b->set(ctx.store, value);
}

// Static information about a Scheme procedure.
class procedure_prototype : public composite_object<procedure_prototype> {
public:
  static constexpr char const* scheme_name = "insider::procedure_prototype";
  static constexpr word_type static_type_index
    = type_indexes::procedure_prototype;

  struct meta {
    unsigned                        locals_size;
    unsigned                        num_required_args;
    unsigned                        num_leading_args;
    bool                            has_rest;
    std::unique_ptr<ptr<keyword>[]> parameter_names;
    std::string                     name;
    debug_info_map                  debug_info;
  };

  bytecode                 code;
  std::size_t              code_size;
  std::unique_ptr<ptr<>[]> constants;
  std::size_t              constants_size;
  meta                     info;

  procedure_prototype(mutable_bytecode bc, meta i,
                      std::vector<ptr<>> const& constants);

  void
  visit_members(member_visitor const& f) const;
};

ptr<procedure_prototype>
make_procedure_prototype(context& ctx, mutable_bytecode bc,
                         procedure_prototype::meta i,
                         std::vector<ptr<>> constants);

// Callable Scheme code, together with any captured free variables.
class procedure : public dynamic_size_object<procedure, ptr<>> {
public:
  static constexpr char const* scheme_name = "insider::procedure";
  static constexpr word_type static_type_index = type_indexes::procedure;

  static std::size_t
  extra_elements(ptr<procedure_prototype>, std::size_t num_captures) {
    return num_captures;
  }

  procedure(ptr<procedure_prototype>, std::size_t num_captures);

  procedure(procedure&&) noexcept;

  ptr<procedure_prototype>
  prototype() const { return prototype_; }

  ptr<>
  ref(std::size_t) const;

  void
  set(free_store& store, std::size_t, ptr<>);

  void
  visit_members(member_visitor const&) const;

private:
  ptr<procedure_prototype> const prototype_;
};

ptr<procedure>
make_captureless_procedure(context&, ptr<procedure_prototype>);

ptr<procedure>
make_procedure(context& ctx, mutable_bytecode const& bc,
               procedure_prototype::meta i,
               std::vector<ptr<>> constants);

// Like procedure, but when invoked, it calls a C++ function.
class native_procedure : public leaf_object<native_procedure> {
public:
  static constexpr char const* scheme_name = "insider::native_procedure";
  static constexpr word_type static_type_index = type_indexes::native_procedure;

  class extra_data {
  public:
    virtual
    ~extra_data() = default;
  };

  using target_type = ptr<> (*)(vm&, ptr<native_procedure>, object_span);
  target_type                 target;
  std::string                 name;
  std::unique_ptr<extra_data> extra;
  bool                        constant_evaluable = false;

  explicit
  native_procedure(target_type f,
                   bool constant_evaluable = false,
                   std::string name = "<native procedure>",
                   std::unique_ptr<extra_data> extra = {})
    : target{f}
    , name{std::move(name)}
    , extra{std::move(extra)}
    , constant_evaluable{constant_evaluable}
  { }

  native_procedure(target_type f, std::unique_ptr<extra_data> extra)
    : target{f}
    , name{"<native procedure>"}
    , extra{std::move(extra)}
  { }
};

bool
is_callable(ptr<> x);

bool
is_procedure(ptr<>);

ptr<>
expect_callable(ptr<> x);

// The rest of a native procedure after a call to a Scheme procedure.
class native_continuation : public composite_object<native_continuation> {
public:
  static constexpr char const* scheme_name = "insider::native_continuation";
  static constexpr word_type static_type_index
    = type_indexes::native_continuation;

  using target_type = std::function<ptr<>(vm&, ptr<>)>;

  // proc is the procedure this is a continuation of. This is usually a
  // native_procedure, but when an exception is raised during an execution of
  // a Scheme procedure, a native_continuation will be created whose proc
  // will be that Scheme procedure.

  target_type target;
  ptr<> const proc;

  native_continuation(target_type t, ptr<> proc)
    : target{std::move(t)}
    , proc{proc}
  { }

  void
  visit_members(member_visitor const& f) const { f(proc); }
};

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

  ptr<> const inner_exception;

  explicit
  uncaught_exception(ptr<> e)
    : inner_exception{e}
  { }

  void
  visit_members(member_visitor const& f) const;
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
  visit_members(member_visitor const&) const;

private:
  ptr<string> const message_;
  ptr<> const       irritants_;
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
  static constexpr word_type static_type_index = type_indexes::values_tuple;

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

  values_tuple(values_tuple&& other) noexcept;

  ptr<>
  ref(std::size_t i) const;

  void
  visit_members(member_visitor const& f) const;

private:
  template <typename... Ts, std::size_t... Is>
  void
  assign_elements(std::index_sequence<Is...>, ptr<Ts>... elems) {
    ((storage_element(Is) = elems), ...);
  }
};

} // namespace insider

#endif
