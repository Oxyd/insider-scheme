#include "basic_types.hpp"

#include "util/define_procedure.hpp"
#include "util/list_iterator.hpp"

namespace insider {

bool
is_list(ptr<> x) {
  while (true)
    if (is<null_type>(x))
      return true;
    else if (ptr<pair> p = match<pair>(x))
      x = cdr(p);
    else
      return false;
}

std::size_t
list_length(ptr<> x) {
  std::size_t result = 0;
  while (!is<null_type>(x)) {
    x = cdr(expect<pair>(x));
    ++result;
  }
  return result;
}

ptr<>
cadr(ptr<pair> x) {
  return car(expect<pair>(cdr(x)));
}

ptr<>
caddr(ptr<pair> x) {
  return car(expect<pair>(cddr(x)));
}

ptr<>
cadddr(ptr<pair> x) {
  return car(expect<pair>(cdddr(x)));
}

ptr<>
cddr(ptr<pair> x) {
  return cdr(expect<pair>(cdr(x)));
}

ptr<>
cdddr(ptr<pair> x) {
  return cdr(expect<pair>(cddr(x)));
}

ptr<>
append(context& ctx, object_span xs) {
  // If all the lists are empty, we return the empty list as well.

  auto x = xs.begin();
  while (x != xs.end() && *x == ctx.constants->null)
    ++x;

  if (x == xs.end())
    return ctx.constants->null;

  if (x == xs.end() - 1)
    return *x;

  // We have more than one list, and at least the current list is non-empty. Do
  // the needful.

  ptr<> new_head = ctx.constants->null;
  ptr<pair> new_tail = nullptr;
  ptr<> current = expect<pair>(*x);
  for (; x != xs.end() - 1; ++x) {
    current = *x;

    while (current != ctx.constants->null) {
      ptr<pair> c = expect<pair>(current);
      ptr<pair> new_c = make<pair>(ctx, car(c), ctx.constants->null);

      if (new_tail)
        new_tail->set_cdr(ctx.store, new_c);
      else
        new_head = new_c;

      new_tail = new_c;
      current = cdr(c);
    }
  }

  assert(x == xs.end() - 1);
  if (new_tail)
    new_tail->set_cdr(ctx.store, *x);
  else
    new_head = *x;

  return new_head;
}

bool
memq(ptr<> element, ptr<> list) {
  for (ptr<> f : in_list{list})
    if (f == element)
      return true;
  return false;
}

vector::vector(std::size_t size, ptr<> fill)
  : dynamic_size_object{size}
{
  for (std::size_t i = 0; i < size_; ++i)
    storage_element(i) = fill;
}

vector::vector(vector&& other)
  : dynamic_size_object{other.size_}
{
  for (std::size_t i = 0; i < size_; ++i)
    storage_element(i) = other.storage_element(i);
}

void
vector::visit_members(member_visitor const& f) {
  for (std::size_t i = 0; i < size_; ++i)
    f(storage_element(i));
}

ptr<>
vector::ref(std::size_t i) const {
  if (i >= size_)
    throw std::runtime_error{fmt::format("Vector access out of bounds: index = {}, size = {}", i, size_)};

  return storage_element(i);
}

void
vector::set(free_store& store, std::size_t i, ptr<> value) {
  if (i >= size_)
    throw std::runtime_error{fmt::format("Vector access out of bounds: index = {}, size = {}", i, size_)};

  storage_element(i) = value;
  store.notify_arc(this, value);
}

static ptr<vector>
list_to_vector(context& ctx, ptr<> lst) {
  std::size_t size = 0;
  for (ptr<> e = lst; e != ctx.constants->null; e = cdr(expect<pair>(e)))
    ++size;

  auto result = make<vector>(ctx, size, ctx.constants->void_);
  std::size_t i = 0;
  for (ptr<> e = lst; e != ctx.constants->null; e = cdr(assume<pair>(e)))
    result->set(ctx.store, i++, car(assume<pair>(e)));

  return result;
}

std::vector<ptr<>>
list_to_std_vector(ptr<> lst) {
  std::vector<ptr<>> result;
  for (ptr<> e : in_list{lst})
    result.push_back(e);

  return result;
}

static ptr<>
vector_to_list(context& ctx, object_span args) {
  require_arg_count(args, 1, 3);
  ptr<vector> v = require_arg<vector>(args, 0);
  integer::value_type start = optional_arg<integer>(args, 1, 0).value();
  integer::value_type end = optional_arg<integer>(args, 2, v->size()).value();

  if (start > end)
    throw std::runtime_error{"Start can't be larger than end"};
  if (start < 0 || end < 0
      || start > static_cast<integer::value_type>(v->size())
      || end > static_cast<integer::value_type>(v->size()))
    throw std::runtime_error{"Argument out of bounds"};

  ptr<> result = ctx.constants->null;
  for (integer::value_type i = end; i > start; --i)
    result = cons(ctx, v->ref(i - 1), result);

  return result;
}

static ptr<>
vector_append(context& ctx, object_span vs) {
  std::size_t size = 0;
  for (ptr<> e : vs) {
    ptr<vector> v = expect<vector>(e);
    size += v->size();
  }

  auto result = make<vector>(ctx, size, ctx.constants->void_);
  std::size_t i = 0;
  for (ptr<> e : vs) {
    ptr<vector> v = assume<vector>(e);
    for (std::size_t j = 0; j < v->size(); ++j)
      result->set(ctx.store, i++, v->ref(j));
  }

  return result;
}

bytevector::bytevector(std::size_t size)
  : dynamic_size_object{size}
{ }

bytevector::bytevector(bytevector&& other)
  : dynamic_size_object{other.size()}
{
  for (std::size_t i = 0; i < size(); ++i)
    storage_element(i) = other.storage_element(i);
}

void
bytevector::visit_members(member_visitor const&) { }

ptr<bytevector>
make_bytevector_from_std_vector(context& ctx, std::vector<std::uint8_t> data) {
  auto result = make<bytevector>(ctx, data.size());
  for (std::size_t i = 0; i < data.size(); ++i)
    result->set(i, data[i]);
  return result;
}

bool
bytevector_eqv(ptr<bytevector> x, ptr<bytevector> y) {
  return std::equal(x->begin(), x->end(), y->begin(), y->end());
}

std::vector<std::uint8_t>
bytevector_data(ptr<bytevector> bv) {
  std::vector<std::uint8_t> result;
  result.reserve(bv->size());
  for (std::size_t i = 0; i < bv->size(); ++i)
    result.push_back(bv->ref(i));
  return result;
}

box::box(ptr<> value)
  : value_{value}
{ }

procedure::procedure(integer::value_type entry_pc, std::size_t bytecode_size, unsigned locals_size,
                     unsigned min_args, bool has_rest, std::optional<std::string> name)
  : entry_pc{entry_pc}
  , bytecode_size{bytecode_size}
  , locals_size{locals_size}
  , min_args{min_args}
  , has_rest{has_rest}
  , name{std::move(name)}
{ }

ptr<procedure>
make_procedure(context& ctx, bytecode const& bc, unsigned locals_size,
               unsigned min_args, bool has_rest, std::optional<std::string> name) {
  std::size_t entry = ctx.program.size();
  ctx.program.insert(ctx.program.end(), bc.begin(), bc.end());
  return make<procedure>(ctx, entry, bc.size(), locals_size, min_args, has_rest, std::move(name));
}

closure::closure(ptr<insider::procedure> p, std::size_t num_captures)
  : dynamic_size_object{num_captures}
  , procedure_{p}
{ }

closure::closure(closure&& other)
  : dynamic_size_object{other.size_}
  , procedure_{other.procedure_}
{
  for (std::size_t i = 0; i < size_; ++i)
    storage_element(i) = other.storage_element(i);
}

ptr<>
closure::ref(std::size_t i) const {
  assert(i < size_);
  return storage_element(i);
}

void
closure::set(free_store& store, std::size_t i, ptr<> value) {
  assert(i < size_);
  assert(value);

  storage_element(i) = value;
  store.notify_arc(this, value);
}

void
closure::visit_members(member_visitor const& f) {
  f(procedure_);
  for (std::size_t i = 0; i < size_; ++i)
    f(storage_element(i));
}

bool
is_callable(ptr<> x) {
  return is<procedure>(x) || is<native_procedure>(x) || is<closure>(x);
}

ptr<>
expect_callable(ptr<> x) {
  if (!is_callable(x))
    throw std::runtime_error{"Expected a callable"};
  else
    return x;
}

void
transformer::visit_members(member_visitor const& f) {
  f(callable_);
}

void
uncaught_exception::visit_members(member_visitor const& f) {
  f(inner_exception);
}

void
error::visit_members(member_visitor const& f) {
  f(message_);
  f(irritants_);
}

values_tuple::values_tuple(object_span values)
  : dynamic_size_object{values.size()}
{
  for (std::size_t i = 0; i < size_; ++i)
    storage_element(i) = values[i];
}

values_tuple::values_tuple(values_tuple&& other)
  : dynamic_size_object{other.size_}
{
  for (std::size_t i = 0; i < size_; ++i)
    storage_element(i) = other.storage_element(i);
}

ptr<>
values_tuple::ref(std::size_t i) const {
  if (i > size())
    throw std::runtime_error{"Index out of range"};

  return storage_element(i);
}

void
values_tuple::visit_members(member_visitor const& f) {
  for (std::size_t i = 0; i < size_; ++i)
    f(storage_element(i));
}

static ptr<bytevector>
make_bytevector(context& ctx, std::size_t len, bytevector::element_type fill) {
  auto result = make<bytevector>(ctx, len);
  for (std::size_t i = 0; i < len; ++i)
    result->set(i, fill);
  return result;
}

static ptr<>
make_bytevector_elems(context& ctx, object_span args) {
  auto result = make<bytevector>(ctx, args.size());
  for (std::size_t i = 0; i < args.size(); ++i)
    result->set(i, from_scheme<bytevector::element_type>(ctx, args[i]));
  return result;
}

static ptr<>
vector_proc(context& ctx, object_span args) {
  return make_vector(ctx, args.begin(), args.end());
}

static ptr<vector>
make_vector_proc(context& ctx, std::size_t len, ptr<> fill) {
  return make<vector>(ctx, len, fill);
}

static void
vector_set(context& ctx, ptr<vector> v, std::size_t i, ptr<> o) {
  v->set(ctx.store, i, o);
}

static ptr<error>
make_error_proc(context& ctx, ptr<string> m, ptr<> i) {
  return make<error>(ctx, m, i);
}

static ptr<>
uncaught_exception_inner_exception(ptr<uncaught_exception> e) {
  return e->inner_exception;
}

void
export_basic_types(context& ctx, module_& result) {
  define_procedure<list_to_vector>(ctx, "list->vector", result, true);
  define_raw_procedure<vector_to_list>(ctx, "vector->list", result, true);
  define_raw_procedure<vector_append>(ctx, "vector-append", result, true);
  define_procedure<&vector::size>(ctx, "vector-length", result, true);
  define_raw_procedure<vector_proc>(ctx, "vector", result, true);
  define_procedure<make_vector_proc>(
    ctx, "make-vector", result, true,
    [] (context& ctx) { return ctx.constants->void_; }
  );
  operand vector_ref_index = define_procedure<&vector::ref>(ctx, "vector-ref", result, true);
  ctx.tag_top_level(vector_ref_index, special_top_level_tag::vector_ref);

  operand vector_set_index = define_procedure<vector_set>(ctx, "vector-set!", result, true);
  ctx.tag_top_level(vector_set_index, special_top_level_tag::vector_set);

  define_procedure<make_error_proc>(ctx, "make-error", result, true);
  define_procedure<&error::message>(ctx, "error-message", result, true);
  define_procedure<&error::irritants>(ctx, "error-irritants", result, true);
  define_procedure<uncaught_exception_inner_exception>(ctx, "uncaught-exception-inner-exception", result, true);
  define_procedure<&file_error::message>(ctx, "file-error-message", result, true);

  define_procedure<make_bytevector>(ctx, "make-bytevector", result, true, [] (context&) -> bytevector::element_type { return 0; });
  define_raw_procedure<make_bytevector_elems>(ctx, "bytevector", result, true);
  define_procedure<&bytevector::size>(ctx, "bytevector-length", result, true);
  define_procedure<&bytevector::ref>(ctx, "bytevector-u8-ref", result, true);
  define_procedure<&bytevector::set>(ctx, "bytevector-u8-set!", result, true);
  define_procedure<&values_tuple::size>(ctx, "values-tuple-length", result, true);
  define_procedure<&values_tuple::ref>(ctx, "values-tuple-ref", result, true);
}

} // namespace insider