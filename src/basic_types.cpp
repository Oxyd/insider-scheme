#include "basic_types.hpp"

#include "list_iterator.hpp"

namespace insider {

string::string(string&& other)
  : size_{other.size_}
{
  for (std::size_t i = 0; i < size_; ++i)
    storage_element(i) = other.storage_element(i);
}

void
string::set(std::size_t i, char c) {
  assert(i < size_);
  storage_element(i) = c;
}

std::string
string::value() const {
  std::string result;
  result.reserve(size_);

  for (std::size_t i = 0; i < size_; ++i)
    result += storage_element(i);

  return result;
}

std::size_t
string::hash() const {
  // djb2
  std::size_t result = 5381;

  for (std::size_t i = 0; i < size_; ++i)
    result = ((result << 5) + result) + storage_element(i);

  return result;
}

ptr<string>
make_string(context& ctx, std::string_view value) {
  auto result = make<string>(ctx, value.size());

  for (std::size_t i = 0; i < value.size(); ++i)
    result->set(i, value[i]);

  return result;
}

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
  while (x != xs.end() && *x == ctx.constants->null.get())
    ++x;

  if (x == xs.end())
    return ctx.constants->null.get();

  if (x == xs.end() - 1)
    return *x;

  // We have more than one list, and at least the current list is non-empty. Do
  // the needful.

  ptr<> new_head = ctx.constants->null.get();
  ptr<pair> new_tail = nullptr;
  ptr<> current = expect<pair>(*x);
  for (; x != xs.end() - 1; ++x) {
    current = *x;

    while (current != ctx.constants->null.get()) {
      ptr<pair> c = expect<pair>(current);
      ptr<pair> new_c = make<pair>(ctx, car(c), ctx.constants->null.get());

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

vector::vector(context& ctx, std::size_t size)
  : size_{size}
{
  for (std::size_t i = 0; i < size_; ++i)
    storage_element(i) = ctx.constants->void_.get();
}

vector::vector(vector&& other)
  : size_{other.size_}
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

std::size_t
vector::hash() const {
  std::size_t result = 0;

  for (std::size_t i = 0; i < size_; ++i)
    result = insider::hash(storage_element(i)) + (result << 6) + (result << 16) - result;

  return result;
}

ptr<vector>
make_vector(context& ctx, std::vector<ptr<>> const& elems) {
  auto result = make<vector>(ctx, ctx, elems.size());
  for (std::size_t i = 0; i < elems.size(); ++i)
    result->set(ctx.store, i, elems[i]);

  return result;
}

ptr<vector>
list_to_vector(context& ctx, ptr<> lst) {
  std::size_t size = 0;
  for (ptr<> e = lst; e != ctx.constants->null.get(); e = cdr(expect<pair>(e)))
    ++size;

  auto result = make<vector>(ctx, ctx, size);
  std::size_t i = 0;
  for (ptr<> e = lst; e != ctx.constants->null.get(); e = cdr(assume<pair>(e)))
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

ptr<>
vector_to_list(context& ctx, ptr<vector> v) {
  ptr<> result = ctx.constants->null.get();
  for (std::size_t i = v->size(); i > 0; --i)
    result = cons(ctx, v->ref(i - 1), result);

  return result;
}

ptr<vector>
vector_append(context& ctx, object_span vs) {
  std::size_t size = 0;
  for (ptr<> e : vs) {
    ptr<vector> v = expect<vector>(e);
    size += v->size();
  }

  auto result = make<vector>(ctx, ctx, size);
  std::size_t i = 0;
  for (ptr<> e : vs) {
    ptr<vector> v = assume<vector>(e);
    for (std::size_t j = 0; j < v->size(); ++j)
      result->set(ctx.store, i++, v->ref(j));
  }

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

std::size_t
procedure::hash() const {
  return std::hash<std::uint64_t>{}(entry_pc);
}

ptr<procedure>
make_procedure(context& ctx, bytecode const& bc, unsigned locals_size,
               unsigned min_args, bool has_rest, std::optional<std::string> name) {
  std::size_t entry = ctx.program.size();
  ctx.program.insert(ctx.program.end(), bc.begin(), bc.end());
  return make<procedure>(ctx, entry, bc.size(), locals_size, min_args, has_rest, std::move(name));
}

closure::closure(ptr<insider::procedure> p, std::size_t num_captures)
  : procedure_{p}
  , size_{num_captures}
{ }

closure::closure(closure&& other)
  : procedure_{other.procedure_}
  , size_{other.size_}
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

std::size_t
uncaught_exception::hash() const {
  return insider::hash(inner_exception);
}

} // namespace insider
