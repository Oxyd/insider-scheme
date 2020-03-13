#include "scheme.hpp"

#include "converters.hpp"
#include "io.hpp"

#include <fmt/format.h>

#include <algorithm>
#include <cassert>
#include <cstdio>

namespace scm {

bool
object::eqv(generic_ptr const& other) const {
  return this == other.get();
}

generic_ptr::generic_ptr(free_store& store, object* value)
  : generic_ptr_base{store, value}
{
  store.register_root(this);
}

generic_ptr::generic_ptr(generic_ptr const& other)
  : generic_ptr_base(other)
{
  assert(store_ || !value_);

  if (store_)
    store_->register_root(this);
}

generic_ptr::~generic_ptr() {
  if (store_)
    store_->unregister_root(this);
}

generic_ptr&
generic_ptr::operator = (generic_ptr const& other) {
  if (this == &other)
    return *this;

  if (store_ != other.store_) {
    if (store_)
      store_->unregister_root(this);

    store_ = other.store_;
    if (store_)
      store_->register_root(this);
  }

  value_ = other.value_;
  assert(store_ || !value_);

  return *this;
}

bool
equal(generic_ptr const& x, generic_ptr const& y) {
  // XXX: This will break on infinite data structures.

  struct record {
    generic_ptr left, right;
  };

  std::vector<record> stack{{x, y}};
  while (!stack.empty()) {
    record top = stack.back();
    stack.pop_back();

    if (!eqv(top.left, top.right)) {
      if (is<pair>(top.left) && is<pair>(top.right)) {
        auto l = assume<pair>(top.left);
        auto r = assume<pair>(top.right);

        stack.push_back({cdr(l), cdr(r)});
        stack.push_back({car(l), car(r)});
      }
      else if (is<vector>(top.left) && is<vector>(top.right)) {
        auto l = assume<vector>(top.left);
        auto r = assume<vector>(top.right);

        if (l->size() != r->size())
          return false;

        for (std::size_t i = l->size(); i > 0; --i)
          stack.push_back({vector_ref(l, i - 1), vector_ref(r, i - 1)});
      }
      else if (is<string>(top.left) && is<string>(top.right)) {
        if (assume<string>(top.left)->value() != assume<string>(top.right)->value())
          return false;
      }
      else
        return false;
    }
  }

  return true;
}

generic_weak_ptr::generic_weak_ptr(free_store& store, object* value)
  : generic_ptr_base{store, value}
{
  if (value)
    store.register_weak(this, value);
}

generic_weak_ptr::generic_weak_ptr(generic_weak_ptr const& other)
  : generic_weak_ptr{other.store(), other.value_}
{
  assert(store_ || !value_);

  if (store_ && value_)
    store_->register_weak(this, value_);
}

generic_weak_ptr::~generic_weak_ptr() {
  if (value_ && store_)
    store_->unregister_weak(this, value_);
}

generic_weak_ptr&
generic_weak_ptr::operator = (generic_weak_ptr const& other) {
  if (this == &other)
    return *this;
  else
    return operator = (other.value_);
}

generic_weak_ptr&
generic_weak_ptr::operator = (object* value) {
  if (value_)
    store_->unregister_weak(this, value_);

  value_ = value;

  if (value_)
    store_->register_weak(this, value_);

  return *this;
}

generic_ptr
generic_weak_ptr::lock() const {
  return {*store_, value_};
}

static void
destroy(object* o) {
  o->~object();
  delete [] reinterpret_cast<std::byte*>(o);
}

free_store::~free_store() {
  for (object* o : objects_)
    destroy(o);
}

void
free_store::register_root(generic_ptr* root) {
  roots_.emplace(root);
}

void
free_store::unregister_root(generic_ptr* root) {
  roots_.erase(root);

#ifndef NDEBUG
  collect_garbage();
#endif
}

void
free_store::register_weak(generic_weak_ptr* ptr, object* pointee) {
  weak_ptrs_.emplace(pointee, ptr);
}

void
free_store::unregister_weak(generic_weak_ptr* ptr, object* pointee) {
  auto [begin, end] = weak_ptrs_.equal_range(pointee);
  for (auto it = begin; it != end; ++it)
    if (it->second == ptr) {
      weak_ptrs_.erase(it);
      return;
    }
}

static void
mark(std::unordered_set<generic_ptr*> const& roots, bool current_mark) {
  std::vector<object*> stack;
  for (generic_ptr const* root : roots)
    if (*root)
      stack.push_back(root->get());

  while (!stack.empty()) {
    object* top = stack.back();
    stack.pop_back();

    if (top->mark != current_mark) {
      top->mark = current_mark;
      top->for_each_subobject([&] (object* subobject) {
        if (subobject && subobject->mark != current_mark)
          stack.push_back(subobject);
      });
    }
  }
}

static void
sweep(std::vector<object*>& objects,
      std::unordered_multimap<object*, generic_weak_ptr*>& weak_ptrs,
      bool current_mark) {
  auto live_end = std::partition(objects.begin(), objects.end(),
                                 [&] (auto const& object) {
                                   return object->mark == current_mark;
                                 });
  for (auto dead = live_end; dead != objects.end(); ++dead) {
    auto [weak_begin, weak_end] = weak_ptrs.equal_range(*dead);
    for (auto weak_ptr = weak_begin; weak_ptr != weak_end; ++weak_ptr)
      weak_ptr->second->reset();

    weak_ptrs.erase(weak_begin, weak_end);
    destroy(*dead);
  }

  objects.erase(live_end, objects.end());
}

void
free_store::collect_garbage() {
  current_mark_ = !current_mark_;
  mark(roots_, current_mark_);
  sweep(objects_, weak_ptrs_, current_mark_);
}

auto
module::find(std::string const& name) const -> std::optional<index_type> {
  if (auto it = bindings_.find(name); it != bindings_.end())
    return it->second;

  if (auto it = imports_.find(name); it != imports_.end())
    return it->second;

  return {};
}

void
module::add(std::string name, index_type i) {
  assert(!bindings_.count(name));
  bindings_.emplace(std::move(name), i);
}

void
module::export_(std::string name) {
  exports_.emplace(std::move(name));
}

void
module::import(std::string name, index_type i) {
  assert(!imports_.count(name));
  imports_.emplace(std::move(name), i);
}

void
import_all(module& to, module const& from) {
  for (auto const& name : from.exports())
    to.import(name, *from.find(name));
}

void
define_top_level(context& ctx, module& m, std::string const& name, generic_ptr const& object,
                 bool export_) {
  auto index = ctx.add_top_level(object);
  m.add(name, index);
  if (export_)
    m.export_(name);
}

static void
export_native(context& ctx, module& m, std::string const& name,
              generic_ptr (*f)(context&, std::vector<generic_ptr> const&), special_top_level_tag tag) {
  auto index = ctx.add_top_level(ctx.store.make<native_procedure>(f));
  ctx.tag_top_level(index, tag);
  m.add(name, index);
  m.export_(name);
}

static module
make_internal_module(context& ctx) {
  module result;
  export_native(ctx, result, "+", add, special_top_level_tag::plus);
  export_native(ctx, result, "-", subtract, special_top_level_tag::minus);
  export_native(ctx, result, "*", multiply, special_top_level_tag::times);
  export_native(ctx, result, "/", divide, special_top_level_tag::divide);
  export_native(ctx, result, "=", arith_equal, special_top_level_tag::arith_equal);
  export_native(ctx, result, "<", less, special_top_level_tag::less_than);
  export_native(ctx, result, ">", greater, special_top_level_tag::greater_than);

  define_lambda<void(context&, generic_ptr const&)>(
    ctx, result, "write-simple", true,
    [] (context& ctx, generic_ptr const& datum) {
      write_simple(ctx, datum, ctx.stdout);
    }
  );

  define_lambda<void(context&)>(
    ctx, result, "newline", true,
    [] (context& ctx) { ctx.stdout->write_char('\n'); }
  );

  define_top_level(ctx, result, "append", make<native_procedure>(ctx, append), true);
  define_lambda<ptr<vector>(context&, generic_ptr const&)>(ctx, result, "list->vector", true, list_to_vector);
  define_top_level(ctx, result, "vector-append", make<native_procedure>(ctx, vector_append), true);

  define_lambda<ptr<pair>(context&, generic_ptr const&, generic_ptr const&)>(
    ctx, result, "cons", true,
    [] (context& ctx, generic_ptr const& car, generic_ptr const& cdr) {
      return make<pair>(ctx, car, cdr);
    }
  );
  define_lambda<generic_ptr(ptr<pair> const&)>(ctx, result, "car", true, car);
  define_lambda<generic_ptr(ptr<pair> const&)>(ctx, result, "cdr", true, cdr);

  return result;
}

context::context() {
  constants.null = store.make<null_type>();
  constants.void_ = store.make<void_type>();
  constants.t = store.make<boolean>(true);
  constants.f = store.make<boolean>(false);

  internal_module = make_internal_module(*this);

  struct {
    ptr<core_form_type>& object;
    std::string          name;
  } core_forms[]{
    {constants.let,              "let"},
    {constants.set,              "set!"},
    {constants.lambda,           "lambda"},
    {constants.if_,              "if"},
    {constants.box,              "box"},
    {constants.unbox,            "unbox"},
    {constants.box_set,          "box-set!"},
    {constants.define,           "define"},
    {constants.define_syntax,    "define-syntax"},
    {constants.quote,            "quote"},
    {constants.quasiquote,       "quasiquote"},
    {constants.unquote,          "unquote"},
    {constants.unquote_splicing, "unquote-splicing"}
  };
  for (auto const& form : core_forms) {
    form.object = store.make<core_form_type>();
    auto index = add_top_level(form.object);
    internal_module.add(form.name, index);
    internal_module.export_(form.name);
  }

  statics.null = operand::static_(intern_static(constants.null));
  statics.void_ = operand::static_(intern_static(constants.void_));
  statics.t = operand::static_(intern_static(constants.t));
  statics.f = operand::static_(intern_static(constants.f));
  statics.zero = operand::static_(intern_static(store.make<integer>(0)));
  statics.one = operand::static_(intern_static(store.make<integer>(1)));

  stdout = make<port>(*this, ::stdout, false, true, false);
}

ptr<symbol>
context::intern(std::string const& s) {
  auto interned = interned_symbols_.find(s);
  if (interned != interned_symbols_.end()) {
    if (ptr<symbol> sym = interned->second.lock())
      return sym;
    else {
      ptr<symbol> result = store.make<symbol>(s);
      interned->second = result;
      return result;
    }
  }

  ptr<symbol> result = store.make<symbol>(s);
  interned_symbols_.emplace(s, result);

  return result;
}

operand::representation_type
context::intern_static(generic_ptr const& x) {
  auto it = statics_cache_.find(x);
  if (it == statics_cache_.end()) {
    statics_.push_back(x);
    it = statics_cache_.emplace(x, statics_.size() - 1).first;
  }

  return it->second;
}

generic_ptr
context::get_static(operand::representation_type i) const {
  assert(i < statics_.size());
  return statics_[i];
}

void
context::set_top_level(operand::representation_type i, generic_ptr const& value) {
  assert(i < top_level_objects_.size());
  top_level_objects_[i] = value;
}

operand::representation_type
context::add_top_level(generic_ptr const& x) {
  top_level_objects_.push_back(x);
  return top_level_objects_.size() - 1;
}

void
context::tag_top_level(operand::representation_type i, special_top_level_tag tag) {
  top_level_tags_.emplace(i, tag);
}

std::optional<special_top_level_tag>
context::find_tag(operand::representation_type i) const {
  if (auto it = top_level_tags_.find(i); it != top_level_tags_.end())
    return it->second;
  else
    return {};
}

bool
integer::eqv(generic_ptr const& other) const {
  if (auto y = match<integer>(other))
    return value() == y->value();
  else
    return false;
}

ptr<integer>
add(context& ctx, ptr<integer> const& lhs, ptr<integer> const& rhs) {
  return make<integer>(ctx, lhs->value() + rhs->value());
}

template <auto F>
generic_ptr
arithmetic(context& ctx, std::vector<generic_ptr> const& xs, bool allow_empty, integer::value_type neutral) {
  if (xs.empty()) {
    if (allow_empty)
      return make<integer>(ctx, neutral);
    else
      throw std::runtime_error{"Not enough arguments"};
  }
  else if (xs.size() == 1)
    return F(ctx, make<integer>(ctx, neutral), expect<integer>(xs.front()));
  else {
    ptr<integer> result = expect<integer>(xs.front());
    for (auto rhs = xs.begin() + 1; rhs != xs.end(); ++rhs)
      result = F(ctx, result, expect<integer>(*rhs));

    return result;
  }
}

using primitive_arithmetic_type = ptr<integer>(context& ctx, ptr<integer> const&, ptr<integer> const&);

generic_ptr
add(context& ctx, std::vector<generic_ptr> const& xs) {
  return arithmetic<static_cast<primitive_arithmetic_type*>(&add)>(ctx, xs, true, 0);
}

ptr<integer>
subtract(context& ctx, ptr<integer> const& lhs, ptr<integer> const& rhs) {
  return make<integer>(ctx, lhs->value() - rhs->value());
}

generic_ptr
subtract(context& ctx, std::vector<generic_ptr> const& xs) {
  return arithmetic<static_cast<primitive_arithmetic_type*>(&subtract)>(ctx, xs, false, 0);
}

ptr<integer>
multiply(context& ctx, ptr<integer> const& lhs, ptr<integer> const& rhs) {
  return make<integer>(ctx, lhs->value() * rhs->value());
}

generic_ptr
multiply(context& ctx, std::vector<generic_ptr> const& xs) {
  return arithmetic<static_cast<primitive_arithmetic_type*>(&multiply)>(ctx, xs, true, 1);
}

ptr<integer>
divide(context& ctx, ptr<integer> const& lhs, ptr<integer> const& rhs) {
  if (rhs->value() == 0)
    throw std::runtime_error{"Divide by zero"};
  else
    return make<integer>(ctx, lhs->value() / rhs->value());
}

generic_ptr
divide(context& ctx, std::vector<generic_ptr> const& xs) {
  return arithmetic<static_cast<primitive_arithmetic_type*>(&divide)>(ctx, xs, false, 1);
}

using primitive_relational_type = ptr<boolean>(context&, ptr<integer> const&, ptr<integer> const&);

template <primitive_relational_type* F>
generic_ptr
relational(context& ctx, std::vector<generic_ptr> const& xs, std::string const& name) {
  if (xs.size() < 2)
    throw std::runtime_error{fmt::format("Not enough arguments to {}", name)};

  ptr<integer> lhs = expect<integer>(xs[0]);
  for (std::size_t i = 1; i < xs.size(); ++i) {
    ptr<integer> rhs = expect<integer>(xs[i]);
    if (F(ctx, lhs, rhs) == ctx.constants.f)
      return ctx.constants.f;

    lhs = rhs;
  }

  return ctx.constants.t;
}

ptr<boolean>
arith_equal(context& ctx, ptr<integer> const& lhs, ptr<integer> const& rhs) {
  return lhs->value() == rhs->value() ? ctx.constants.t : ctx.constants.f;
}

generic_ptr
arith_equal(context& ctx, std::vector<generic_ptr> const& xs) {
  return relational<arith_equal>(ctx, xs, "=");
}

ptr<boolean>
less(context& ctx, ptr<integer> const& lhs, ptr<integer> const& rhs) {
  return lhs->value() < rhs->value() ? ctx.constants.t : ctx.constants.f;
}

generic_ptr
less(context& ctx, std::vector<generic_ptr> const& xs) {
  return relational<less>(ctx, xs, "<");
}

ptr<boolean>
greater(context& ctx, ptr<integer> const& lhs, ptr<integer> const& rhs) {
  return lhs->value() > rhs->value() ? ctx.constants.t : ctx.constants.f;
}

generic_ptr
greater(context& ctx, std::vector<generic_ptr> const& xs) {
  return relational<greater>(ctx, xs, ">");
}

void
string::set(std::size_t i, char c) {
  assert(i < size_);
  dynamic_storage()[i] = c;
}

std::string
string::value() const {
  std::string result;
  result.reserve(size_);

  for (std::size_t i = 0; i < size_; ++i)
    result += dynamic_storage()[i];

  return result;
}

ptr<string>
make_string(context& ctx, std::string const& value) {
  auto result = make<string>(ctx, value.size());

  for (std::size_t i = 0; i < value.size(); ++i)
    result->set(i, value[i]);

  return result;
}

port::port(FILE* f, bool input, bool output, bool should_close)
  : buffer_{f}
  , input_{input}
  , output_{output}
  , should_close_{should_close}
{ }

port::port(std::string buffer, bool input, bool output)
  : buffer_{string_buffer{std::move(buffer)}}
  , input_{input}
  , output_{output}
{ }

void
port::write_string(std::string const& s) {
  if (!output_)
    throw std::runtime_error{"Writing to non-writeable port"};

  if (FILE** f = std::get_if<FILE*>(&buffer_))
    std::fputs(s.c_str(), *f);
  else
    std::get<string_buffer>(buffer_).data += s;
}

void
port::write_char(char c) {
  if (!output_)
    throw std::runtime_error{"Writing to non-writeable port"};

  if (FILE** f = std::get_if<FILE*>(&buffer_))
    std::fputc(c, *f);
  else
    std::get<string_buffer>(buffer_).data += c;
}

std::optional<char>
port::peek_char() {
  if (FILE** f = std::get_if<FILE*>(&buffer_)) {
    int c = std::getc(*f);
    if (c == EOF)
      return {};

    std::ungetc(c, *f);
    return c;
  }
  else {
    string_buffer const& buf = std::get<string_buffer>(buffer_);
    if (buf.read_index == buf.data.size())
      return {};
    else
      return buf.data[buf.read_index];
  }
}

std::optional<char>
port::read_char() {
  if (FILE** f = std::get_if<FILE*>(&buffer_)) {
    int c = std::getc(*f);
    if (c == EOF)
      return {};
    else
      return c;
  }
  else {
    string_buffer& buf = std::get<string_buffer>(buffer_);
    if (buf.read_index == buf.data.size())
      return {};
    else
      return buf.data[buf.read_index++];
  }
}

std::string
port::get_string() const {
  if (string_buffer const* sb = std::get_if<string_buffer>(&buffer_))
    return sb->data;
  else
    throw std::runtime_error{"Not a string port"};
}

std::size_t
pair::hash() const {
  return 3 * subobjects_[0]->hash() ^ subobjects_[1]->hash();
}

bool
is_list(context& ctx, generic_ptr x) {
  while (true)
    if (x == ctx.constants.null)
      return true;
    else if (ptr<pair> p = match<pair>(x))
      x = cdr(p);
    else
      return false;
}

std::size_t
list_length(context& ctx, generic_ptr x) {
  std::size_t result = 0;
  while (x != ctx.constants.null) {
    x = cdr(expect<pair>(x));
    ++result;
  }
  return result;
}

generic_ptr
cadr(ptr<pair> const& x) {
  return car(expect<pair>(cdr(x)));
}

generic_ptr
caddr(ptr<pair> const& x) {
  return car(expect<pair>(cddr(x)));
}

generic_ptr
cadddr(ptr<pair> const& x) {
  return car(expect<pair>(cdddr(x)));
}

generic_ptr
cddr(ptr<pair> const& x) {
  return cdr(expect<pair>(cdr(x)));
}

generic_ptr
cdddr(ptr<pair> const& x) {
  return cdr(expect<pair>(cddr(x)));
}

generic_ptr
append(context& ctx, std::vector<generic_ptr> const& xs) {
  // If all the lists are empty, we return the empty list as well.

  auto x = xs.begin();
  while (x != xs.end() && *x == ctx.constants.null)
    ++x;

  if (x == xs.end())
    return ctx.constants.null;

  if (x == xs.end() - 1)
    return *x;

  // We have more than one list, and at least the current list is non-empty. Do
  // the needful.

  generic_ptr new_head = ctx.constants.null;
  ptr<pair> new_tail;
  generic_ptr current = expect<pair>(*x);
  for (; x != xs.end() - 1; ++x) {
    current = *x;

    while (current != ctx.constants.null) {
      ptr<pair> c = expect<pair>(current);
      ptr<pair> new_c = make<pair>(ctx, car(c), ctx.constants.null);

      if (new_tail)
        new_tail->set_cdr(new_c);
      else
        new_head = new_c;

      new_tail = new_c;
      current = cdr(c);
    }
  }

  assert(x == xs.end() - 1);
  if (new_tail)
    new_tail->set_cdr(*x);
  else
    new_head = *x;

  return new_head;
}

vector::vector(std::size_t size)
  : size_{size}
{
  for (std::size_t i = 0; i < size_; ++i)
    dynamic_storage()[i] = nullptr;
}

void
vector::for_each_subobject(std::function<void(object*)> const& f) {
  for (std::size_t i = 0; i < size_; ++i)
    f(dynamic_storage()[i]);
}

generic_ptr
vector::ref(free_store& store, std::size_t i) const {
  if (i >= size_)
    throw std::runtime_error{fmt::format("Vector access out of bounds: index = {}, size = {}", i, size_)};

  return {store, dynamic_storage()[i]};
}

void
vector::set(std::size_t i, generic_ptr value) {
  if (i >= size_)
    throw std::runtime_error{fmt::format("Vector access out of bounds: index = {}, size = {}", i, size_)};

  dynamic_storage()[i] = value.get();
}

std::size_t
vector::hash() const {
  std::size_t result = 0;
  for (std::size_t i = 0; i < size_; ++i)
    result = 3 * result ^ dynamic_storage()[i]->hash();

  return result;
}

ptr<vector>
make_vector(context& ctx, std::vector<generic_ptr> const& elems) {
  auto result = make<vector>(ctx, elems.size());
  for (std::size_t i = 0; i < elems.size(); ++i)
    result->set(i, elems[i]);

  return result;
}

ptr<vector>
list_to_vector(context& ctx, generic_ptr const& lst) {
  std::size_t size = 0;
  for (generic_ptr e = lst; e != ctx.constants.null; e = cdr(expect<pair>(e)))
    ++size;

  auto result = make<vector>(ctx, size);
  std::size_t i = 0;
  for (generic_ptr e = lst; e != ctx.constants.null; e = cdr(assume<pair>(e)))
    result->set(i++, car(assume<pair>(e)));

  return result;
}

ptr<vector>
vector_append(context& ctx, std::vector<generic_ptr> const& vs) {
  std::size_t size = 0;
  for (generic_ptr const& e : vs) {
    ptr<vector> v = expect<vector>(e);
    size += v->size();
  }

  auto result = make<vector>(ctx, size);
  std::size_t i = 0;
  for (generic_ptr const& e : vs) {
    ptr<vector> v = assume<vector>(e);
    for (std::size_t j = 0; j < v->size(); ++j)
      result->set(i++, vector_ref(v, j));
  }

  return result;
}

box::box(generic_ptr const& value)
  : compound_object{{value.get()}}
{ }

generic_ptr
box::get(free_store& store) const {
  return {store, subobjects_[0]};
}

void
box::set(generic_ptr const& value) {
  subobjects_[0] = value.get();
}

procedure::procedure(scm::bytecode bc, unsigned locals_size, unsigned num_args)
  : bytecode(std::move(bc))
  , locals_size{locals_size}
  , num_args{num_args}
{ }

closure::closure(ptr<scm::procedure> const& p, std::vector<generic_ptr> const& captures)
  : procedure_{p.get()}
  , size_{captures.size()}
{
  for (std::size_t i = 0; i < size_; ++i)
    dynamic_storage()[i] = captures[i].get();
}

generic_ptr
closure::ref(free_store& store, std::size_t i) const {
  assert(i < size_);
  return {store, dynamic_storage()[i]};
}

void
closure::for_each_subobject(std::function<void(object*)> const& f) {
  f(procedure_);
  for (std::size_t i = 0; i < size_; ++i)
    f(dynamic_storage()[i]);
}

} // namespace scm
