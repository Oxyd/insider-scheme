#include "scheme.hpp"

#include <fmt/format.h>

#include <algorithm>
#include <cassert>

namespace game::scm {

bool
object::eqv(generic_ptr const& other) const {
  return this == other.get();
}

generic_ptr::generic_ptr(object* value)
  : generic_ptr(thread_context().store, value)
{ }

generic_ptr::generic_ptr(free_store& store, object* value)
  : generic_ptr_base{value}
{
  store.register_root(this);
}

generic_ptr::generic_ptr(generic_ptr const& other)
  : generic_ptr(other.value_)
{ }

generic_ptr::~generic_ptr() {
  thread_context().store.unregister_root(this);
}

generic_weak_ptr::generic_weak_ptr(object* value)
  : generic_weak_ptr(thread_context().store, value)
{ }

generic_weak_ptr::generic_weak_ptr(free_store& store, object* value)
  : generic_ptr_base{value}
{
  if (value)
    store.register_weak(this, value);
}

generic_weak_ptr::generic_weak_ptr(generic_weak_ptr const& other)
  : generic_weak_ptr{other.value_}
{ }

generic_weak_ptr::~generic_weak_ptr() {
  if (value_)
    thread_context().store.unregister_weak(this, value_);
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
    thread_context().store.unregister_weak(this, value_);

  value_ = value;

  if (value_)
    thread_context().store.register_weak(this, value_);

  return *this;
}

generic_ptr
generic_weak_ptr::lock() const {
  return {value_};
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

static void
export_native(context& ctx, ptr<module> const& m, std::string const& name,
              generic_ptr (*f)(std::vector<generic_ptr> const&), module::binding_tag tag) {
  m->objects.emplace_back(ctx.store.make<native_procedure>(f).get());
  m->bindings.push_back({m->objects.size() - 1, tag, m.get()});
  m->exports.emplace(ctx.intern(name).get(), m->bindings.size() - 1);
}

static ptr<module>
make_internal_module(context& ctx) {
  ptr<module> result = ctx.store.make<module>();
  export_native(ctx, result, "+", add, module::binding_tag::plus);
  export_native(ctx, result, "-", subtract, module::binding_tag::minus);
  export_native(ctx, result, "*", multiply, module::binding_tag::times);
  export_native(ctx, result, "/", divide, module::binding_tag::divide);
  export_native(ctx, result, "=", arith_equal, module::binding_tag::arith_equal);
  export_native(ctx, result, "<", less, module::binding_tag::less_than);
  export_native(ctx, result, ">", greater, module::binding_tag::greater_than);
  return result;
}

void
context::init() {
  constants = std::make_unique<struct context::constants>();
  constants->null = store.make<null_type>();
  constants->void_ = store.make<void_type>();
  constants->t = store.make<boolean>(true);
  constants->f = store.make<boolean>(false);
  constants->internal = make_internal_module(*this);

  statics.null = operand::static_(intern_static(constants->null));
  statics.void_ = operand::static_(intern_static(constants->void_));
  statics.t = operand::static_(intern_static(constants->t));
  statics.f = operand::static_(intern_static(constants->f));
  statics.zero = operand::static_(intern_static(store.make<integer>(0)));
  statics.one = operand::static_(intern_static(store.make<integer>(1)));
}

void
context::deinit() {
  constants.reset();
  interned_symbols_.clear();
  statics_.clear();
  statics_cache_.clear();
}

thread_local std::unique_ptr<context> thread_context_value;

void
make_context() {
  assert(!thread_context_value);
  thread_context_value = std::unique_ptr<context>(new context);
  thread_context_value->init();
}

void
destroy_context() {
  assert(thread_context_value);
  thread_context_value->deinit();
  thread_context_value.reset();
}

context&
thread_context() {
  assert(thread_context_value);
  return *thread_context_value;
}

bool
integer::eqv(generic_ptr const& other) const {
  if (auto y = match<integer>(other))
    return value() == y->value();
  else
    return false;
}

ptr<integer>
add(ptr<integer> const& lhs, ptr<integer> const& rhs) {
  return make<integer>(lhs->value() + rhs->value());
}

template <auto F>
generic_ptr
arithmetic(std::vector<generic_ptr> const& xs, bool allow_empty, integer::value_type neutral) {
  if (xs.empty()) {
    if (allow_empty)
      return make<integer>(neutral);
    else
      throw std::runtime_error{"Not enough arguments"};
  }
  else if (xs.size() == 1)
    return F(make<integer>(neutral), expect<integer>(xs.front()));
  else {
    ptr<integer> result = expect<integer>(xs.front());
    for (auto rhs = xs.begin() + 1; rhs != xs.end(); ++rhs)
      result = F(result, expect<integer>(*rhs));

    return result;
  }
}

using primitive_arithmetic_type = ptr<integer>(ptr<integer> const&, ptr<integer> const&);

generic_ptr
add(std::vector<generic_ptr> const& xs) {
  return arithmetic<static_cast<primitive_arithmetic_type*>(&add)>(xs, true, 0);
}

ptr<integer>
subtract(ptr<integer> const& lhs, ptr<integer> const& rhs) {
  return make<integer>(lhs->value() - rhs->value());
}

generic_ptr
subtract(std::vector<generic_ptr> const& xs) {
  return arithmetic<static_cast<primitive_arithmetic_type*>(&subtract)>(xs, false, 0);
}

ptr<integer>
multiply(ptr<integer> const& lhs, ptr<integer> const& rhs) {
  return make<integer>(lhs->value() * rhs->value());
}

generic_ptr
multiply(std::vector<generic_ptr> const& xs) {
  return arithmetic<static_cast<primitive_arithmetic_type*>(&multiply)>(xs, true, 1);
}

ptr<integer>
divide(ptr<integer> const& lhs, ptr<integer> const& rhs) {
  if (rhs->value() == 0)
    throw std::runtime_error{"Divide by zero"};
  else
    return make<integer>(lhs->value() / rhs->value());
}

generic_ptr
divide(std::vector<generic_ptr> const& xs) {
  return arithmetic<static_cast<primitive_arithmetic_type*>(&divide)>(xs, false, 1);
}

using primitive_relational_type = ptr<boolean>(ptr<integer> const&, ptr<integer> const&);

template <primitive_relational_type* F>
generic_ptr
relational(std::vector<generic_ptr> const& xs, std::string const& name) {
  if (xs.size() < 2)
    throw std::runtime_error{fmt::format("Not enough arguments to {}", name)};

  ptr<integer> lhs = expect<integer>(xs[0]);
  for (std::size_t i = 1; i < xs.size(); ++i) {
    ptr<integer> rhs = expect<integer>(xs[i]);
    if (F(lhs, rhs) == thread_context().constants->f)
      return thread_context().constants->f;

    lhs = rhs;
  }

  return thread_context().constants->t;
}

ptr<boolean>
arith_equal(ptr<integer> const& lhs, ptr<integer> const& rhs) {
  return lhs->value() == rhs->value() ? thread_context().constants->t : thread_context().constants->f;
}

generic_ptr
arith_equal(std::vector<generic_ptr> const& xs) {
  return relational<arith_equal>(xs, "=");
}

ptr<boolean>
less(ptr<integer> const& lhs, ptr<integer> const& rhs) {
  return lhs->value() < rhs->value() ? thread_context().constants->t : thread_context().constants->f;
}

generic_ptr
less(std::vector<generic_ptr> const& xs) {
  return relational<less>(xs, "<");
}

ptr<boolean>
greater(ptr<integer> const& lhs, ptr<integer> const& rhs) {
  return lhs->value() > rhs->value() ? thread_context().constants->t : thread_context().constants->f;
}

generic_ptr
greater(std::vector<generic_ptr> const& xs) {
  return relational<greater>(xs, ">");
}

std::size_t
pair::hash() const {
  return 3 * car()->hash() ^ cdr()->hash();
}

bool
is_list(generic_ptr x) {
  while (true)
    if (x == null())
      return true;
    else if (ptr<pair> p = match<pair>(x))
      x = p->cdr();
    else
      return false;
}

std::size_t
list_length(generic_ptr x) {
  std::size_t result = 0;
  while (x != null()) {
    x = expect<pair>(x)->cdr();
    ++result;
  }
  return result;
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
vector::ref(std::size_t i) const {
  if (i >= size_)
    throw std::runtime_error{fmt::format("Vector access out of bounds: index = {}, size = {}", i, size_)};

  return dynamic_storage()[i];
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
    result = 3 * result ^ ref(i)->hash();

  return result;
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
closure::ref(std::size_t i) const {
  assert(i < size_);
  return dynamic_storage()[i];
}

void
closure::for_each_subobject(std::function<void(object*)> const& f) {
  f(procedure_);
  for (std::size_t i = 0; i < size_; ++i)
    f(dynamic_storage()[i]);
}

void
module::for_each_subobject(std::function<void(object*)> const& f) {
  for (object* o : objects)
    f(o);

  for (binding const& b : bindings)
    f(b.parent);

  for (auto [symbol, index] : symbols)
    f(symbol);

  for (auto [symbol, index] : exports)
    f(symbol);

  for (auto [symbol, index] : imports)
    f(symbol);
}

void
import_all(ptr<module> const& dest, ptr<module> const& source) {
  for (auto const& [name, ex] : source->exports) {
    if (dest->imports.count(name))
      throw std::runtime_error{fmt::format("Name {} already imported", name->value())};

    dest->bindings.push_back(source->bindings[ex]);
    dest->imports.emplace(name, dest->bindings.size() - 1);
  }
}

} // namespace scm
