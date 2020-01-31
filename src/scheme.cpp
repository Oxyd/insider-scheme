#include "scheme.hpp"

#include <fmt/format.h>

#include <algorithm>
#include <cassert>

namespace game::scm {

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

static void
export_native(context& ctx, ptr<module> const& m, std::string const& name,
              generic_ptr (*f)(context&, std::vector<generic_ptr> const&), special_top_level_tag tag) {
  auto index = ctx.add_top_level(ctx.store.make<native_procedure>(f));
  ctx.tag_top_level(index, tag);
  m->add(name, index);
  m->export_(name);
}

static ptr<module>
make_internal_module(context& ctx) {
  ptr<module> result = ctx.store.make<module>();
  export_native(ctx, result, "+", add, special_top_level_tag::plus);
  export_native(ctx, result, "-", subtract, special_top_level_tag::minus);
  export_native(ctx, result, "*", multiply, special_top_level_tag::times);
  export_native(ctx, result, "/", divide, special_top_level_tag::divide);
  export_native(ctx, result, "=", arith_equal, special_top_level_tag::arith_equal);
  export_native(ctx, result, "<", less, special_top_level_tag::less_than);
  export_native(ctx, result, ">", greater, special_top_level_tag::greater_than);
  return result;
}

context::context() {
  constants.null = store.make<null_type>();
  constants.void_ = store.make<void_type>();
  constants.t = store.make<boolean>(true);
  constants.f = store.make<boolean>(false);
  constants.internal = make_internal_module(*this);

  statics.null = operand::static_(intern_static(constants.null));
  statics.void_ = operand::static_(intern_static(constants.void_));
  statics.t = operand::static_(intern_static(constants.t));
  statics.f = operand::static_(intern_static(constants.f));
  statics.zero = operand::static_(intern_static(store.make<integer>(0)));
  statics.one = operand::static_(intern_static(store.make<integer>(1)));
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
module::for_each_subobject(std::function<void(object*)> const& f) {
  f(proc_);
}

void
import_all(ptr<module> const& to, ptr<module> const& from) {
  for (auto const& name : from->exports())
    to->import(name, *from->find(name));
}

} // namespace scm
