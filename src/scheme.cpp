#include "scheme.hpp"

#include "analyser.hpp"
#include "compiler.hpp"
#include "converters.hpp"
#include "io.hpp"

#include <fmt/format.h>

#include <algorithm>
#include <cassert>
#include <cstdio>

namespace scm {

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

auto
module::find(std::string const& name) const -> std::optional<index_type> {
  if (auto it = env_->bindings.find(name); it != env_->bindings.end())
    return it->second->global;

  return {};
}

void
module::add(std::string name, index_type i) {
  assert(!env_->bindings.count(name));
  env_->bindings.emplace(name, std::make_shared<variable>(name, i));
}

void
module::export_(std::string name) {
  if (!env_->bindings.count(name))
    throw std::runtime_error{fmt::format("Can't export undefined symbol {}", name)};

  exports_.emplace(std::move(name));
}

void
import_all(module& to, module const& from) {
  for (auto const& name : from.exports())
    to.add(name, *from.find(name));
}

namespace {
  struct import_set {
    module* source;
    std::vector<std::tuple<std::string, std::string>> names;
  };
}

static import_set
parse_import_set(context& ctx, import_specifier const& spec) {
  if (auto* mn = std::get_if<module_name>(&spec.value)) {
    import_set result;
    result.source = ctx.find_module(*mn);

    for (std::string const& name : result.source->exports())
      result.names.push_back(std::tuple{name, name});

    return result;
  }
  else if (auto* o = std::get_if<import_specifier::only>(&spec.value)) {
    import_set result = parse_import_set(ctx, *o->from);
    result.names.erase(std::remove_if(result.names.begin(), result.names.end(),
                                      [&] (auto const& name) {
                                        return std::find(o->identifiers.begin(), o->identifiers.end(),
                                                         std::get<0>(name)) == o->identifiers.end();
                                      }),
                       result.names.end());
    return result;
  }
  else if (auto* e = std::get_if<import_specifier::except>(&spec.value)) {
    import_set result = parse_import_set(ctx, *e->from);
    result.names.erase(std::remove_if(result.names.begin(), result.names.end(),
                                      [&] (auto const& name) {
                                        return std::find(e->identifiers.begin(), e->identifiers.end(),
                                                         std::get<0>(name)) != e->identifiers.end();
                                      }),
                       result.names.end());
    return result;
  }
  else if (auto* p = std::get_if<import_specifier::prefix>(&spec.value)) {
    import_set result = parse_import_set(ctx, *p->from);
    for (auto& [target, source] : result.names)
      target = p->prefix + target;
    return result;
  }
  else if (auto* r = std::get_if<import_specifier::rename>(&spec.value)) {
    import_set result = parse_import_set(ctx, *r->from);

    for (auto& [target, source] : result.names) {
      for (auto const& [rename_from, rename_to] : r->renames) {
        if (source == rename_from) {
          target = rename_to;
          break;
        }
      }
    }

    return result;
  }
  else
    assert(!"Can't happen");
}

void
perform_imports(context& ctx, module& m, protomodule const& pm) {
  for (import_specifier const& spec : pm.imports) {
    import_set set = parse_import_set(ctx, spec);

    for (auto const& [to_name, from_name] : set.names)
      m.add(to_name, *set.source->find(from_name));

    if (!set.source->active())
      execute(ctx, *set.source);
  }
}

void
define_top_level(context& ctx, module& m, std::string const& name, generic_ptr const& object,
                 bool export_) {
  auto index = ctx.add_top_level(object);
  m.add(name, index);
  if (export_)
    m.export_(name);
}

static generic_ptr
run_module(context& ctx, module& m) {
  auto state = make_state(ctx, m.top_level_procedure());
  return run(state);
}

generic_ptr
execute(context& ctx, module& mod) {
  generic_ptr result = run_module(ctx, mod);
  mod.mark_active();

  return result;
}

std::optional<std::vector<generic_ptr>>
filesystem_module_provider::find_module(context& ctx, module_name const& name) {
  std::filesystem::path p = root_;
  for (std::string const& element : name)
    p /= element;

  std::vector<std::filesystem::path> candidates{p.replace_extension(".sld"),
                                                p.replace_extension(".scm")};
  for (auto const& candidate : candidates) {
    FILE* f = std::fopen(candidate.c_str(), "r");
    if (f) {
      auto in = make<port>(ctx, f, true, false);
      std::optional<module_name> candidate_name = read_library_name(ctx, in);
      if (candidate_name == name) {
        in->rewind();
        return read_multiple(ctx, in);
      }
    }
  }

  return std::nullopt;
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
  result.mark_active();

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
      write_simple(ctx, datum, ctx.output_port);
    }
  );

  define_lambda<void(context&)>(
    ctx, result, "newline", true,
    [] (context& ctx) { ctx.output_port->write_char('\n'); }
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

  define_lambda<ptr<syntactic_closure>(context&, ptr<environment_holder> const&,
                                       generic_ptr const&, generic_ptr const&)>(
    ctx, result, "make-syntactic-closure", true,
    [] (context& ctx, ptr<environment_holder> const& env, generic_ptr const& free, generic_ptr const& form) {
      return make<syntactic_closure>(ctx, env->value, form, free);
    }
  );

  return result;
}

context::context() {
  constants = std::make_unique<struct constants>();
  constants->null = store.make<null_type>();
  constants->void_ = store.make<void_type>();
  constants->t = store.make<boolean>(true);
  constants->f = store.make<boolean>(false);

  internal_module = make_internal_module(*this);

  struct {
    ptr<core_form_type>& object;
    std::string          name;
  } core_forms[]{
    {constants->let,              "let"},
    {constants->set,              "set!"},
    {constants->lambda,           "lambda"},
    {constants->if_,              "if"},
    {constants->box,              "box"},
    {constants->unbox,            "unbox"},
    {constants->box_set,          "box-set!"},
    {constants->define,           "define"},
    {constants->define_syntax,    "define-syntax"},
    {constants->begin,            "begin"},
    {constants->quote,            "quote"},
    {constants->quasiquote,       "quasiquote"},
    {constants->unquote,          "unquote"},
    {constants->unquote_splicing, "unquote-splicing"}
  };
  for (auto const& form : core_forms) {
    form.object = store.make<core_form_type>();
    auto index = add_top_level(form.object);
    internal_module.add(form.name, index);
    internal_module.export_(form.name);
  }

  statics.null = operand::static_(intern_static(constants->null));
  statics.void_ = operand::static_(intern_static(constants->void_));
  statics.t = operand::static_(intern_static(constants->t));
  statics.f = operand::static_(intern_static(constants->f));
  statics.zero = operand::static_(intern_static(store.make<integer>(0)));
  statics.one = operand::static_(intern_static(store.make<integer>(1)));

  output_port = make<port>(*this, stdout, false, true, false);
}

context::~context() {
  constants.reset();
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

static std::string
module_name_to_string(module_name const& name) {
  std::string result = "(";
  for (auto it = name.begin(); it != name.end(); ++it) {
    if (it != name.begin())
      result += " ";
    result += *it;
  }
  result += ")";

  return result;
}

void
context::load_library_module(std::vector<generic_ptr> const& data) {
  protomodule pm = read_library(data);
  assert(pm.name);

  module_name name = *pm.name;
  bool inserted = protomodules_.emplace(name, std::move(pm)).second;

  if (!inserted)
    throw std::runtime_error{fmt::format("Module {} already loaded", module_name_to_string(*pm.name))};
}

static std::unique_ptr<module>
instantiate(context& ctx, protomodule const& pm) {
  auto result = std::make_unique<module>();

  perform_imports(ctx, *result, pm);
  compile_module_body(ctx, *result, pm.body);

  for (std::string const& name : pm.exports)
    result->export_(name);

  return result;
}

module*
context::find_module(module_name const& name) {
  using namespace std::literals;

  if (name == std::vector{"insider"s, "internal"s})
    return &internal_module;

  auto mod_it = modules_.find(name);
  if (mod_it != modules_.end())
    return mod_it->second.get();

  auto pm = protomodules_.find(name);
  if (pm == protomodules_.end()) {
    for (std::unique_ptr<module_provider> const& provider : module_providers_)
      if (std::optional<std::vector<generic_ptr>> lib = provider->find_module(*this, name)) {
        load_library_module(*lib);

        pm = protomodules_.find(name);
        assert(pm != protomodules_.end());

        break;
      }

    if (pm == protomodules_.end())
      throw std::runtime_error{fmt::format("Unknown module {}", module_name_to_string(name))};
  }

  std::unique_ptr<module> m = instantiate(*this, pm->second);
  protomodules_.erase(pm);

  mod_it = modules_.emplace(name, std::move(m)).first;
  return mod_it->second.get();
}

void
context::prepend_module_provider(std::unique_ptr<module_provider> provider) {
  module_providers_.insert(module_providers_.begin(), std::move(provider));
}

void
context::append_module_provider(std::unique_ptr<module_provider> provider) {
  module_providers_.push_back(std::move(provider));
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
    if (F(ctx, lhs, rhs) == ctx.constants->f)
      return ctx.constants->f;

    lhs = rhs;
  }

  return ctx.constants->t;
}

ptr<boolean>
arith_equal(context& ctx, ptr<integer> const& lhs, ptr<integer> const& rhs) {
  return lhs->value() == rhs->value() ? ctx.constants->t : ctx.constants->f;
}

generic_ptr
arith_equal(context& ctx, std::vector<generic_ptr> const& xs) {
  return relational<arith_equal>(ctx, xs, "=");
}

ptr<boolean>
less(context& ctx, ptr<integer> const& lhs, ptr<integer> const& rhs) {
  return lhs->value() < rhs->value() ? ctx.constants->t : ctx.constants->f;
}

generic_ptr
less(context& ctx, std::vector<generic_ptr> const& xs) {
  return relational<less>(ctx, xs, "<");
}

ptr<boolean>
greater(context& ctx, ptr<integer> const& lhs, ptr<integer> const& rhs) {
  return lhs->value() > rhs->value() ? ctx.constants->t : ctx.constants->f;
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

void
port::rewind() {
  if (string_buffer* sb = std::get_if<string_buffer>(&buffer_))
    sb->read_index = 0;
  else
    std::rewind(std::get<FILE*>(buffer_));
}

std::size_t
pair::hash() const {
  return 3 * subobjects_[0]->hash() ^ subobjects_[1]->hash();
}

bool
is_list(generic_ptr x) {
  while (true)
    if (is<null_type>(x))
      return true;
    else if (ptr<pair> p = match<pair>(x))
      x = cdr(p);
    else
      return false;
}

std::size_t
list_length(generic_ptr x) {
  std::size_t result = 0;
  while (!is<null_type>(x)) {
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
  while (x != xs.end() && *x == ctx.constants->null)
    ++x;

  if (x == xs.end())
    return ctx.constants->null;

  if (x == xs.end() - 1)
    return *x;

  // We have more than one list, and at least the current list is non-empty. Do
  // the needful.

  generic_ptr new_head = ctx.constants->null;
  ptr<pair> new_tail;
  generic_ptr current = expect<pair>(*x);
  for (; x != xs.end() - 1; ++x) {
    current = *x;

    while (current != ctx.constants->null) {
      ptr<pair> c = expect<pair>(current);
      ptr<pair> new_c = make<pair>(ctx, car(c), ctx.constants->null);

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
  for (generic_ptr e = lst; e != ctx.constants->null; e = cdr(expect<pair>(e)))
    ++size;

  auto result = make<vector>(ctx, size);
  std::size_t i = 0;
  for (generic_ptr e = lst; e != ctx.constants->null; e = cdr(assume<pair>(e)))
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

std::size_t
syntactic_closure::extra_storage_size(std::shared_ptr<scm::environment>,
                                      generic_ptr const& expr, generic_ptr const& free) {
  return list_length(free) * sizeof(object*);
}

syntactic_closure::syntactic_closure(std::shared_ptr<scm::environment> env,
                                     generic_ptr const& expr, generic_ptr const& free)
  : environment{std::move(env)}
  , expression_{expr.get()}
  , free_size_{0}
{
  for (generic_ptr name : in_list{free}) {
    expect<symbol>(name);
    dynamic_storage()[free_size_++] = name.get();
  }
}

std::vector<ptr<symbol>>
syntactic_closure::free(free_store& store) const {
  std::vector<ptr<symbol>> result;
  result.reserve(free_size_);

  for (std::size_t i = 0; i < free_size_; ++i)
    result.push_back(ptr<symbol>{store, dynamic_storage()[i]});

  return result;
}

void
syntactic_closure::for_each_subobject(std::function<void(object*)> const& f) {
  f(expression_);
  for (std::size_t i = 0; i < free_size_; ++i)
    f(dynamic_storage()[i]);
}

} // namespace scm
