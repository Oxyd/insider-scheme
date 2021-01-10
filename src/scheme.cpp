#include "scheme.hpp"

#include "action.hpp"
#include "analyser.hpp"
#include "compiler.hpp"
#include "internal_module.hpp"
#include "numeric.hpp"
#include "vm.hpp"

#include <fmt/format.h>

#include <algorithm>
#include <cassert>
#include <cstdio>
#include <functional>
#include <typeinfo>

#ifdef WIN32
#include <tchar.h>
#else
#define _T(x) x
#endif

namespace insider {

std::size_t
hash(object* x) {
  if (auto i = match<integer>(x))
    return integer_hash(*i);
  else
    return object_type(x).hash(x);
}

bool
eqv(context& ctx, object* x, object* y) {
  if (x == y)
    return true;

  if (!is_object_ptr(x) || !is_object_ptr(y))
    return false; // Either both are fixnums and not the same, or they're different types.

  if (is_number(x) && is_number(y) && is_exact(x) == is_exact(y))
    return arith_equal(ctx, x, y);

  if (object_type_index(x) != object_type_index(y))
    return false;

  if (auto lhs = match<character>(x))
    return lhs->value() == assume<character>(x)->value();

  return false;
}

bool
equal(context& ctx, object* x, object* y) {
  // XXX: This will break on infinite data structures.

  struct record {
    object* left;
    object* right;
  };

  std::vector<record> stack{{x, y}};
  while (!stack.empty()) {
    record top = stack.back();
    stack.pop_back();

    if (!eqv(ctx, top.left, top.right)) {
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
          stack.push_back({l->ref(i - 1), r->ref(i - 1)});
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

bool
is_identifier(object* x) {
  if (is<symbol>(x))
    return true;
  else if (auto sc = match<syntactic_closure>(x))
    return is<symbol>(sc->expression());
  else
    return false;
}

std::string
identifier_name(object* x) {
  if (auto s = match<symbol>(x))
    return s->value();
  else
    return expect<symbol>(expect<syntactic_closure>(x)->expression())->value();
}

void
environment::add(free_store& store, object* identifier, std::shared_ptr<variable> var) {
  assert(is_identifier(identifier));

  bool inserted = bindings_.emplace(identifier, std::move(var)).second;
  if (!inserted)
    throw std::runtime_error{fmt::format("Redefinition of {}", identifier_name(identifier))};

  store.notify_arc(this, identifier);
}

void
environment::add(free_store& store, object* identifier, transformer* tr) {
  assert(is_identifier(identifier));

  bool inserted = bindings_.emplace(identifier, tr).second;
  if (!inserted)
    throw std::runtime_error{fmt::format("Redefinition of {}", identifier_name(identifier))};

  store.notify_arc(this, identifier);
  store.notify_arc(this, tr);
}

void
environment::add(free_store& store, object* identifier, value_type const& value) {
  if (auto var = std::get_if<std::shared_ptr<variable>>(&value))
    add(store, identifier, *var);
  else
    add(store, identifier, std::get<transformer*>(value));
}

auto
environment::lookup(object* identifier) const -> std::optional<value_type> {
  if (auto it = bindings_.find(identifier); it != bindings_.end()) {
    if (auto var = std::get_if<std::shared_ptr<variable>>(&it->second))
      return *var;
    else
      return std::get<transformer*>(it->second);
  }

  return std::nullopt;
}

std::vector<std::string>
environment::bound_names() const {
  std::vector<std::string> result;
  result.reserve(bindings_.size());

  for (auto const& [identifier, binding] : bindings_)
    result.push_back(identifier_name(identifier));

  return result;
}

void
environment::trace(tracing_context& tc) const {
  tc.trace(parent_);

  for (auto& [identifier, binding] : bindings_) {
    tc.trace(identifier);
    if (transformer* const* tr = std::get_if<transformer*>(&binding))
      tc.trace(*tr);
  }
}

void
environment::update_references() {
  update_reference(parent_);

  std::vector<std::tuple<object*, representation_type>> to_reinsert;
  for (auto it = bindings_.begin(); it != bindings_.end();) {
    if (transformer** tr = std::get_if<transformer*>(&it->second))
      update_reference(*tr);

    object* new_id = update_reference_copy(it->first);
    if (new_id != it->first) {
      to_reinsert.emplace_back(new_id, std::move(it->second));
      it = bindings_.erase(it);
    } else
      ++it;
  }

  for (auto& [id, value] : to_reinsert)
    bindings_.emplace(id, std::move(value));
}

std::size_t
environment::hash() const {
  return (parent_ ? insider::hash(parent_) : 0) ^ bindings_.size();
}

module::module(context& ctx)
  : env_{make_tracked<insider::environment>(ctx, nullptr)}
{ }

auto
module::find(object* identifier) const -> std::optional<binding_type> {
  if (auto binding = env_->lookup(identifier)) {
    if (std::shared_ptr<variable>* var = std::get_if<std::shared_ptr<variable>>(&*binding))
      return (**var).global;
    else
      return std::get<transformer*>(*binding);
  }

  return std::nullopt;
}

void
module::add(object* identifier, binding_type b) {
  assert(is_identifier(identifier));

  if (auto v = find(identifier)) {
    if (*v == b)
      return; // Re-importing the same variable under the same name is OK.
    else
      throw std::runtime_error{fmt::format("Redefinition of {}", identifier_name(identifier))};
  }

  if (auto* index = std::get_if<index_type>(&b))
    env_->add(env_.store(), identifier, std::make_shared<variable>(identifier_name(identifier), *index));
  else
    env_->add(env_.store(), identifier, std::get<transformer*>(b));
}

void
module::export_(symbol* name) {
  if (!env_->has(name))
    throw std::runtime_error{fmt::format("Can't export undefined symbol {}", name->value())};

  exports_.emplace(name->value());
}

namespace {
  struct import_set {
    module* source;
    std::vector<std::tuple<std::string, std::string>> names;
  };
}

static void
check_all_names_exist(std::vector<std::string> const& names, import_set const& set) {
  for (auto const& name : names)
    if (std::none_of(set.names.begin(), set.names.end(),
                     [&] (auto const& set_name) {
                       return std::get<0>(set_name) == name;
                     }))
      throw std::runtime_error{fmt::format("Identifier {} is not exported", name)};
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
    check_all_names_exist(o->identifiers, result);
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
    check_all_names_exist(e->identifiers, result);
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
      target = p->prefix_ + target;
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
  else {
    assert(!"Can't happen");
    return {};
  }
}

static void
perform_imports(context& ctx, module& m, import_set const& set) {
  for (auto const& [to_name, from_name] : set.names) {
    if (auto b = set.source->find(ctx.intern(from_name)))
      m.add(ctx.intern(to_name), *b);
    else
      assert(!"Trying to import a nonexistent symbol");
  }

  if (!set.source->active())
    execute(ctx, *set.source);
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

std::unique_ptr<module>
instantiate(context& ctx, protomodule const& pm) {
  auto result = std::make_unique<module>(ctx);

  perform_imports(ctx, *result, pm);
  compile_module_body(ctx, *result, pm);

  for (std::string const& name : pm.exports)
    result->export_(ctx.intern(name));

  return result;
}

void
import_all_exported(context& ctx, module& to, module& from) {
  import_set is{&from, {}};

  for (std::string const& name : from.exports())
    is.names.push_back(std::tuple{name, name});

  perform_imports(ctx, to, is);
}

void
import_all_top_level(context& ctx, module& to, module& from) {
  import_set is{&from, {}};

  for (std::string const& name : from.top_level_names())
    is.names.push_back(std::tuple{name, name});

  perform_imports(ctx, to, is);
}

void
perform_imports(context& ctx, module& m, protomodule const& pm) {
  for (import_specifier const& spec : pm.imports)
    perform_imports(ctx, m, parse_import_set(ctx, spec));
}

operand
define_top_level(context& ctx, std::string const& name, module& m, bool export_, object* object) {
  auto index = ctx.add_top_level(object, name);
  m.add(ctx.intern(name), index);
  if (export_)
    m.export_(ctx.intern(name));

  return index;
}

static generic_tracked_ptr
run_module(context& ctx, module& m) {
  return call(ctx, m.top_level_procedure(), {});
}

generic_tracked_ptr
execute(context& ctx, module& mod) {
  generic_tracked_ptr result = run_module(ctx, mod);
  mod.mark_active();

  return result;
}

static FILE*
open_file(std::filesystem::path const& path, std::filesystem::path::value_type const* mode) {
#ifndef WIN32
  return std::fopen(path.c_str(), mode);
#else
  return _wfopen(path.c_str(), mode);
#endif
}

std::optional<std::vector<generic_tracked_ptr>>
filesystem_module_provider::find_module(context& ctx, module_name const& name) {
  std::filesystem::path p = root_;
  for (std::string const& element : name)
    p /= element;

  std::vector<std::filesystem::path> candidates{p.replace_extension(".sld"),
                                                p.replace_extension(".scm")};
  for (auto const& candidate : candidates) {
    FILE* f = open_file(candidate.c_str(), _T("r"));
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

context::context()
  : internal_module{*this}
#ifdef INSIDER_VM_PROFILER
  , instruction_counts(instructions.size())
  , instruction_times(instructions.size())
#endif
  , statics_cache_{0, {}, eqv_compare{*this}}
{
  constants = std::make_unique<struct constants>();
  constants->null = make_tracked<null_type>(*this);
  constants->void_ = make_tracked<void_type>(*this);
  constants->t = make_tracked<boolean>(*this, true);
  constants->f = make_tracked<boolean>(*this, false);

  internal_module = make_internal_module(*this);

  struct {
    tracked_ptr<core_form_type>& object;
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
    {constants->begin_for_syntax, "begin-for-syntax"},
    {constants->quote,            "quote"},
    {constants->quasiquote,       "quasiquote"},
    {constants->unquote,          "unquote"},
    {constants->unquote_splicing, "unquote-splicing"},
    {constants->expand_quote,     "expand-quote"},
    {constants->syntax_trap,      "syntax-trap"},
    {constants->syntax_error,     "syntax-error"}
  };
  for (auto const& form : core_forms) {
    form.object = make_tracked<core_form_type>(*this);
    auto index = add_top_level(form.object.get(), form.name);
    auto id = intern(form.name);
    internal_module.add(id, index);
    internal_module.export_(id);
  }

  statics.null = intern_static(constants->null);
  statics.void_ = intern_static(constants->void_);
  statics.t = intern_static(constants->t);
  statics.f = intern_static(constants->f);
  statics.zero = intern_static(generic_tracked_ptr{store, integer_to_ptr(0)});
  statics.one = intern_static(generic_tracked_ptr{store, integer_to_ptr(1)});

  output_port = make_tracked<port>(*this, stdout, false, true, false);
}

context::~context() {
  constants.reset();
}

symbol*
context::intern(std::string const& s) {
  auto interned = interned_symbols_.find(s);
  if (interned != interned_symbols_.end()) {
    if (tracked_ptr<symbol> sym = interned->second.lock())
      return sym.get();
    else {
      tracked_ptr<symbol> result = make_tracked<symbol>(*this, s);
      interned->second = result;
      return result.get();
    }
  }

  tracked_ptr<symbol> result = make_tracked<symbol>(*this, s);
  interned_symbols_.emplace(s, weak_ptr<symbol>{result});

  return result.get();
}

operand
context::intern_static(generic_tracked_ptr const& x) {
  auto it = statics_cache_.find(x);
  if (it == statics_cache_.end()) {
    statics_.push_back(x);
    it = statics_cache_.emplace(x, statics_.size() - 1).first;
  }

  return it->second;
}

object*
context::get_static_checked(operand i) const {
  if (i >= statics_.size())
    throw std::runtime_error{fmt::format("Nonexistent static object {}", i)};

  return statics_[i].get();
}

object*
context::get_top_level_checked(operand i) const {
  if (i >= top_level_objects_.size())
    throw std::runtime_error{fmt::format("Nonexistent top-level object {}", i)};

  return top_level_objects_[i].get();
}

void
context::set_top_level(operand i, object* value) {
  assert(i < top_level_objects_.size());
  top_level_objects_[i] = {store, value};
}

operand
context::add_top_level(object* x, std::string name) {
  top_level_objects_.push_back({store, x});
  top_level_binding_names_.emplace_back(std::move(name));
  return top_level_objects_.size() - 1;
}

std::string
context::get_top_level_name(operand i) const {
  if (i < top_level_binding_names_.size())
    return top_level_binding_names_[i];
  else
    throw error{"Invalid global operand {}", i};
}

void
context::tag_top_level(operand i, special_top_level_tag tag) {
  top_level_tags_.emplace(i, tag);
}

std::optional<special_top_level_tag>
context::find_tag(operand i) const {
  if (auto it = top_level_tags_.find(i); it != top_level_tags_.end())
    return it->second;
  else
    return {};
}

void
context::load_library_module(std::vector<generic_tracked_ptr> const& data) {
  protomodule pm = read_library(*this, data);
  assert(pm.name);

  module_name name = *pm.name;
  bool inserted = protomodules_.emplace(name, std::move(pm)).second;

  if (!inserted)
    throw std::runtime_error{fmt::format("Module {} already loaded", module_name_to_string(*pm.name))};
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
      if (std::optional<std::vector<generic_tracked_ptr>> lib = provider->find_module(*this, name)) {
        load_library_module(*lib);

        pm = protomodules_.find(name);
        assert(pm != protomodules_.end());

        break;
      }

    if (pm == protomodules_.end())
      throw std::runtime_error{fmt::format("Unknown module {}", module_name_to_string(name))};
  }

  simple_action a(*this, "Analysing module {}", pm->second.name ? module_name_to_string(*pm->second.name) : "<unknown>");
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

string*
make_string(context& ctx, std::string_view value) {
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

port::port(port&& other)
  : buffer_(std::move(other.buffer_))
  , put_back_buffer_(std::move(other.put_back_buffer_))
  , input_{other.input_}
  , output_{other.output_}
  , should_close_{other.should_close_}
{
  other.should_close_ = false;
}

port::~port() {
  destroy();
}

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
  if (!put_back_buffer_.empty())
    return put_back_buffer_.back();

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
  if (!put_back_buffer_.empty()) {
    char result = put_back_buffer_.back();
    put_back_buffer_.pop_back();
    return result;
  }

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

void
port::put_back(char c) {
  if (!input_)
    throw std::runtime_error{"Not an input port"};

  put_back_buffer_.push_back(c);
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
port::hash() const {
  if (FILE* const* fp = std::get_if<FILE*>(&buffer_))
    return reinterpret_cast<std::size_t>(*fp);

  string_buffer const& sb = std::get<string_buffer>(buffer_);
  return std::hash<std::string>{}(sb.data) ^ sb.read_index;
}

void
port::destroy() {
  if (should_close_)
    if (FILE** f = std::get_if<FILE*>(&buffer_))
      std::fclose(*f);
}

bool
is_list(object* x) {
  while (true)
    if (is<null_type>(x))
      return true;
    else if (pair* p = match<pair>(x))
      x = cdr(p);
    else
      return false;
}

std::size_t
list_length(object* x) {
  std::size_t result = 0;
  while (!is<null_type>(x)) {
    x = cdr(expect<pair>(x));
    ++result;
  }
  return result;
}

object*
cadr(pair* x) {
  return car(expect<pair>(cdr(x)));
}

object*
caddr(pair* x) {
  return car(expect<pair>(cddr(x)));
}

object*
cadddr(pair* x) {
  return car(expect<pair>(cdddr(x)));
}

object*
cddr(pair* x) {
  return cdr(expect<pair>(cdr(x)));
}

object*
cdddr(pair* x) {
  return cdr(expect<pair>(cddr(x)));
}

object*
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

  object* new_head = ctx.constants->null.get();
  pair* new_tail = nullptr;
  object* current = expect<pair>(*x);
  for (; x != xs.end() - 1; ++x) {
    current = *x;

    while (current != ctx.constants->null.get()) {
      pair* c = expect<pair>(current);
      pair* new_c = make<pair>(ctx, car(c), ctx.constants->null.get());

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
vector::trace(tracing_context& tc) const {
  for (std::size_t i = 0; i < size_; ++i)
    tc.trace(storage_element(i));
}

void
vector::update_references() {
  for (std::size_t i = 0; i < size_; ++i)
    update_reference(storage_element(i));
}

object*
vector::ref(std::size_t i) const {
  if (i >= size_)
    throw std::runtime_error{fmt::format("Vector access out of bounds: index = {}, size = {}", i, size_)};

  return storage_element(i);
}

void
vector::set(free_store& store, std::size_t i, object* value) {
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

vector*
make_vector(context& ctx, std::vector<object*> const& elems) {
  auto result = make<vector>(ctx, ctx, elems.size());
  for (std::size_t i = 0; i < elems.size(); ++i)
    result->set(ctx.store, i, elems[i]);

  return result;
}

vector*
list_to_vector(context& ctx, object* lst) {
  std::size_t size = 0;
  for (object* e = lst; e != ctx.constants->null.get(); e = cdr(expect<pair>(e)))
    ++size;

  auto result = make<vector>(ctx, ctx, size);
  std::size_t i = 0;
  for (object* e = lst; e != ctx.constants->null.get(); e = cdr(assume<pair>(e)))
    result->set(ctx.store, i++, car(assume<pair>(e)));

  return result;
}

std::vector<object*>
list_to_std_vector(object* lst) {
  std::vector<object*> result;
  for (object* e : in_list{lst})
    result.push_back(e);

  return result;
}

vector*
vector_append(context& ctx, object_span vs) {
  std::size_t size = 0;
  for (object* e : vs) {
    vector* v = expect<vector>(e);
    size += v->size();
  }

  auto result = make<vector>(ctx, ctx, size);
  std::size_t i = 0;
  for (object* e : vs) {
    vector* v = assume<vector>(e);
    for (std::size_t j = 0; j < v->size(); ++j)
      result->set(ctx.store, i++, v->ref(j));
  }

  return result;
}

box::box(object* value)
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

procedure*
make_procedure(context& ctx, bytecode const& bc, unsigned locals_size,
               unsigned min_args, bool has_rest, std::optional<std::string> name) {
  std::size_t entry = ctx.program.size();
  ctx.program.insert(ctx.program.end(), bc.begin(), bc.end());
  return make<procedure>(ctx, entry, bc.size(), locals_size, min_args, has_rest, std::move(name));
}

closure::closure(insider::procedure* p, std::size_t num_captures)
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

object*
closure::ref(std::size_t i) const {
  assert(i < size_);
  return storage_element(i);
}

void
closure::set(free_store& store, std::size_t i, object* value) {
  assert(i < size_);
  storage_element(i) = value;
  store.notify_arc(this, value);
}

void
closure::trace(tracing_context& tc) const {
  tc.trace(procedure_);
  for (std::size_t i = 0; i < size_; ++i)
    tc.trace(storage_element(i));
}

void
closure::update_references() {
  update_reference(procedure_);
  for (std::size_t i = 0; i < size_; ++i)
    update_reference(storage_element(i));
}

bool
is_callable(object* x) {
  return is<procedure>(x) || is<native_procedure>(x) || is<closure>(x);
}

object*
expect_callable(object* x) {
  if (!is_callable(x))
    throw std::runtime_error{"Expected a callable"};
  else
    return x;
}

std::size_t
syntactic_closure::extra_elements(insider::environment*, object*, object* free) {
  return list_length(free);
}

syntactic_closure::syntactic_closure(insider::environment* env, object* expr, object* free)
  : expression_{expr}
  , env_{env}
  , free_size_{0}
{
  for (object* name : in_list{free}) {
    expect<symbol>(name);
    storage_element(free_size_++) = name;
  }
}

syntactic_closure::syntactic_closure(syntactic_closure&& other)
  : expression_{other.expression_}
  , env_{other.env_}
  , free_size_{other.free_size_}
{
  for (std::size_t i = 0; i < free_size_; ++i)
    storage_element(i) = other.storage_element(i);
}

std::vector<symbol*>
syntactic_closure::free() const {
  std::vector<symbol*> result;
  result.reserve(free_size_);

  for (std::size_t i = 0; i < free_size_; ++i)
    result.push_back(assume<symbol>(storage_element(i)));

  return result;
}

void
syntactic_closure::trace(tracing_context& tc) const {
  tc.trace(expression_);
  tc.trace(env_);
  for (std::size_t i = 0; i < free_size_; ++i)
    tc.trace(storage_element(i));
}

void
syntactic_closure::update_references() {
  update_reference(expression_);
  update_reference(env_);
  for (std::size_t i = 0; i < free_size_; ++i)
    update_reference(storage_element(i));
}

void
transformer::trace(tracing_context& tc) const {
  tc.trace(env_);
  tc.trace(callable_);
}

void
transformer::update_references() {
  update_reference(env_);
  update_reference(callable_);
}

} // namespace insider
