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
  if (!is<syntax>(x))
    return false;
  return is<symbol>(assume<syntax>(x)->expression());
}

std::string
identifier_name(syntax* x) {
  return syntax_expect<symbol>(x)->value();
}

bool
add_scope(scope_set& set, scope* env) {
  if (std::find(set.begin(), set.end(), env) == set.end()) {
    set.push_back(env);
    return true;
  } else
    return false;
}

void
remove_scope(scope_set& set, scope* env) {
  set.erase(std::remove(set.begin(), set.end(), env), set.end());
}

bool
flip_scope(scope_set& set, scope* env) {
  auto it = std::find(set.begin(), set.end(), env);
  if (it == set.end()) {
    set.push_back(env);
    return true;
  } else {
    set.erase(it);
    return false;
  }
}

bool
scope_sets_subseteq(scope_set const& lhs, scope_set const& rhs) {
  for (scope const* e : lhs)
    if (std::find(rhs.begin(), rhs.end(), e) == rhs.end())
      return false;
  return true;
}

bool
scope_sets_equal(scope_set const& lhs, scope_set const& rhs) {
  return scope_sets_subseteq(lhs, rhs) && scope_sets_subseteq(rhs, lhs);
}

void
scope::add(free_store& store, syntax* identifier, std::shared_ptr<variable> var) {
  assert(is_identifier(identifier));

  if (is_redefinition(identifier, var))
    throw std::runtime_error{fmt::format("Redefinition of {}", identifier_name(identifier))};

  bindings_.emplace_back(binding{identifier, std::move(var)});
  store.notify_arc(this, identifier);
}

void
scope::add(free_store& store, syntax* identifier, transformer* tr) {
  assert(is_identifier(identifier));
  assert(tr != nullptr);

  if (is_redefinition(identifier, tr))
    throw std::runtime_error{fmt::format("Redefinition of {}", identifier_name(identifier))};

  bindings_.emplace_back(binding{identifier, tr});

  store.notify_arc(this, identifier);
  store.notify_arc(this, tr);
}

void
scope::add(free_store& store, syntax* identifier, value_type const& value) {
  if (auto var = std::get_if<std::shared_ptr<variable>>(&value))
    add(store, identifier, *var);
  else
    add(store, identifier, std::get<transformer*>(value));
}

auto
scope::find_candidates(symbol* name, scope_set const& envs) const -> std::vector<binding> {
  std::vector<binding> result;
  for (binding const& e : bindings_) {
    syntax const* s = std::get<syntax*>(e);
    if (assume<symbol>(s->expression()) == name && scope_sets_subseteq(s->scopes(), envs))
      result.push_back(e);
  }

  return result;
}

std::vector<std::string>
scope::bound_names() const {
  std::vector<std::string> result;
  result.reserve(bindings_.size());

  for (auto const& [identifier, binding] : bindings_)
    result.push_back(identifier_name(identifier));

  return result;
}

void
scope::trace(tracing_context& tc) const {
  for (auto& [identifier, binding] : bindings_) {
    tc.trace(identifier);
    if (transformer* const* tr = std::get_if<transformer*>(&binding))
      tc.trace(*tr);
  }
}

void
scope::update_references() {
  for (binding& b : bindings_) {
    update_reference(std::get<syntax*>(b));

    value_type& value = std::get<value_type>(b);
    if (transformer** tr = std::get_if<transformer*>(&value))
      update_reference(*tr);
  }
}

std::size_t
scope::hash() const {
  return bindings_.size();
}

bool
scope::is_redefinition(syntax* id, value_type const& intended_value) const {
  for (binding const& b : bindings_)
    if (assume<symbol>(std::get<syntax*>(b)->expression())->value() == assume<symbol>(id->expression())->value()
        && scope_sets_equal(id->scopes(), std::get<syntax*>(b)->scopes())
        && std::get<value_type>(b) != intended_value)
      return true;
  return false;
}

std::optional<scope::value_type>
lookup(symbol* name, scope_set const& envs) {
  std::optional<scope::value_type> result;
  scope_set maximal_scope_set;
  bool ambiguous = false;

  for (scope const* e : envs)
    for (scope::binding const& b : e->find_candidates(name, envs)) {
      scope_set const& binding_set = std::get<syntax*>(b)->scopes();

      if (scope_sets_subseteq(maximal_scope_set, binding_set)) {
        ambiguous = false;
        maximal_scope_set = binding_set;
        result = std::get<scope::value_type>(b);
      } else if (!scope_sets_subseteq(binding_set, maximal_scope_set))
        ambiguous = true;
    }

  if (ambiguous)
    throw error{fmt::format("Ambiguous reference to {}", name->value())};

  return result;
}

std::optional<scope::value_type>
lookup(syntax* id) {
  assert(is_identifier(id));
  return lookup(assume<symbol>(id->expression()), id->scopes());
}

module::module(context& ctx)
  : env_{make_tracked<insider::scope>(ctx, "module top-level")}
{ }

auto
module::find(symbol* identifier) const -> std::optional<binding_type> {
  if (auto binding = lookup(identifier, {env_.get()}))
    return binding;

  return std::nullopt;
}

void
module::import_(context& ctx, symbol* identifier, binding_type b) {
  if (auto v = find(identifier)) {
    if (*v == b)
      return; // Re-importing the same variable under the same name is OK.
    else
      throw std::runtime_error{fmt::format("Redefinition of {}", identifier->value())};
  }

  if (auto* var = std::get_if<std::shared_ptr<variable>>(&b))
    env_->add(env_.store(), make<syntax>(ctx, identifier, scope_set{env_.get()}), *var);
  else
    env_->add(env_.store(), make<syntax>(ctx, identifier, scope_set{env_.get()}),
              std::get<transformer*>(b));
}

void
module::export_(symbol* name) {
  if (!find(name))
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
      m.scope()->add(ctx.store, make<syntax>(ctx, ctx.intern(to_name), scope_set{m.scope()}), *b);
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
  auto name_sym = ctx.intern(name);
  auto var = std::make_shared<variable>(name, index);
  m.scope()->add(ctx.store, make<syntax>(ctx, name_sym, scope_set{m.scope()}), var);

  if (export_)
    m.export_(name_sym);

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

std::optional<std::vector<tracked_ptr<syntax>>>
filesystem_module_provider::find_module(context& ctx, module_name const& name) {
  std::filesystem::path p = root_;
  for (std::string const& element : name)
    p /= element;

  std::vector<std::filesystem::path> candidates{p.replace_extension(".sld"),
                                                p.replace_extension(".scm")};
  for (auto const& candidate : candidates) {
    FILE* f = open_file(candidate.c_str(), _T("r"));
    if (f) {
      auto in = make<port>(ctx, f, candidate, true, false);
      std::optional<module_name> candidate_name = read_library_name(ctx, in);
      if (candidate_name == name) {
        in->rewind();
        return read_syntax_multiple(ctx, in);
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
    {constants->let,               "let"},
    {constants->letrec_star,       "letrec*"},
    {constants->set,               "set!"},
    {constants->lambda,            "lambda"},
    {constants->if_,               "if"},
    {constants->box,               "box"},
    {constants->unbox,             "unbox"},
    {constants->box_set,           "box-set!"},
    {constants->define,            "define"},
    {constants->define_syntax,     "define-syntax"},
    {constants->begin,             "begin"},
    {constants->begin_for_syntax,  "begin-for-syntax"},
    {constants->quote,             "quote"},
    {constants->quasiquote,        "quasiquote"},
    {constants->unquote,           "unquote"},
    {constants->unquote_splicing,  "unquote-splicing"},
    {constants->expand_quote,      "expand-quote"},
    {constants->syntax,            "syntax"},
    {constants->quasisyntax,       "quasisyntax"},
    {constants->unsyntax,          "unsyntax"},
    {constants->unsyntax_splicing, "unsyntax-splicing"},
    {constants->syntax_trap,       "syntax-trap"},
    {constants->syntax_error,      "syntax-error"},
    {constants->let_syntax,        "let-syntax"},
    {constants->letrec_syntax,     "letrec-syntax"}
  };
  for (auto const& form : core_forms) {
    form.object = make_tracked<core_form_type>(*this, form.name);
    auto index = add_top_level(form.object.get(), form.name);
    auto name = intern(form.name);
    auto id = make<syntax>(*this, name, scope_set{internal_module.scope()});
    auto var = std::make_shared<variable>(form.name, index);
    internal_module.scope()->add(store, id, std::move(var));
    internal_module.export_(name);
  }

  statics.null = intern_static(constants->null);
  statics.void_ = intern_static(constants->void_);
  statics.t = intern_static(constants->t);
  statics.f = intern_static(constants->f);
  statics.zero = intern_static(generic_tracked_ptr{store, integer_to_ptr(0)});
  statics.one = intern_static(generic_tracked_ptr{store, integer_to_ptr(1)});

  output_port = make_tracked<port>(*this, stdout, "<stdout>", false, true, false);
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
context::load_library_module(std::vector<tracked_ptr<syntax>> const& data) {
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
      if (std::optional<std::vector<tracked_ptr<syntax>>> lib = provider->find_module(*this, name)) {
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

port::port(FILE* f, std::string name, bool input, bool output, bool should_close)
  : buffer_{f}
  , input_{input}
  , output_{output}
  , should_close_{should_close}
  , name_{std::move(name)}
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

object*
vector_to_list(context& ctx, vector* v) {
  object* result = ctx.constants->null.get();
  for (std::size_t i = v->size(); i > 0; --i)
    result = cons(ctx, v->ref(i - 1), result);

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

std::string
format_location(source_location const& loc) {
  return fmt::format("{}:{}:{}",
                     loc.file_name.empty() ? "<unknown>" : loc.file_name,
                     loc.line, loc.column);
}

input_stream::input_stream(insider::port* p)
  : port_{p}
{ }

std::optional<char>
input_stream::peek_char() {
  return port_->peek_char();
}

std::optional<char>
input_stream::read_char() {
  if (auto c = port_->read_char()) {
    if (*c == '\n') {
      ++line_;
      column_ = 1;
    } else
      ++column_;

    return c;
  } else
    return std::nullopt;
}

void
input_stream::put_back(char c) {
  if (c == '\n') {
    assert(line_ > 1);
    --line_;
    column_ = 1;
  } else {
    assert(column_ > 1);
    --column_;
  }

  port_->put_back(c);
}

std::optional<char>
input_stream::advance_and_peek_char() {
  read_char();
  return peek_char();
}

source_location
input_stream::current_location() const {
  return source_location{port_->name(), line_, column_};
}

void
syntax::trace(tracing_context& tc) const {
  tc.trace(expression_);

  for (scope* env : scopes_)
    tc.trace(env);
}

void
syntax::add_scope(free_store& fs, scope* s) {
  bool added = insider::add_scope(scopes_, s);
  if (added)
    fs.notify_arc(this, s);
}

void
syntax::remove_scope(scope* s) {
  insider::remove_scope(scopes_, s);
}

void
syntax::flip_scope(free_store& fs, scope* s) {
  bool added = insider::flip_scope(scopes_, s);
  if (added)
    fs.notify_arc(this, s);
}

void
syntax::update_references() {
  update_reference(expression_);

  for (scope*& env : scopes_)
    update_reference(env);
}

static object*
syntax_to_datum_helper(context& ctx, object* o) {
  if (o == ctx.constants->null.get()) {
    return o;
  } else if (auto p = semisyntax_match<pair>(o)) {
    return cons(ctx, syntax_to_datum_helper(ctx, car(p)), syntax_to_datum_helper(ctx, cdr(p)));
  } else if (auto v = semisyntax_match<vector>(o)) {
    auto result = make<vector>(ctx, ctx, v->size());
    for (std::size_t i = 0; i < v->size(); ++i)
      result->set(ctx.store, i, syntax_to_datum_helper(ctx, v->ref(i)));
    return result;
  } else if (auto stx = match<syntax>(o)) {
    return stx->expression();
  } else {
    return o;
  }
}

object*
syntax_to_datum(context& ctx, syntax* stx) {
  return syntax_to_datum_helper(ctx, stx);
}

syntax*
datum_to_syntax(context& ctx, syntax* s, object* datum) {
  if (auto p = match<pair>(datum)) {
    syntax* head = datum_to_syntax(ctx, s, car(p));
    syntax* tail = datum_to_syntax(ctx, s, cdr(p));
    return make<syntax>(ctx, cons(ctx, head, tail), s->location(), s->scopes());
  } else if (auto v = match<vector>(datum)) {
    auto result_vec = make<vector>(ctx, ctx, v->size());
    for (std::size_t i = 0; i < v->size(); ++i)
      result_vec->set(ctx.store, i, datum_to_syntax(ctx, s, v->ref(i)));
    return make<syntax>(ctx, result_vec, s->location(), s->scopes());
  } else if (auto stx = match<syntax>(datum)) {
    return stx;
  } else
    return make<syntax>(ctx, datum, s->location(), s->scopes());
}

object*
syntax_to_list(context& ctx, object* stx) {
  if (semisyntax_is<null_type>(stx))
    return ctx.constants->null.get();

  if (!is<pair>(stx) && (!is<syntax>(stx) || !syntax_is<pair>(assume<syntax>(stx))))
    return nullptr;

  pair* result = make<pair>(ctx, car(semisyntax_assume<pair>(stx)), ctx.constants->null.get());
  pair* tail = result;
  object* datum = cdr(semisyntax_assume<pair>(stx));

  while (pair* p = semisyntax_match<pair>(datum)) {
    auto new_pair = make<pair>(ctx, car(p), ctx.constants->null.get());
    tail->set_cdr(ctx.store, new_pair);
    tail = new_pair;

    datum = cdr(p);
  }

  if (!semisyntax_is<null_type>(datum))
    return nullptr;

  return result;
}

static object*
copy_syntax_helper(context& ctx, object* o) {
  if (auto stx = match<syntax>(o))
    return make<syntax>(ctx, copy_syntax_helper(ctx, stx->expression()), stx->location(), stx->scopes());
  else if (auto p = match<pair>(o))
    return make<pair>(ctx, copy_syntax_helper(ctx, car(p)), copy_syntax_helper(ctx, cdr(p)));
  else if (auto v = match<vector>(o)) {
    auto new_v = make<vector>(ctx, ctx, v->size());
    for (std::size_t i = 0; i < v->size(); ++i)
      new_v->set(ctx.store, i, copy_syntax_helper(ctx, v->ref(i)));
    return new_v;
  } else
    return o;
}

syntax*
copy_syntax(context& ctx, syntax* stx) {
  return assume<syntax>(copy_syntax_helper(ctx, stx));
}

void
transformer::trace(tracing_context& tc) const {
  tc.trace(callable_);
}

void
transformer::update_references() {
  update_reference(callable_);
}

} // namespace insider
