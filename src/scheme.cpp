#include "scheme.hpp"

#include "analyser.hpp"
#include "compiler.hpp"
#include "converters.hpp"
#include "io.hpp"
#include "numeric.hpp"

#include <fmt/format.h>

#include <algorithm>
#include <cassert>
#include <cstdio>
#include <typeinfo>

namespace insider {

std::size_t
hash(generic_ptr const& x) {
  if (auto i = match<integer>(x))
    return integer_hash(i);
  if (auto b = match<boolean>(x))
    return boolean_hash(b);
  else if (auto p = match<pair>(x))
    return pair_hash(p);
  else if (auto v = match<vector>(x))
    return vector_hash(v);
  else
    return reinterpret_cast<std::size_t>(x.get());
}

bool
eqv(generic_ptr const& x, generic_ptr const& y) {
  if (typeid(*x) != typeid(*y))
    return false;
  else if (auto lhs = match<integer>(x)) {
    if (auto rhs = match<integer>(y))
      return lhs->value() == rhs->value();
    else
      return false;
  }
  else
    return x.get() == y.get();
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

void
environment::add(std::string const& name, std::shared_ptr<variable> var) {
  bool inserted = bindings_.emplace(name, std::move(var)).second;
  if (!inserted)
    throw std::runtime_error{fmt::format("Redefinition of {}", name)};
}

void
environment::add_transformer(std::string const& name, ptr<transformer> const& tr) {
  bool inserted = bindings_.emplace(name, tr.get()).second;
  if (!inserted)
    throw std::runtime_error{fmt::format("Redefinition of {}", name)};
}

std::shared_ptr<variable>
environment::lookup(std::string const& name) const {
  if (auto it = bindings_.find(name); it != bindings_.end())
    if (auto var = std::get_if<std::shared_ptr<variable>>(&it->second))
      return *var;

  return {};
}

ptr<transformer>
environment::lookup_transformer(free_store& fs, std::string const& name) const {
  if (auto it = bindings_.find(name); it != bindings_.end())
    if (auto tr = std::get_if<transformer*>(&it->second))
      return {fs, *tr};

  return {};
}

std::vector<std::string>
environment::bound_names() const {
  std::vector<std::string> result;
  result.reserve(bindings_.size());

  for (auto const& [name, binding] : bindings_)
    result.push_back(name);

  return result;
}

void
environment::trace(tracing_context& tc) {
  tc.trace(parent_);

  for (auto& [name, binding] : bindings_)
    if (transformer** tr = std::get_if<transformer*>(&binding))
      tc.trace(*tr);
}

void
environment::update_references() {
  update_reference(parent_);

  for (auto& [name, binding] : bindings_)
    if (transformer** tr = std::get_if<transformer*>(&binding))
      update_reference(*tr);
}

module::module(context& ctx)
  : env_{make<insider::environment>(ctx, ptr<insider::environment>{})}
{ }

auto
module::find(std::string const& name) const -> std::optional<index_type> {
  if (std::shared_ptr<variable> var = env_->lookup(name))
    return var->global;

  return std::nullopt;
}

void
module::add(std::string name, index_type i) {
  if (auto v = env_->lookup(name)) {
    if (v->global && *v->global == i)
      return; // Re-importing the same variable under the same name is OK.
    else
      throw std::runtime_error{fmt::format("Redefinition of {}", name)};
  }
  env_->add(name, std::make_shared<variable>(name, i));
}

void
module::add_transformer(std::string name, ptr<transformer> const& tr) {
  if (auto old_tr = environment_lookup_transformer(env_, name)) {
    if (old_tr == tr)
      return; // Re-importing the same variable under the same name is OK.
    else
      throw std::runtime_error{fmt::format("Redefinition of {}", name)};
  }
  env_->add_transformer(name, tr);
}

void
module::export_(std::string name) {
  if (!env_->has(name))
    throw std::runtime_error{fmt::format("Can't export undefined symbol {}", name)};

  exports_.emplace(std::move(name));
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
  else {
    assert(!"Can't happen");
    return {};
  }
}

static void
perform_imports(context& ctx, module& m, import_set const& set) {
  for (auto const& [to_name, from_name] : set.names) {
    if (std::optional<module::index_type> var = set.source->find(from_name))
      m.add(to_name, *var);
    else if (ptr<transformer> tr = set.source->environment()->lookup_transformer(ctx.store, from_name))
      m.add_transformer(to_name, tr);
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
    result->export_(name);

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

void
define_top_level(context& ctx, module& m, std::string const& name, generic_ptr const& object,
                 bool export_) {
  auto index = ctx.add_top_level(object, name);
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

static ptr<symbol>
type(context& ctx, generic_ptr const& x) {
  return ctx.intern(object_type(x.get()).name);
}

static module
make_internal_module(context& ctx) {
  module result{ctx};
  result.mark_active();

  export_numeric(ctx, result);

  define_lambda<void(context&, generic_ptr const&)>(
    ctx, result, "write-simple", true,
    [] (context& ctx, generic_ptr const& datum) {
      write_simple(ctx, datum, ctx.output_port);
    }
  );

  define_lambda<void(context&, generic_ptr const&)>(
    ctx, result, "display", true,
    [] (context& ctx, generic_ptr const& datum) {
      display(ctx, datum, ctx.output_port);
    }
  );

  define_lambda<void(context&)>(
    ctx, result, "newline", true,
    [] (context& ctx) { ctx.output_port->write_char('\n'); }
  );

  define_top_level(ctx, result, "append", make<native_procedure>(ctx, append, "append"), true);
  define_lambda<ptr<vector>(context&, generic_ptr const&)>(ctx, result, "list->vector", true, list_to_vector);
  define_top_level(ctx, result, "vector-append", make<native_procedure>(ctx, vector_append, "vector-append"), true);
  define_lambda<ptr<integer>(context&, ptr<vector> const&)>(
    ctx, result, "vector-length", true,
    [] (context& ctx, ptr<vector> const& v) {
      return make<integer>(ctx, v->size());
    }
  );
  define_lambda<vector_ref>(ctx, result, "vector-ref", true);
  define_lambda<vector_set>(ctx, result, "vector-set!", true);

  define_lambda<ptr<pair>(context&, generic_ptr const&, generic_ptr const&)>(
    ctx, result, "cons", true,
    [] (context& ctx, generic_ptr const& car, generic_ptr const& cdr) {
      return make<pair>(ctx, car, cdr);
    }
  );
  define_lambda<generic_ptr(ptr<pair> const&)>(ctx, result, "car", true, car);
  define_lambda<generic_ptr(ptr<pair> const&)>(ctx, result, "cdr", true, cdr);
  define_lambda<generic_ptr(ptr<pair> const&)>(ctx, result, "cadr", true, cadr);
  define_lambda<generic_ptr(ptr<pair> const&)>(ctx, result, "caddr", true, caddr);
  define_lambda<generic_ptr(ptr<pair> const&)>(ctx, result, "cadddr", true, cadddr);
  define_lambda<generic_ptr(ptr<pair> const&)>(ctx, result, "cddr", true, cddr);
  define_lambda<generic_ptr(ptr<pair> const&)>(ctx, result, "cdddr", true, cdddr);

  define_lambda<ptr<integer>(context&, ptr<string> const&)>(
    ctx, result, "string-length", true,
    [] (context& ctx, ptr<string> const& s) {
      return make<integer>(ctx, s->size());
    }
  );

  define_lambda<ptr<syntactic_closure>(context&, ptr<environment> const&,
                                       generic_ptr const&, generic_ptr const&)>(
    ctx, result, "make-syntactic-closure", true,
    [] (context& ctx, ptr<environment> const& env, generic_ptr const& free, generic_ptr const& form) {
      return make<syntactic_closure>(ctx, env, form, free);
    }
  );

  define_lambda<type>(ctx, result, "type", true);

  define_lambda<ptr<boolean>(context&, generic_ptr const&, generic_ptr const&)>(
    ctx, result, "eq?", true,
    [] (context& ctx, generic_ptr const& x, generic_ptr const& y) {
      return x == y ? ctx.constants->t : ctx.constants->f;
    }
  );

  define_lambda<eqv>(ctx, result, "eqv?", true);

  define_lambda<ptr<vector>(context&, ptr<procedure> const&)>(
    ctx, result, "procedure-bytecode", true,
    [] (context& ctx, ptr<procedure> const& f) {
      disable_collection dc{ctx.store};
      return make_list_from_vector(ctx, f->bytecode,
                                   [&] (instruction i) {
                                     return make<opaque_value<instruction>>(ctx, i);
                                   });
    }
  );

  define_lambda<generic_ptr(context&, ptr<procedure> const&)>(
    ctx, result, "procedure-name", true,
    [] (context& ctx, ptr<procedure> const& f) -> generic_ptr {
      disable_collection dc{ctx.store};

      if (f->name)
        return make_string(ctx, *f->name);
      else
        return ctx.constants->f;
    }
  );

  define_lambda<ptr<integer>(ptr<opaque_value<instruction>> const&)>(
    ctx, result, "instruction-opcode", true,
    [] (ptr<opaque_value<instruction>> const& i) { return static_cast<integer::storage_type>(i->value.opcode); }
  );

  define_lambda<generic_ptr(context&, ptr<opaque_value<instruction>> const&)>(
    ctx, result, "instruction-operands", true,
    [] (context& ctx, ptr<opaque_value<instruction>> const& i) {
      instruction instr = i->value;
      return make_list(ctx,
                       make<opaque_value<operand>>(ctx, instr.x),
                       make<opaque_value<operand>>(ctx, instr.y),
                       make<opaque_value<operand>>(ctx, instr.dest));
    }
  );

  define_lambda<ptr<symbol>(context&, ptr<opaque_value<operand>> const&)>(
    ctx, result, "operand-scope", true,
    [] (context& ctx, ptr<opaque_value<operand>> const& o) {
      switch (o->value.scope()) {
      case operand::scope_type::local: return ctx.intern("local");
      case operand::scope_type::global: return ctx.intern("global");
      case operand::scope_type::static_: return ctx.intern("static");
      case operand::scope_type::closure: return ctx.intern("closure");
      default: assert(!"Unreachable"); return ctx.intern("invalid");
      }
    }
  );

  define_lambda<generic_ptr(context&, ptr<opaque_value<operand>> const&)>(
    ctx, result, "operand-value", true,
    [] (context& ctx, ptr<opaque_value<operand>> const& o) {
      return make<integer>(ctx, static_cast<integer::storage_type>(o->value.value()));
    }
  );

  define_lambda<generic_ptr(context&, ptr<opaque_value<operand>> const&)>(
    ctx, result, "operand-immediate-value", true,
    [] (context& ctx, ptr<opaque_value<operand>> const& o) {
      return make<integer>(ctx, static_cast<integer::storage_type>(o->value.immediate_value()));
    }
  );

  define_lambda<generic_ptr(context&, ptr<opaque_value<operand>> const&)>(
    ctx, result, "operand-offset", true,
    [] (context& ctx, ptr<opaque_value<operand>> const& o) {
      return make<integer>(ctx, static_cast<integer::storage_type>(o->value.offset()));
    }
  );

  std::vector<generic_ptr> opcodes;
  auto category_to_symbol = [&] (opcode_category cat) {
    switch (cat) {
    case opcode_category::none: return ctx.intern("none");
    case opcode_category::register_: return ctx.intern("register");
    case opcode_category::absolute: return ctx.intern("absolute");
    case opcode_category::offset: return ctx.intern("offset");
    default: assert(!"Unreachable"); return ctx.intern("invalid");
    }
  };
  for (auto const& info : opcode_value_to_info)
    opcodes.emplace_back(make_list(ctx,
                                   make_string(ctx, info.mnemonic),
                                   category_to_symbol(info.x),
                                   category_to_symbol(info.y),
                                   category_to_symbol(info.dest)));
  define_top_level(ctx, result, "opcodes", make_vector(ctx, opcodes), true);

  return result;
}

context::context()
  : internal_module{*this}
{
  store.register_callback([&] { gc_callback(); });

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
    {constants->begin_for_syntax, "begin-for-syntax"},
    {constants->quote,            "quote"},
    {constants->quasiquote,       "quasiquote"},
    {constants->unquote,          "unquote"},
    {constants->unquote_splicing, "unquote-splicing"},
    {constants->expand_quote,     "expand-quote"}
  };
  for (auto const& form : core_forms) {
    form.object = store.make<core_form_type>();
    auto index = add_top_level(form.object, form.name);
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
context::add_top_level(generic_ptr const& x, std::string name) {
  top_level_objects_.push_back(x);
  top_level_binding_names_.emplace_back(std::move(name));
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

void
context::load_library_module(std::vector<generic_ptr> const& data) {
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
      if (std::optional<std::vector<generic_ptr>> lib = provider->find_module(*this, name)) {
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

void
context::gc_callback() {
  std::vector<std::tuple<object*, std::size_t>> to_reinsert;
  for (auto it = statics_cache_.begin(); it != statics_cache_.end();) {
    object* updated = update_reference_copy(it->first.get());
    if (it->first.get() != updated) {
      to_reinsert.emplace_back(updated, it->second);
      it = statics_cache_.erase(it);
    } else
      ++it;
  }

  for (auto [object, index] : to_reinsert)
    statics_cache_.emplace(generic_ptr{store, object}, index);
}

simple_action::simple_action(context& ctx, generic_ptr const& irritant, std::string message)
  : action{ctx}
  , message_{std::move(message)}
  , irritant_{irritant}
{ }

std::string
simple_action::format() const {
  if (irritant_)
    return message_ + ": " + datum_to_string(ctx_, irritant_);
  else
    return message_;
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

ptr<string>
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

void
port::destroy() {
  if (should_close_)
    if (FILE** f = std::get_if<FILE*>(&buffer_))
      std::fclose(*f);
}

std::size_t
pair_hash(ptr<pair> const& p) {
  return 3 * hash(car(p)) ^ hash(cdr(p));
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
        set_cdr(new_tail, new_c);
      else
        new_head = new_c;

      new_tail = new_c;
      current = cdr(c);
    }
  }

  assert(x == xs.end() - 1);
  if (new_tail)
    set_cdr(new_tail, *x);
  else
    new_head = *x;

  return new_head;
}

vector::vector(std::size_t size)
  : size_{size}
{
  for (std::size_t i = 0; i < size_; ++i)
    storage_element(i) = nullptr;
}

vector::vector(vector&& other)
  : size_{other.size_}
{
  for (std::size_t i = 0; i < size_; ++i)
    storage_element(i) = other.storage_element(i);
}

void
vector::trace(tracing_context& tc) {
  for (std::size_t i = 0; i < size_; ++i)
    tc.trace(storage_element(i));
}

void
vector::update_references() {
  for (std::size_t i = 0; i < size_; ++i)
    update_reference(storage_element(i));
}

generic_ptr
vector::ref(free_store& store, std::size_t i) const {
  if (i >= size_)
    throw std::runtime_error{fmt::format("Vector access out of bounds: index = {}, size = {}", i, size_)};

  return {store, storage_element(i)};
}

void
vector::set(std::size_t i, object* value) {
  if (i >= size_)
    throw std::runtime_error{fmt::format("Vector access out of bounds: index = {}, size = {}", i, size_)};

  storage_element(i) = value;
}

std::size_t
vector_hash(ptr<vector> const& v) {
  std::size_t result = 0;
  for (std::size_t i = 0; i < v->size(); ++i)
    result = 3 * result ^ hash(vector_ref(v, i));

  return result;
}

ptr<vector>
make_vector(context& ctx, std::vector<generic_ptr> const& elems) {
  auto result = make<vector>(ctx, elems.size());
  for (std::size_t i = 0; i < elems.size(); ++i)
    vector_set(result, i, elems[i]);

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
    vector_set(result, i++, car(assume<pair>(e)));

  return result;
}

std::vector<generic_ptr>
list_to_std_vector(generic_ptr const& lst) {
  std::vector<generic_ptr> result;
  for (generic_ptr e : in_list{lst})
    result.push_back(e);

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
      vector_set(result, i++, vector_ref(v, j));
  }

  return result;
}

box::box(generic_ptr const& value)
  : value_{value.get()}
{ }

procedure::procedure(insider::bytecode bc, unsigned locals_size, unsigned num_args,
                     std::optional<std::string> name)
  : bytecode(std::move(bc))
  , locals_size{locals_size}
  , num_args{num_args}
  , name{std::move(name)}
{ }

closure::closure(ptr<insider::procedure> const& p, std::vector<generic_ptr> const& captures)
  : procedure_{p.get()}
  , size_{captures.size()}
{
  for (std::size_t i = 0; i < size_; ++i)
    storage_element(i) = captures[i].get();
}

closure::closure(closure&& other)
  : procedure_{other.procedure_}
  , size_{other.size_}
{
  for (std::size_t i = 0; i < size_; ++i)
    storage_element(i) = other.storage_element(i);
}

generic_ptr
closure::ref(free_store& store, std::size_t i) const {
  assert(i < size_);
  return {store, storage_element(i)};
}

void
closure::trace(tracing_context& tc) {
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
is_callable(generic_ptr const& x) {
  return is<procedure>(x) || is<native_procedure>(x) || is<closure>(x);
}

generic_ptr
expect_callable(generic_ptr const& x) {
  if (!is_callable(x))
    throw std::runtime_error{"Expected a callable"};
  else
    return x;
}

std::size_t
syntactic_closure::extra_elements(ptr<insider::environment>,
                                  generic_ptr const&, generic_ptr const& free) {
  return list_length(free);
}

syntactic_closure::syntactic_closure(ptr<insider::environment> env,
                                     generic_ptr const& expr, generic_ptr const& free)
  : expression_{expr.get()}
  , env_{env.get()}
  , free_size_{0}
{
  for (generic_ptr name : in_list{free}) {
    expect<symbol>(name);
    storage_element(free_size_++) = name.get();
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

std::vector<ptr<symbol>>
syntactic_closure::free(free_store& store) const {
  std::vector<ptr<symbol>> result;
  result.reserve(free_size_);

  for (std::size_t i = 0; i < free_size_; ++i)
    result.push_back(ptr<symbol>{store, storage_element(i)});

  return result;
}

void
syntactic_closure::trace(tracing_context& tc) {
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
transformer::trace(tracing_context& tc) {
  tc.trace(env_);
  tc.trace(callable_);
}

void
transformer::update_references() {
  update_reference(env_);
  update_reference(callable_);
}

} // namespace insider
