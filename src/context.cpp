#include "context.hpp"

#include "action.hpp"
#include "analyser.hpp"
#include "basic_types.hpp"
#include "internal_module.hpp"
#include "port.hpp"
#include "vm.hpp"

namespace insider {

ptr<>&
parameter_map::find_value(ptr<parameter_tag> tag) {
  for (auto& [key, value] : values_)
    if (key == tag)
      return value;

  assert(!"Can't happen");
  return std::get<1>(values_.front());
}

void
parameter_map::add_value(ptr<parameter_tag> tag, ptr<> value) {
  values_.emplace_back(tag, value);
}

void
parameter_map::visit_members(member_visitor const& f) {
  for (auto& [key, value] : values_) {
    f(key);
    f(value);
  }
}

context::context()
  : internal_module{*this}
  , statics_cache_{0, {}, eqv_compare{*this}}
{
  constants = std::make_unique<struct constants>();
  constants->null = make_tracked<null_type>(*this);
  constants->void_ = make_tracked<void_type>(*this);
  constants->t = make_tracked<boolean>(*this, true);
  constants->f = make_tracked<boolean>(*this, false);
  constants->tail_call_tag = make_tracked<tail_call_tag_type>(*this);

  parameters = make_tracked<parameter_map>(*this);
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
  statics.zero = intern_static(tracked_ptr<>{store, integer_to_ptr(0)});
  statics.one = intern_static(tracked_ptr<>{store, integer_to_ptr(1)});

  output_port = make_tracked<port>(*this, stdout, "<stdout>", false, true, false);
}

context::~context() {
  constants.reset();
}

ptr<symbol>
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
context::intern_static(tracked_ptr<> const& x) {
  auto it = statics_cache_.find(x);
  if (it == statics_cache_.end()) {
    statics_.push_back(x);
    it = statics_cache_.emplace(x, statics_.size() - 1).first;
  }

  return it->second;
}

ptr<>
context::get_static_checked(operand i) const {
  if (i >= statics_.size())
    throw std::runtime_error{fmt::format("Nonexistent static object {}", i)};

  return statics_[i].get();
}

ptr<>
context::get_top_level_checked(operand i) const {
  if (i >= top_level_objects_.size())
    throw std::runtime_error{fmt::format("Nonexistent top-level object {}", i)};

  return top_level_objects_[i].get();
}

void
context::set_top_level(operand i, ptr<> value) {
  assert(i < top_level_objects_.size());
  top_level_objects_[i] = {store, value};
}

operand
context::add_top_level(ptr<> x, std::string name) {
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

} // namespace insider
