#include "context.hpp"

#include "action.hpp"
#include "analyser.hpp"
#include "basic_types.hpp"
#include "internal_module.hpp"
#include "port.hpp"
#include "read.hpp"
#include "source_code_provider.hpp"
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

  output_port = make_tracked<textual_output_port>(*this, std::make_unique<file_port_sink>(stdout, false));
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

static std::optional<source_file>
find_module_in_provider(context& ctx, source_code_provider& provider, module_name const& name) {
  std::filesystem::path path = module_name_to_path(name);
  std::array<std::filesystem::path, 2> candidates{path.replace_extension(".sld"),
                                                  path.replace_extension(".scm")};
  for (auto const& candidate : candidates)
    if (auto source = provider.find_file(ctx, candidate))
      if (read_library_name(ctx, *source->port) == name) {
        source->port->rewind();
        return source;
      }

  return std::nullopt;
}

static protomodule
find_protomodule(context& ctx, module_name const& name,
                 std::vector<std::unique_ptr<source_code_provider>> const& providers) {
  for (std::unique_ptr<source_code_provider> const& provider : providers)
    if (auto source = find_module_in_provider(ctx, *provider, name))
      return read_library(ctx, read_syntax_multiple(ctx, *source->port));

  throw std::runtime_error{fmt::format("Unknown module {}", module_name_to_string(name))};
}

module*
context::find_module(module_name const& name) {
  using namespace std::literals;

  if (name == std::vector{"insider"s, "internal"s})
    return &internal_module;

  auto mod_it = modules_.find(name);
  if (mod_it != modules_.end())
    return mod_it->second.get();

  simple_action a(*this, "Analysing module {}", module_name_to_string(name));
  std::unique_ptr<module> m = instantiate(*this, find_protomodule(*this, name, source_providers_));

  mod_it = modules_.emplace(name, std::move(m)).first;
  return mod_it->second.get();
}

void
context::prepend_source_code_provider(std::unique_ptr<source_code_provider> provider) {
  source_providers_.insert(source_providers_.begin(), std::move(provider));
}

void
context::append_source_code_provider(std::unique_ptr<source_code_provider> provider) {
  source_providers_.push_back(std::move(provider));
}

} // namespace insider
