#include "module.hpp"

#include "analyser.hpp"
#include "basic_types.hpp"
#include "compiler.hpp"
#include "context.hpp"
#include "port.hpp"
#include "read.hpp"
#include "vm.hpp"

namespace insider {

module_::module_(context& ctx, std::optional<module_name> const& name)
  : env_{make_tracked<insider::scope>(ctx, ctx,
                                      fmt::format("{} module top-level",
                                                  name ? module_name_to_string(*name) : "<unnamed module>"))}
{ }

auto
module_::find(ptr<symbol> identifier) const -> std::optional<binding_type> {
  if (auto binding = lookup(identifier, scope_set{env_.get()}))
    return binding;

  return std::nullopt;
}

void
module_::import_(context& ctx, ptr<symbol> identifier, binding_type b) {
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
              std::get<ptr<transformer>>(b));
}

ptr<procedure>
module_::top_level_procedure() const {
  return proc_.get();
}

std::vector<std::string>
module_::top_level_names() const {
  std::vector<std::string> result;
  for (auto const& [identifier, binding] : *env_)
    if (identifier->scopes().size() == 1)
      result.push_back(identifier_name(identifier));
  return result;
}

void
module_::export_(ptr<symbol> name) {
  assert(find(name));
  exports_.emplace(name->value());
}

namespace {
  struct import_set {
    module_* source;
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
perform_imports(context& ctx, module_& m, import_set const& set) {
  for (auto const& [to_name, from_name] : set.names) {
    if (auto b = set.source->find(ctx.intern(from_name)))
      m.scope()->add(ctx.store, make<syntax>(ctx, ctx.intern(to_name), scope_set{m.scope()}), *b);
    else
      assert(!"Trying to import a nonexistent symbol");
  }

  if (!set.source->active())
    execute(ctx, *set.source);
}

static void
check_all_defined(context& ctx, module_& m, std::vector<std::string> const& names) {
  std::vector<std::string> undefined;
  for (std::string const& name : names)
    if (!m.find(ctx.intern(name)))
      undefined.push_back(name);

  if (!undefined.empty()) {
    if (undefined.size() == 1)
      throw std::runtime_error{fmt::format("Can't export undefined symbol {}", undefined.front())};
    else {
      std::string names = undefined.front();
      for (auto n = undefined.begin() + 1; n != undefined.end(); ++n)
        names += ", " + *n;
      throw std::runtime_error{fmt::format("Can't export undefined symbols: {}", names)};
    }
  }
}

std::unique_ptr<module_>
instantiate(context& ctx, protomodule const& pm) {
  auto result = std::make_unique<module_>(ctx, pm.name);

  perform_imports(ctx, *result, pm);
  compile_module_body(ctx, *result, pm);

  check_all_defined(ctx, *result, pm.exports);
  for (std::string const& name : pm.exports)
    result->export_(ctx.intern(name));

  return result;
}

void
import_all_exported(context& ctx, module_& to, module_& from) {
  import_set is{&from, {}};

  for (std::string const& name : from.exports())
    is.names.push_back(std::tuple{name, name});

  perform_imports(ctx, to, is);
}

void
import_all_top_level(context& ctx, module_& to, module_& from) {
  import_set is{&from, {}};

  for (std::string const& name : from.top_level_names())
    is.names.push_back(std::tuple{name, name});

  perform_imports(ctx, to, is);
}

void
perform_imports(context& ctx, module_& m, protomodule const& pm) {
  for (import_specifier const& spec : pm.imports)
    perform_imports(ctx, m, parse_import_set(ctx, spec));
}

operand
define_top_level(context& ctx, std::string const& name, module_& m, bool export_, ptr<> object) {
  auto index = ctx.add_top_level(object, name);
  auto name_sym = ctx.intern(name);
  auto var = std::make_shared<variable>(name, index);
  m.scope()->add(ctx.store, make<syntax>(ctx, name_sym, scope_set{m.scope()}), var);

  if (export_)
    m.export_(name_sym);

  return index;
}

static tracked_ptr<>
run_module(context& ctx, module_& m) {
  return call_with_continuation_barrier(ctx, m.top_level_procedure(), {});
}

tracked_ptr<>
execute(context& ctx, module_& mod) {
  tracked_ptr<> result = run_module(ctx, mod);
  mod.mark_active();

  return result;
}

} // namespace insider
