#include "module.hpp"

#include "compiler/analyser.hpp"
#include "compiler/compiler.hpp"
#include "compiler/module_name.hpp"
#include "compiler/module_specifier.hpp"
#include "context.hpp"
#include "io/read.hpp"
#include "runtime/basic_types.hpp"
#include "runtime/symbol.hpp"
#include "runtime/syntax.hpp"
#include "vm/vm.hpp"

namespace insider {

module_::module_(context& ctx, std::optional<module_name> const& name)
  : env_{make<insider::scope>(ctx, ctx,
                              fmt::format("{} module top-level",
                                          name
                                            ? module_name_to_string(*name)
                                            : "<unnamed module>"))}
{ }

auto
module_::find(ptr<symbol> identifier) const -> std::optional<binding_type> {
  if (auto binding = lookup(identifier, scope_set{env_}))
    return binding;

  return std::nullopt;
}

void
module_::import_(context& ctx, ptr<symbol> identifier, binding_type b) {
  if (auto v = find(identifier)) {
    if (*v == b)
      return; // Re-importing the same variable under the same name is OK.
    else
      throw std::runtime_error{fmt::format("Redefinition of {}",
                                           identifier->value())};
  }

  if (auto* var = std::get_if<std::shared_ptr<variable>>(&b))
    env_->add(ctx.store, make<syntax>(ctx, identifier, scope_set{env_}), *var);
  else
    env_->add(ctx.store, make<syntax>(ctx, identifier, scope_set{env_}),
              std::get<ptr<transformer>>(b));
}

ptr<procedure>
module_::top_level_procedure() const {
  return proc_;
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

void
module_::visit_members(member_visitor const& f) {
  f(env_);
  f(proc_);
}

namespace {
  struct import_set {
    tracked_ptr<module_> source;
    std::vector<std::tuple<std::string, std::string>> names;

    explicit
    import_set(context& ctx)
      : source{ctx.store}
    { }

    import_set(tracked_ptr<module_> source,
               std::vector<std::tuple<std::string, std::string>> names)
      : source{std::move(source)}
      , names{std::move(names)}
    { }
  };
}

static void
check_all_names_exist(std::vector<std::string> const& names,
                      import_set const& set) {
  for (auto const& name : names)
    if (std::none_of(set.names.begin(), set.names.end(),
                     [&] (auto const& set_name) {
                       return std::get<0>(set_name) == name;
                     }))
      throw std::runtime_error{fmt::format("Identifier {} is not exported",
                                           name)};
}

static import_set
parse_import_set(context& ctx, import_specifier const& spec);

static import_set
parse_module_name_import_set(context& ctx, module_name const* mn) {
  import_set result{ctx};
  result.source = ctx.module_resolver().find_module(ctx, *mn);

  for (std::string const& name : result.source->exports())
    result.names.emplace_back(name, name);

  return result;
}

static import_set
parse_only_import_specifier(context& ctx, import_specifier::only const* o) {
  import_set result = parse_import_set(ctx, *o->from);
  check_all_names_exist(o->identifiers, result);
  result.names.erase(
    std::remove_if(result.names.begin(), result.names.end(),
                   [&] (auto const& name) {
                     return std::find(o->identifiers.begin(),
                                      o->identifiers.end(),
                                      std::get<0>(name)) == o->identifiers.end();
                   }),
    result.names.end()
  );
  return result;
}

import_set
parse_except_import_specifier(context& ctx, import_specifier::except const* e) {
  import_set result = parse_import_set(ctx, *e->from);
  check_all_names_exist(e->identifiers, result);
  result.names.erase(
    std::remove_if(result.names.begin(), result.names.end(),
                   [&] (auto const& name) {
                     return std::find(e->identifiers.begin(),
                                      e->identifiers.end(),
                                      std::get<0>(name)) != e->identifiers.end();
                   }),
    result.names.end()
  );
  return result;
}

import_set
parse_prefix_import_specifier(context& ctx, import_specifier::prefix const* p) {
  import_set result = parse_import_set(ctx, *p->from);
  for (auto& [target, source] : result.names)
    target = p->prefix_ + target;
  return result;
}

import_set
parse_rename_import_specifier(context& ctx, import_specifier::rename const* r) {
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

import_set
parse_import_set(context& ctx, import_specifier const& spec) {
  if (auto const* mn = std::get_if<module_name>(&spec.value))
    return parse_module_name_import_set(ctx, mn);
  else if (auto const* o = std::get_if<import_specifier::only>(&spec.value))
    return parse_only_import_specifier(ctx, o);
  else if (auto const* e = std::get_if<import_specifier::except>(&spec.value))
    return parse_except_import_specifier(ctx, e);
  else if (auto const* p = std::get_if<import_specifier::prefix>(&spec.value))
    return parse_prefix_import_specifier(ctx, p);
  else if (auto const* r = std::get_if<import_specifier::rename>(&spec.value))
    return parse_rename_import_specifier(ctx, r);
  else {
    assert(!"Can't happen");
    return import_set{ctx};
  }
}

static void
perform_imports(context& ctx, tracked_ptr<module_> const& m,
                import_set const& set) {
  for (auto const& [to_name, from_name] : set.names) {
    if (auto b = set.source->find(ctx.intern(from_name)))
      m->scope()->add(
        ctx.store,
        make<syntax>(ctx, ctx.intern(to_name), scope_set{m->scope()}),
        *b
      );
    else
      assert(!"Trying to import a nonexistent symbol");
  }

  if (!set.source->active())
    execute(ctx, set.source);
}

static void
check_all_defined(context& ctx, ptr<module_> m,
                  std::vector<std::string> const& names) {
  std::vector<std::string> undefined;
  for (std::string const& name : names)
    if (!m->find(ctx.intern(name)))
      undefined.push_back(name);

  if (!undefined.empty()) {
    if (undefined.size() == 1)
      throw std::runtime_error{fmt::format("Can't export undefined symbol {}",
                                           undefined.front())};
    else {
      std::string names = undefined.front();
      for (auto n = undefined.begin() + 1; n != undefined.end(); ++n)
        names += ", " + *n;
      throw std::runtime_error{fmt::format("Can't export undefined symbols: {}",
                                           names)};
    }
  }
}

tracked_ptr<module_>
instantiate(context& ctx, module_specifier const& pm) {
  auto result = make_tracked<module_>(ctx, ctx, pm.name);

  perform_imports(ctx, result, pm.imports);
  compile_module_body(ctx, result, pm);

  check_all_defined(ctx, result.get(), pm.exports);
  for (std::string const& name : pm.exports)
    result->export_(ctx.intern(name));

  return result;
}

void
import_all_exported(context& ctx,
                    tracked_ptr<module_> const& to,
                    tracked_ptr<module_> const& from) {
  import_set is{from, {}};

  for (std::string const& name : from->exports())
    is.names.emplace_back(name, name);

  perform_imports(ctx, to, is);
}

void
import_all_top_level(context& ctx,
                     tracked_ptr<module_> const& to,
                     tracked_ptr<module_> const& from) {
  import_set is{from, {}};

  for (std::string const& name : from->top_level_names())
    is.names.emplace_back(name, name);

  perform_imports(ctx, to, is);
}

void
perform_imports(context& ctx, tracked_ptr<module_> const& to,
                imports_list const& imports) {
  for (import_specifier const& spec : imports) {
    import_set const& set = parse_import_set(ctx, spec); // GC
    perform_imports(ctx, to, set);
  }
}

operand
define_top_level(context& ctx, std::string const& name, ptr<module_> m,
                 bool export_, ptr<> object) {
  auto index = ctx.add_top_level(object, name);
  auto name_sym = ctx.intern(name);
  auto var = std::make_shared<variable>(name, index);
  m->scope()->add(ctx.store,
                  make<syntax>(ctx, name_sym, scope_set{m->scope()}),
                  var);

  if (export_)
    m->export_(name_sym);

  return index;
}

static tracked_ptr<>
run_module(context& ctx, ptr<module_> m) {
  return call_with_continuation_barrier(ctx, m->top_level_procedure(), {});
}

tracked_ptr<>
execute(context& ctx, tracked_ptr<module_> const& mod) {
  tracked_ptr<> result = run_module(ctx, mod.get());
  mod->mark_active();

  return result;
}

} // namespace insider
