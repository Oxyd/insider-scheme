#include "module.hpp"

#include "compiler/analyser.hpp"
#include "compiler/compiler.hpp"
#include "compiler/module_name.hpp"
#include "compiler/module_specifier.hpp"
#include "compiler/variable.hpp"
#include "context.hpp"
#include "io/read.hpp"
#include "memory/tracked_ptr.hpp"
#include "runtime/basic_types.hpp"
#include "runtime/parameter_map.hpp"
#include "runtime/symbol.hpp"
#include "runtime/syntax.hpp"
#include "util/define_procedure.hpp"
#include "vm/vm.hpp"

#include <ranges>

namespace insider {

module_::module_(context& ctx, std::optional<module_name> name)
  : env_{make<insider::scope>(ctx, ctx,
                              fmt::format("{} module top-level",
                                          name
                                            ? module_name_to_string(*name)
                                            : "<unnamed module>"))}
  , name_{std::move(name)}
{ }

module_::module_(context& ctx, type t)
  : module_{ctx}
{
  type_ = t;
}

auto
module_::find(ptr<symbol> identifier) const -> std::optional<binding_type> {
  if (auto binding = lookup(identifier, scope_set{env_}))
    return binding;

  return std::nullopt;
}

void
module_::import_(context& ctx, ptr<symbol> identifier, binding_type b) {
  if (auto v = find(identifier)) {
    if (binding_targets_equal(b, *v))
      return; // Re-importing the same variable under the same name is OK.
    else
      throw std::runtime_error{fmt::format("Redefinition of {}",
                                           identifier->value())};
  }

  if (b.variable)
    env_->add(ctx.store, make<syntax>(ctx, identifier, scope_set{env_}),
              b.variable);
  else
    env_->add(ctx.store, make<syntax>(ctx, identifier, scope_set{env_}),
              b.transformer);
}

ptr<procedure>
module_::top_level_procedure() const {
  return proc_;
}

void
module_::export_(ptr<symbol> name) {
  assert(find(name));
  exports_.emplace(name->value());
}

void
module_::visit_members(member_visitor const& f) const {
  f(env_);
  f(proc_);
}

namespace {
  struct import_set {
    struct imported_name {
      std::string target;
      std::string source;
    };

    tracked_ptr<module_>       source;
    std::vector<imported_name> names;

    explicit
    import_set(context& ctx)
      : source{ctx.store}
    { }

    import_set(tracked_ptr<module_> source, std::vector<imported_name> names)
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
                       return set_name.target == name;
                     }))
      throw unbound_variable_error{fmt::format("Identifier {} is not exported",
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
                                      name.target) == o->identifiers.end();
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
                                      name.target) != e->identifiers.end();
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
      import_binding(
        ctx.store,
        m->scope(),
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
    else
      throw std::runtime_error{fmt::format("Can't export undefined symbols: {}",
                                           fmt::join(undefined, ", "))};
  }
}

static std::vector<std::string>
exports_list_to_exported_names(std::vector<import_set> const& import_sets,
                               exports_list const& list) {
  std::vector<std::string> result;
  for (export_specifier const& spec : list) {
    if (auto const* n = std::get_if<export_specifier::name>(&spec.value))
      result.push_back(n->identifier);
    else if (auto const* aif
             = std::get_if<export_specifier::all_imported_from>(&spec.value)) {
      for (import_set const& is : import_sets)
        if (is.source->name() == aif->module)
          for (auto const& name : is.names)
            result.push_back(name.target);
    } else
      assert(!"Unhandled export specifier");
  }

  return result;
}

static std::vector<import_set>
imports_list_to_import_sets(context& ctx, imports_list const& imports) {
  std::vector<import_set> result;
  result.reserve(imports.size());

  for (import_specifier const& spec : imports)
    result.push_back(parse_import_set(ctx, spec));

  return result;
}

static void
perform_imports(context& ctx, tracked_ptr<module_> const& to,
                std::vector<import_set> const& import_sets) {
  for (import_set const& set : import_sets)
    perform_imports(ctx, to, set);
}

tracked_ptr<module_>
instantiate(context& ctx, module_specifier const& pm) {
  auto result = make_tracked<module_>(ctx, ctx, pm.name);

  std::vector<import_set> import_sets
    = imports_list_to_import_sets(ctx, pm.imports);

  perform_imports(ctx, result, import_sets);
  compile_module_body(ctx, result, pm);

  auto exported_names = exports_list_to_exported_names(import_sets, pm.exports);
  check_all_defined(ctx, result.get(), exported_names);
  for (std::string const& name : exported_names)
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

static auto
top_level_names(ptr<module_> m) {
  return
    *m->scope()
      | std::views::filter([] (scope::binding const& b) {
        return b.id->scopes().size() == 1;
      })
      | std::views::transform([] (scope::binding const& b) {
        return b.id->get_symbol()->value();
      });
}

void
import_all_top_level(context& ctx,
                     tracked_ptr<module_> const& to,
                     tracked_ptr<module_> const& from) {
  import_set is{from, {}};

  for (std::string const& name : top_level_names(from.get()))
    is.names.emplace_back(name, name);

  perform_imports(ctx, to, is);
}

void
perform_imports(context& ctx, tracked_ptr<module_> const& to,
                imports_list const& imports) {
  for (import_specifier const& spec : imports) {
    import_set set = parse_import_set(ctx, spec); // GC
    perform_imports(ctx, to, set);
  }
}

static operand
define_top_level(context& ctx, std::string const& name, ptr<module_> m,
                 bool export_, ptr<> object, bool mutable_) {
  auto index = mutable_
               ? ctx.add_top_level_mutable(object, name)
               : ctx.add_top_level(object, name);
  auto name_sym = ctx.intern(name);
  auto var = make<top_level_variable>(ctx, name, index);
  m->scope()->add(ctx.store,
                  make<syntax>(ctx, name_sym, scope_set{m->scope()}),
                  var);

  if (export_)
    m->export_(name_sym);

  if (mutable_)
    var->flags().is_set = true;

  return index;
}

operand
define_top_level(context& ctx, std::string const& name, ptr<module_> m,
                 bool export_, ptr<> object) {
  return define_top_level(ctx, name, m, export_, object, false);
}

operand
define_top_level_mutable(context& ctx, std::string const& name, ptr<module_> m,
                         bool export_, ptr<> object) {
  return define_top_level(ctx, name, m, export_, object, true);
}

static ptr<>
run_module(vm& state, ptr<module_> m) {
  return call_with_continuation_barrier(state, m->top_level_procedure(), {});
}

ptr<>
execute(vm& state, tracked_ptr<module_> const& mod) {
  ptr<> result = run_module(state, mod.get());
  mod->mark_active();
  return result;
}

ptr<>
execute(context& ctx, tracked_ptr<module_> const& mod) {
  vm state{ctx};
  return execute(state, mod);
}

static imports_list
parse_imports(context& ctx, object_span imports) {
  imports_list result;
  for (ptr<> import : imports)
    result.push_back(
      parse_import_specifier(ctx, datum_to_syntax(ctx, {}, import))
    );
  return result;
}

static ptr<>
environment(context& ctx, ptr<native_procedure>, object_span args) {
  auto result = make_tracked<module_>(ctx, ctx, module_::type::immutable);
  perform_imports(ctx, result, parse_imports(ctx, args));
  return result.get();
}

tracked_ptr<module_>
make_interactive_module(context& ctx, imports_list const& imports) {
  auto result = make_tracked<module_>(ctx, ctx, module_::type::interactive);
  perform_imports(ctx, result, imports);
  return result;
}

static ptr<>
interactive_environment(context& ctx, ptr<native_procedure>, object_span args) {
  return make_interactive_module(ctx, parse_imports(ctx, args)).get();
}

static ptr<>
dynamic_import(context& ctx, ptr<native_procedure>, object_span args) {
  require_arg_count(args, 2);
  auto m = expect<module_>(args[0]);
  auto imports = parse_imports(ctx, args.subspan(1));
  perform_imports(ctx, track(ctx, m), imports);
  return ctx.constants->void_;
}

tracked_ptr<module_>
interaction_environment(context& ctx) {
  auto import_datum = ctx.parameters->find_value(
    ctx.constants->interaction_environment_specifier_tag
  );
  auto import_stx = datum_to_syntax(ctx, {}, import_datum);
  import_specifier import_spec = parse_import_specifier(ctx, import_stx);
  return make_interactive_module(ctx, {import_spec});
}

void
export_module(context& ctx, ptr<module_> result) {
  ctx.constants->interaction_environment_specifier_tag
    = create_parameter_tag(ctx, read(ctx, "(insider internal)"));
  define_top_level(ctx, "interaction-environment-specifier-tag", result, true,
                   ctx.constants->interaction_environment_specifier_tag);
  define_raw_procedure<environment>(ctx, "environment", result);
  define_raw_procedure<interactive_environment>(ctx, "interactive-environment",
                                                result);
  define_raw_procedure<dynamic_import>(ctx, "dynamic-import", result);
}

} // namespace insider
