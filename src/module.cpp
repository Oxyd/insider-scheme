#include "module.hpp"

#include "analyser.hpp"
#include "basic_types.hpp"
#include "compiler.hpp"
#include "context.hpp"
#include "io.hpp"
#include "port.hpp"
#include "vm.hpp"

#ifdef WIN32
#include <tchar.h>
#else
#define _T(x) x
#endif

namespace insider {

module::module(context& ctx)
  : env_{make_tracked<insider::scope>(ctx, "module top-level")}
{ }

auto
module::find(ptr<symbol> identifier) const -> std::optional<binding_type> {
  if (auto binding = lookup(identifier, {env_.get()}))
    return binding;

  return std::nullopt;
}

void
module::import_(context& ctx, ptr<symbol> identifier, binding_type b) {
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
module::top_level_procedure() const {
  return proc_.get();
}

void
module::export_(ptr<symbol> name) {
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

std::string
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
define_top_level(context& ctx, std::string const& name, module& m, bool export_, ptr<> object) {
  auto index = ctx.add_top_level(object, name);
  auto name_sym = ctx.intern(name);
  auto var = std::make_shared<variable>(name, index);
  m.scope()->add(ctx.store, make<syntax>(ctx, name_sym, scope_set{m.scope()}), var);

  if (export_)
    m.export_(name_sym);

  return index;
}

static tracked_ptr<>
run_module(context& ctx, module& m) {
  return call_with_continuation_barrier(ctx, m.top_level_procedure(), {});
}

tracked_ptr<>
execute(context& ctx, module& mod) {
  tracked_ptr<> result = run_module(ctx, mod);
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

} // namespace insider
