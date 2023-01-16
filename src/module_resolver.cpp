#include "module_resolver.hpp"

#include "compiler/analyser.hpp"
#include "compiler/compilation_config.hpp"
#include "compiler/source_code_provider.hpp"
#include "io/read.hpp"

#include <ranges>

namespace insider {

module_resolver::module_resolver(context& ctx)
  : root_provider{ctx.store}
{ }

// Don't require the header to include the definition of source_code_provider.
module_resolver::~module_resolver() = default;

bool
module_resolver::knows_module(context& ctx, module_name const& name) {
  using namespace std::literals;

  if (name == std::vector{"insider"s, "internal"s})
    return true;
  else if (auto mod_it = modules_.find(name); mod_it != modules_.end())
    return true;
  else
    return any_provider_has_module(ctx, name);
}

ptr<module_>
module_resolver::find_module(context& ctx, module_name const& name,
                             compilation_config const& config) {
  using namespace std::literals;

  if (name == std::vector{"insider"s, "internal"s})
    return internal_module_;
  else if (auto mod_it = modules_.find(name); mod_it != modules_.end()) {
    if (!mod_it->second)
      throw std::runtime_error{fmt::format("Module {} depends on itself",
                                           module_name_to_string(name))};

    return mod_it->second;
  } else
    return load_module(ctx, name, config);
}

void
module_resolver::prepend_source_code_provider(
  std::unique_ptr<source_code_provider> provider
) {
  source_providers_.insert(source_providers_.begin(), std::move(provider));
}

void
module_resolver::append_source_code_provider(
  std::unique_ptr<source_code_provider> provider
) {
  source_providers_.push_back(std::move(provider));
}

void
module_resolver::visit_roots(member_visitor const& f) {
  f(internal_module_);
  for (auto& [name, m] : modules_)
    f(m);
}

static std::optional<source_file>
find_module_in_provider(context& ctx,
                        source_code_provider& provider,
                        module_name const& name) {
  std::filesystem::path path = module_name_to_path(name);
  std::array<std::filesystem::path, 2> candidates{
    path.replace_extension(".sld"),
    path.replace_extension(".scm")
  };
  for (auto const& candidate : candidates)
    if (auto source = provider.find_file(ctx, candidate))
      if (read_library_name(ctx, source->port.get().get()) == name) {
        source->port->rewind();
        return source;
      }

  return std::nullopt;
}

bool
module_resolver::any_provider_has_module(context& ctx,
                                         module_name const& name) {
  return std::ranges::any_of(
    source_providers_,
    [&] (std::unique_ptr<source_code_provider> const& provider) {
      return find_module_in_provider(
        ctx, *provider, name
      ).has_value();
    }
  );
}

module_specifier
module_resolver::find_module_in_providers(context& ctx,
                                          module_name const& name) {
  for (auto const& provider : source_providers_)
    if (auto source = find_module_in_provider(ctx, *provider, name))
      return read_module(ctx, read_syntax_multiple(ctx,
                                                   source->port.get().get()),
                         source->origin);

  throw std::runtime_error{fmt::format("Unknown module {}",
                                       module_name_to_string(name))};
}

ptr<module_>
module_resolver::load_module(context& ctx, module_name const& name,
                             compilation_config const& config) {
  modules_.emplace(name, nullptr);
  tracked_ptr<module_> m = instantiate(
    ctx,
    find_module_in_providers(ctx, name),
    config
  );
  modules_.find(name)->second = m.get();
  return m.get();
}

} // insider
