#include "source_code_provider.hpp"

#include "context.hpp"
#include "io/port.hpp"
#include "util/define_procedure.hpp"

namespace insider {

null_source_code_provider null_source_code_provider_instance;

std::optional<source_file>
filesystem_source_code_provider::find_file(context& ctx,
                                           std::filesystem::path const& path) {
  if (auto port = open_file_for_text_input(ctx, root_ / path))
    return source_file{{ctx.store.root_list(), unique_port_handle{port}},
                       {this, path.parent_path()}};
  else
    return std::nullopt;
}

void
virtual_filesystem_source_code_provider::add(std::filesystem::path const& path,
                                             std::string data) {
  files_.emplace(path.lexically_normal(), std::move(data));
}

std::optional<source_file>
virtual_filesystem_source_code_provider::find_file(
  context& ctx,
  std::filesystem::path const& path
) {
  if (auto f = files_.find(path.lexically_normal()); f != files_.end())
    return source_file{
      {ctx.store.root_list(),
       unique_port_handle{open_input_string(ctx, f->second)}},
      {this, path.parent_path()}
    };
  else
    return std::nullopt;
}

std::filesystem::path
module_name_to_path(module_name const& name) {
  std::filesystem::path result;
  for (std::string const& component : name)
    result /= component;
  return result;
}

std::optional<source_file>
find_source_relative(context& ctx, source_file_origin origin,
                     std::filesystem::path const& path) {
  return origin.provider->find_file(ctx, origin.path / path);
}

static ptr<>
open_source_file_relative(context& ctx,
                          ptr<opaque_value<source_file_origin>> origin,
                          std::filesystem::path const& path) {
  if (auto f = find_source_relative(ctx, origin->value, path))
    return f->port.get().release();
  else
    return ctx.constants->f;
}

void
export_source_code_provider(context& ctx, ptr<module_> result) {
  define_procedure<open_source_file_relative>(ctx, "open-source-file-relative",
                                              result);
}

} // namespace insider
