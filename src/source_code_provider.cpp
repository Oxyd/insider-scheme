#include "source_code_provider.hpp"

#include "context.hpp"
#include "port.hpp"

namespace insider {

std::optional<source_file>
filesystem_source_code_provider::find_file(context& ctx, std::filesystem::path const& path) {
  if (auto port = open_file_for_text_input(ctx, root_ / path))
    return source_file{unique_port_handle{track(ctx, port)}, {this, path}};
  else
    return std::nullopt;
}

void
virtual_filesystem_source_code_provider::add(std::filesystem::path const& path, std::string data) {
  files_.emplace(path.lexically_normal(), std::move(data));
}

std::optional<source_file>
virtual_filesystem_source_code_provider::find_file(context& ctx, std::filesystem::path const& path) {
  if (auto f = files_.find(path.lexically_normal()); f != files_.end())
    return source_file{unique_port_handle{track(ctx, make_string_input_port(ctx, f->second))},
                       {this, path}};
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
find_source_relative(context& ctx, source_file_origin origin, std::filesystem::path const& path) {
  return origin.provider->find_file(ctx, origin.path.replace_filename(path));
}

} // namespace insider
