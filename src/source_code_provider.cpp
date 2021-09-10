#include "source_code_provider.hpp"

#include "port.hpp"

namespace insider {

std::optional<source_file>
filesystem_source_code_provider::find_file(context& ctx, std::filesystem::path const& path) {
  if (auto port = open_file_for_text_input(ctx, root_ / path))
    return source_file{unique_port_handle{port}, this, path};
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

} // namespace insider
