#ifndef INSIDER_SOURCE_CODE_PROVIDER_HPP
#define INSIDER_SOURCE_CODE_PROVIDER_HPP

#include "expression.hpp"
#include "port.hpp"
#include "ptr.hpp"

#include <filesystem>
#include <optional>
#include <vector>

namespace insider {

class context;
class syntax;

class source_code_provider;

struct source_file {
  unique_port_handle<ptr<textual_input_port>> port;
  source_code_provider* provider;
  std::filesystem::path path;
};

// Interface for source code providers. A source code provider is used when a
// file is requested that isn't currently known in the given context. The
// registered providers are then tried in order until one of them successfully
// provides the library.
class source_code_provider {
public:
  virtual
  ~source_code_provider() = default;

  // Try to provide the module with the given name. This function must only
  // return either nullopt or a library with the specified name.
  virtual std::optional<source_file>
  find_file(context&, std::filesystem::path const&) = 0;
};

// Module provider that looks for files within a given directory and its
// subdirectories for libraries.
class filesystem_source_code_provider : public source_code_provider {
public:
  explicit
  filesystem_source_code_provider(std::filesystem::path root) : root_{std::move(root)} { }

  std::optional<source_file>
  find_file(context&, std::filesystem::path const&) override;

private:
  std::filesystem::path root_;
};

std::filesystem::path
module_name_to_path(module_name const&);

} // namespace insider

#endif
