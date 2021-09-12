#ifndef INSIDER_SOURCE_CODE_PROVIDER_HPP
#define INSIDER_SOURCE_CODE_PROVIDER_HPP

#include "expression.hpp"
#include "port.hpp"
#include "ptr.hpp"
#include "source_file_origin.hpp"

#include <filesystem>
#include <optional>
#include <unordered_map>
#include <vector>

namespace insider {

class context;
class syntax;

struct source_file {
  unique_port_handle<ptr<textual_input_port>> port;
  source_file_origin origin;
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

// Source code provider that looks for files within a given directory and its
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

// Source code provider that implements a key-value store of source
// files. Useful for tests.
class virtual_filesystem_source_code_provider : public source_code_provider {
public:
  void
  add(std::filesystem::path const&, std::string);

  std::optional<source_file>
  find_file(context&, std::filesystem::path const&) override;

private:
  struct hash {
    std::size_t
    operator () (std::filesystem::path const& p) const { return std::filesystem::hash_value(p); }
  };

  std::unordered_map<std::filesystem::path, std::string, hash> files_;
};

std::filesystem::path
module_name_to_path(module_name const&);

std::optional<source_file>
find_source_relative(context&, source_file_origin, std::filesystem::path const&);

} // namespace insider

#endif
