#ifndef INSIDER_COMPILER_MODULE_SPECIFIER_HPP
#define INSIDER_COMPILER_MODULE_SPECIFIER_HPP

#include "compiler/module_name.hpp"
#include "compiler/source_file_origin.hpp"
#include "memory/root_provider.hpp"

#include <string>
#include <vector>
#include <memory>
#include <variant>
#include <optional>

namespace insider {

class syntax;

struct import_specifier {
  struct only {
    std::unique_ptr<import_specifier> from;
    std::vector<std::string>          identifiers;

    only() = default;

    only(std::unique_ptr<import_specifier> from,
         std::vector<std::string> identifiers)
      : from{std::move(from)}
      , identifiers{std::move(identifiers)}
    { }
  };

  struct except {
    std::unique_ptr<import_specifier> from;
    std::vector<std::string>          identifiers;

    except() = default;

    except(std::unique_ptr<import_specifier> from,
           std::vector<std::string> identifiers)
      : from{std::move(from)}
      , identifiers{std::move(identifiers)}
    { }
  };

  struct prefix {
    std::unique_ptr<import_specifier> from;
    std::string                       prefix_;

    prefix() = default;

    prefix(std::unique_ptr<import_specifier> from, std::string prefix)
      : from{std::move(from)}
      , prefix_{std::move(prefix)}
    { }
  };

  struct rename {
    std::unique_ptr<import_specifier> from;
    std::vector<std::tuple<std::string, std::string>> renames;

    rename() = default;

    rename(std::unique_ptr<import_specifier> from,
           std::vector<std::tuple<std::string, std::string>> renames)
      : from{std::move(from)}
      , renames{std::move(renames)}
    { }
  };

  using value_type = std::variant<
    module_name,
    only,
    except,
    prefix,
    rename
  >;

  value_type value;

  template <typename T>
  explicit
  import_specifier(T value) : value{std::move(value)} { }

  import_specifier(import_specifier const&);
  import_specifier(import_specifier&&) = default;

  import_specifier&
  operator = (import_specifier const&);

  import_specifier&
  operator = (import_specifier&&) = default;
};

using imports_list = std::vector<import_specifier>;

// Build an imports_list that simply imports everything from each of the given
// modules.
imports_list
import_modules(auto const&... names) {
  return {import_specifier{names}...};
}

// Metainformation about a module -- its name, list of imports and exports, plus
// its body as a list of unparsed data.
class module_specifier : root_provider {
public:
  std::optional<module_name> name;
  imports_list               imports;
  std::vector<std::string>   exports;
  std::vector<ptr<syntax>>   body;
  source_file_origin         origin;

  module_specifier(free_store& fs, source_file_origin origin)
    : root_provider{fs}
    , origin{std::move(origin)}
  { }

  module_specifier(free_store& fs,
                   std::optional<module_name> name,
                   std::vector<import_specifier> imports,
                   std::vector<std::string> exports,
                   std::vector<ptr<syntax>> body,
                   source_file_origin origin)
    : root_provider{fs}
    , name{std::move(name)}
    , imports{std::move(imports)}
    , exports{std::move(exports)}
    , body{std::move(body)}
    , origin{std::move(origin)}
  { }

  void
  visit_roots(member_visitor const& f) override {
    for (ptr<syntax>& s : body)
      f(s);
  }
};

} // namespace insider

#endif
