#ifndef INSIDER_COMPILER_COMPILATION_CONFIG_HPP
#define INSIDER_COMPILER_COMPILATION_CONFIG_HPP

#include "compiler/expression.hpp"
#include "compiler/source_location.hpp"

#include <memory>
#include <unordered_set>
#include <vector>

namespace insider {

class parsing_context;

using pass = expression (*)(parsing_context&, expression);
using pass_list = std::vector<pass>;

class diagnostic_sink {
public:
  virtual
  ~diagnostic_sink() = default;

  void
  show(source_location const& location, std::string const& message);

private:
  std::unordered_set<source_location> emitted_locations_;

  virtual void
  output(source_location const&, std::string const&) = 0;
};

class null_diagnostic_sink final : public diagnostic_sink {
public:
  static null_diagnostic_sink instance;

private:
  void
  output(source_location const&, std::string const&) override { }
};

class stdout_diagnostic_sink final : public diagnostic_sink {
  void
  output(source_location const&, std::string const&) override;
};

struct compilation_config {
  pass_list        passes;
  diagnostic_sink& diagnostics;

  explicit
  compilation_config(pass_list passes, diagnostic_sink& diagnostics)
    : passes{std::move(passes)}
    , diagnostics{diagnostics}
  { }

  static compilation_config
  optimisations_config(
    diagnostic_sink& diagnostics = null_diagnostic_sink::instance
  );

  static compilation_config
  debug_config(diagnostic_sink& diagnostics = null_diagnostic_sink::instance);
};

} // namespace insider

#endif
