#ifndef INSIDER_COMPILER_COMPILATION_CONFIG_HPP
#define INSIDER_COMPILER_COMPILATION_CONFIG_HPP

#include "compiler/expression.hpp"

#include <vector>

namespace insider {

class parsing_context;

using pass = expression (*)(parsing_context&, expression);
using pass_list = std::vector<pass>;

struct compilation_config {
  pass_list passes;

  explicit
  compilation_config(pass_list passes)
    : passes{std::move(passes)}
  { }

  compilation_config(compilation_config const&) = delete;
  void operator = (compilation_config const&) = delete;

  static compilation_config
  optimisations_config();

  static compilation_config
  debug_config();
};

} // namespace insider

#endif
