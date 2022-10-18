#ifndef INSIDER_COMPILER_CLONE_AST_HPP
#define INSIDER_COMPILER_CLONE_AST_HPP

#include "compiler/expression.hpp"

#include <optional>
#include <string>

namespace insider {

class context;

expression
clone_ast(context& ctx, expression e,
          std::optional<std::string> procedure_name_to_append = {});

template <typename T>
static ptr<T>
clone_ast(context& ctx, ptr<T> e,
          std::optional<std::string> procedure_name_to_append = {}) {
  return assume<T>(clone_ast(ctx, expression{e},
                             std::move(procedure_name_to_append)));
}

} // namespace insider

#endif
