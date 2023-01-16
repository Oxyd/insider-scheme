#ifndef INSIDER_COMPILER_AST_TRANSFORMS_HPP
#define INSIDER_COMPILER_AST_TRANSFORMS_HPP

#include "compiler/compilation_config.hpp"
#include "compiler/expression.hpp"
#include "compiler/parsing_context.hpp"

#include <vector>

namespace insider {

extern pass_list const all_passes;
extern pass_list const no_optimisations;

expression
apply_passes(parsing_context&, expression);

} // namespace insider

#endif
