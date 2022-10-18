#ifndef INSIDER_COMPILER_AST_TRANSFORMS_HPP
#define INSIDER_COMPILER_AST_TRANSFORMS_HPP

#include "compiler/analysis_context.hpp"
#include "compiler/expression.hpp"

#include <vector>

namespace insider {

class context;

using pass = expression (*)(context&, expression, analysis_context);
using pass_list = std::vector<pass>;

extern pass_list const all_passes;
extern pass_list const no_optimisations;

expression
apply_passes(context&, expression, analysis_context, pass_list const&);

} // namespace insider

#endif
