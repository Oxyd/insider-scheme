#ifndef INSIDER_COMPILER_PARSER_EXPANDER_HPP
#define INSIDER_COMPILER_PARSER_EXPANDER_HPP

#include "compiler/expression.hpp"
#include "compiler/parsing_context.hpp"
#include "memory/root.hpp"

#include <memory>

namespace insider {

// The parser/expander turns syntax objects into the internal representation
// defined in ast.hpp. In Scheme, macro expansion and parsing can't really be
// separated since they are fundamentally interleaved, thus this module does
// both.

class module_;
class syntax;

expression
parse(parsing_context& pc, ptr<syntax> s);

std::vector<ptr<syntax>>
expand_top_level(parsing_context& pc, root_ptr<module_> const& m,
                 std::vector<ptr<syntax>> const& exprs);

} // namespace insider

#endif
