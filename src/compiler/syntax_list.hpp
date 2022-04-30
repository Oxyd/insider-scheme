#ifndef INSIDER_COMPILER_SYNTAX_LIST_HPP
#define INSIDER_COMPILER_SYNTAX_LIST_HPP

#include "runtime/basic_types.hpp"
#include "runtime/syntax.hpp"

namespace insider {

inline ptr<syntax>
syntax_car(context& ctx, ptr<> stx) {
  return expect<syntax>(car(semisyntax_expect<pair>(ctx, stx)));
}

inline ptr<>
syntax_cdr(context& ctx, ptr<> stx) {
  return cdr(semisyntax_expect<pair>(ctx, stx));
}

inline ptr<syntax>
syntax_cadr(context& ctx, ptr<> stx) {
  return expect<syntax>(car(semisyntax_expect<pair>(ctx, syntax_cdr(ctx, stx))));
}

} // namespace insider

#endif
