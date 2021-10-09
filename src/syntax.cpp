#include "syntax.hpp"

#include "basic_types.hpp"
#include "define_procedure.hpp"
#include "object_conversions.hpp"
#include "ptr.hpp"

namespace insider {

bool
is_identifier(ptr<> x) {
  if (!is<syntax>(x))
    return false;
  return is<symbol>(assume<syntax>(x)->expression());
}

std::string
identifier_name(ptr<syntax> x) {
  return syntax_expect<symbol>(x)->value();
}

std::optional<scope::value_type>
lookup(ptr<syntax> id) {
  assert(is_identifier(id));
  return lookup(assume<symbol>(id->expression()), id->scopes());
}

void
syntax::visit_members(member_visitor const& f) {
  f(expression_);

  for (ptr<scope>& env : scopes_)
    f(env);
}

void
syntax::add_scope(free_store& fs, ptr<scope> s) {
  bool added = insider::add_scope(scopes_, s);
  if (added)
    fs.notify_arc(this, s);
}

void
syntax::remove_scope(ptr<scope> s) {
  insider::remove_scope(scopes_, s);
}

void
syntax::flip_scope(free_store& fs, ptr<scope> s) {
  bool added = insider::flip_scope(scopes_, s);
  if (added)
    fs.notify_arc(this, s);
}

static ptr<>
syntax_to_datum_helper(context& ctx, ptr<> o) {
  if (o == ctx.constants->null.get()) {
    return o;
  } else if (auto p = semisyntax_match<pair>(o)) {
    return cons(ctx, syntax_to_datum_helper(ctx, car(p)), syntax_to_datum_helper(ctx, cdr(p)));
  } else if (auto v = semisyntax_match<vector>(o)) {
    auto result = make<vector>(ctx, ctx, v->size());
    for (std::size_t i = 0; i < v->size(); ++i)
      result->set(ctx.store, i, syntax_to_datum_helper(ctx, v->ref(i)));
    return result;
  } else if (auto stx = match<syntax>(o)) {
    return stx->expression();
  } else {
    return o;
  }
}

ptr<>
syntax_to_datum(context& ctx, ptr<syntax> stx) {
  return syntax_to_datum_helper(ctx, stx);
}

ptr<syntax>
datum_to_syntax(context& ctx, ptr<syntax> s, ptr<> datum) {
  if (auto p = match<pair>(datum)) {
    ptr<syntax> head = datum_to_syntax(ctx, s, car(p));
    ptr<syntax> tail = datum_to_syntax(ctx, s, cdr(p));
    return make<syntax>(ctx, cons(ctx, head, tail), s->location(), s->scopes());
  } else if (auto v = match<vector>(datum)) {
    auto result_vec = make<vector>(ctx, ctx, v->size());
    for (std::size_t i = 0; i < v->size(); ++i)
      result_vec->set(ctx.store, i, datum_to_syntax(ctx, s, v->ref(i)));
    return make<syntax>(ctx, result_vec, s->location(), s->scopes());
  } else if (auto stx = match<syntax>(datum)) {
    return stx;
  } else
    return make<syntax>(ctx, datum, s->location(), s->scopes());
}

ptr<>
syntax_to_list(context& ctx, ptr<> stx) {
  if (semisyntax_is<null_type>(stx))
    return ctx.constants->null.get();

  if (!is<pair>(stx) && (!is<syntax>(stx) || !syntax_is<pair>(assume<syntax>(stx))))
    return nullptr;

  ptr<pair> result = make<pair>(ctx, car(semisyntax_assume<pair>(stx)), ctx.constants->null.get());
  ptr<pair> tail = result;
  ptr<> datum = cdr(semisyntax_assume<pair>(stx));

  while (ptr<pair> p = semisyntax_match<pair>(datum)) {
    auto new_pair = make<pair>(ctx, car(p), ctx.constants->null.get());
    tail->set_cdr(ctx.store, new_pair);
    tail = new_pair;

    datum = cdr(p);
  }

  if (!semisyntax_is<null_type>(datum))
    return nullptr;

  return result;
}

static ptr<>
copy_syntax_helper(context& ctx, ptr<> o) {
  if (auto stx = match<syntax>(o))
    return make<syntax>(ctx, copy_syntax_helper(ctx, stx->expression()), stx->location(), stx->scopes());
  else if (auto p = match<pair>(o))
    return make<pair>(ctx, copy_syntax_helper(ctx, car(p)), copy_syntax_helper(ctx, cdr(p)));
  else if (auto v = match<vector>(o)) {
    auto new_v = make<vector>(ctx, ctx, v->size());
    for (std::size_t i = 0; i < v->size(); ++i)
      new_v->set(ctx.store, i, copy_syntax_helper(ctx, v->ref(i)));
    return new_v;
  } else
    return o;
}

ptr<syntax>
copy_syntax(context& ctx, ptr<syntax> stx) {
  return assume<syntax>(copy_syntax_helper(ctx, stx));
}

void
export_syntax(context& ctx, module_& result) {
  define_procedure(ctx, "syntax-expression", result, true, &syntax::expression);
  define_procedure(ctx, "syntax-scopes", result, true,
                   [] (context& ctx, ptr<syntax> s) {
                     return make_list_from_vector(ctx, s->scopes());
                   });
  define_procedure(ctx, "syntax-add-scope!", result, true,
                   [] (context& ctx, ptr<syntax> stx, ptr<scope> s) {
                     stx->add_scope(ctx.store, s);
                   });

  define_procedure(ctx, "syntax->datum", result, true, syntax_to_datum);
  define_procedure(ctx, "syntax->list", result, true,
                   [] (context& ctx, ptr<> stx) -> ptr<> {
                     if (ptr<> r = syntax_to_list(ctx, stx))
                       return r;
                     else
                       return ctx.constants->f.get();
                   });

  define_procedure(ctx, "datum->syntax", result, true,
                   [] (context& ctx, ptr<syntax> s, ptr<> datum) {
                     return datum_to_syntax(ctx, s, datum);
                   });

  define_procedure(ctx, "free-identifier=?", result, true,
                   [] (ptr<syntax> x, ptr<syntax> y) {
                     if (!is_identifier(x) || !is_identifier(y))
                       throw std::runtime_error{"Expected two identifiers"};

                     auto x_binding = lookup(x);
                     auto y_binding = lookup(y);

                     if (x_binding && y_binding)
                       return *x_binding == *y_binding;
                     else if (!x_binding && !y_binding)
                       return identifier_name(x) == identifier_name(y);
                     else
                       return false;
                   });

  define_procedure(ctx, "bound-identifier=?", result, true,
                   [] (ptr<syntax> x, ptr<syntax> y) {
                     if (!is_identifier(x) || !is_identifier(y))
                       throw std::runtime_error{"Expected two identifiers"};

                     if (x->expression() != y->expression())
                       return false;

                     scope_set x_scopes = x->scopes();
                     scope_set y_scopes = y->scopes();

                     if (x_scopes.size() != y_scopes.size())
                       return false;

                     std::sort(x_scopes.begin(), x_scopes.end());
                     std::sort(y_scopes.begin(), y_scopes.end());

                     return x_scopes == y_scopes;
                   });
}

} // namespace insider
