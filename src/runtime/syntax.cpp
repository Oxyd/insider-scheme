#include "syntax.hpp"

#include "ptr.hpp"
#include "runtime/basic_types.hpp"
#include "util/define_procedure.hpp"
#include "util/object_conversions.hpp"

namespace insider {

bool
is_identifier(ptr<> x) {
  if (!is<syntax>(x))
    return false;
  else
    return assume<syntax>(x)->contains<symbol>();
}

std::string
identifier_name(ptr<syntax> x) {
  if (!is_identifier(x))
    throw make_syntax_error(x, "Expected identifier");
  else
    return x->get_symbol()->value();
}

std::optional<scope::value_type>
lookup(ptr<syntax> id) {
  return lookup(id->get_symbol(), id->scopes());
}


syntax::syntax(ptr<> expr)
  : expression_{expr}
{ }

syntax::syntax(ptr<> expr, source_location loc)
  : expression_{expr}
  , location_{std::move(loc)}
{
  assert(expr);
}

syntax::syntax(ptr<> expr, scope_set envs)
  : expression_{expr}
  , scopes_{std::move(envs)}
{ }

syntax::syntax(ptr<> expr, source_location loc, scope_set scopes,
               std::vector<update_record> update_records)
  : expression_{expr}
  , location_{std::move(loc)}
  , scopes_{std::move(scopes)}
  , update_records_{std::move(update_records)}
{ }

void
syntax::visit_members(member_visitor const& f) {
  f(expression_);
  scopes_.visit_members(f);
  for (update_record& ur : update_records_)
    f(ur.scope);
}

ptr<>
syntax::update_and_get_expression(context& ctx) {
  if (dirty())
    update_children(ctx);

  return expression_;
}

ptr<syntax>
syntax::update_scope(free_store& fs, ptr<scope> s,
                     scope_set_operation op) const {
  scope_set new_scopes = scopes_;
  update_scope_set(new_scopes, op, s);
  auto result = fs.make<syntax>(expression_, location_, std::move(new_scopes),
                                update_records_);
  result->update_records_.emplace_back(op, s);
  return result;
}

ptr<syntax>
syntax::add_scope(free_store& fs, ptr<scope> s) const {
  return update_scope(fs, s, scope_set_operation::add);
}

ptr<syntax>
syntax::remove_scope(free_store& fs, ptr<scope> s) const {
  return update_scope(fs, s, scope_set_operation::remove);
}

ptr<syntax>
syntax::flip_scope(free_store& fs, ptr<scope> s) const {
  return update_scope(fs, s, scope_set_operation::flip);
}

using update_record_list = std::vector<syntax::update_record>;

static ptr<syntax>
apply_update_records_to_syntax(
  context& ctx, ptr<syntax> stx, update_record_list const& records
) {
  std::vector<syntax::update_record> effective_update_records
    = stx->update_records();
  effective_update_records.insert(effective_update_records.end(),
                                  records.begin(), records.end());

  scope_set set = stx->scopes();
  for (syntax::update_record const& ur : records)
    update_scope_set(set, ur.operation, ur.scope);

  return make<syntax>(ctx, stx->get_expression_without_update(), stx->location(),
                      std::move(set), std::move(effective_update_records));
}

static ptr<>
apply_update_records_if_syntax(context& ctx, ptr<> x,
                               update_record_list const& records) {
  if (auto stx = match<syntax>(x))
    return apply_update_records_to_syntax(ctx, stx, records);
  else
    return x;
}

static ptr<>
apply_update_records_to_list(context& ctx, ptr<pair> head,
                             update_record_list const& records) {
  return map(ctx, head, [&] (ptr<> x) {
    return apply_update_records_if_syntax(ctx, x, records);
  });
}

static ptr<vector>
apply_update_records_to_vector(context& ctx, ptr<vector> v,
                               update_record_list const& records) {
  auto result = make<vector>(ctx, v->size(), ctx.constants->void_);
  for (std::size_t i = 0; i < v->size(); ++i)
    result->set(ctx.store, i,
                apply_update_records_if_syntax(ctx, v->ref(i), records));
  return result;
}

void
syntax::update_children(context& ctx) {
  assert(dirty());

  if (auto stx = match<syntax>(expression_))
    expression_ = apply_update_records_to_syntax(ctx, stx, update_records_);
  else if (auto p = match<pair>(expression_))
    expression_ = apply_update_records_to_list(ctx, p, update_records_);
  else if (auto v = match<vector>(expression_))
    expression_ = apply_update_records_to_vector(ctx, v, update_records_);

  ctx.store.notify_arc(this, expression_);
  update_records_.clear();
}

// Recurse through stx and create a new pair or vector for each pair or vector
// recursively contained in it. These new pairs and vectors are initialised to
// contain all nulls.
static std::unordered_map<ptr<>, ptr<>>
make_fresh_aggregates(context& ctx, ptr<syntax> stx) {
  std::unordered_map<ptr<>, ptr<>> result;
  std::vector<ptr<>> stack{stx};
  while (!stack.empty()) {
    ptr<> current = stack.back();
    stack.pop_back();

    if (auto stx = match<syntax>(current))
      current = stx->get_expression_without_update();

    if (auto p = match<pair>(current); p && !result.count(p)) {
      auto new_p = cons(ctx, ctx.constants->null, ctx.constants->null);
      result.emplace(p, new_p);

      stack.push_back(car(p));
      stack.push_back(cdr(p));
    } else if (auto v = match<vector>(current); v && !result.count(v)) {
      auto new_v = make<vector>(ctx, v->size(), ctx.constants->null);
      result.emplace(v, new_v);

      for (std::size_t i = 0; i < v->size(); ++i)
        stack.push_back(v->ref(i));
    }
  }

  return result;
}

static ptr<>
unwrap(ptr<> x) {
  if (auto stx = match<syntax>(x))
    return stx->get_expression_without_update();
  else
    return x;
}

static ptr<>
unwrapped_value(ptr<> x, std::unordered_map<ptr<>, ptr<>> const& aggregates) {
  x = unwrap(x);
  if (auto it = aggregates.find(x); it != aggregates.end())
    return it->second;
  else
    return x;
}

template <typename T>
auto
semisyntax_match_without_update(ptr<> x) {
  if (auto stx = match<syntax>(x))
    return match<T>(stx->get_expression_without_update());
  else
    return match<T>(x);
}

static void
unwrap_syntaxes(context& ctx, ptr<syntax> stx,
                std::unordered_map<ptr<>, ptr<>> const& aggregates) {
  std::vector<ptr<>> stack{stx};

  auto unwrap_aggregate = [&] <typename T> (ptr<T> aggregate) {
    ptr<T> result = assume<T>(aggregates.at(aggregate));
    for (std::size_t i = 0; i < aggregate->size(); ++i)
      if (result->ref(i) == ctx.constants->null) {
        result->set(ctx.store, i,
                    unwrapped_value(aggregate->ref(i), aggregates));
        stack.push_back(unwrap(aggregate->ref(i)));
      }
  };

  while (!stack.empty()) {
    ptr<> current = stack.back();
    stack.pop_back();

    if (auto p = semisyntax_match_without_update<pair>(current))
      unwrap_aggregate(p);
    else if (auto v = semisyntax_match_without_update<vector>(current))
      unwrap_aggregate(v);
  }
}

ptr<>
syntax_to_datum(context& ctx, ptr<syntax> stx) {
  auto aggregates = make_fresh_aggregates(ctx, stx);
  unwrap_syntaxes(ctx, stx, aggregates);

  ptr<> expr = stx->get_expression_without_update();
  if (auto it = aggregates.find(expr); it != aggregates.end())
    return it->second;
  else
    return expr;
}

ptr<syntax>
datum_to_syntax(context& ctx, ptr<syntax> s, ptr<> datum) {
  if (auto p = match<pair>(datum)) {
    ptr<syntax> head = datum_to_syntax(ctx, s, car(p));
    ptr<syntax> tail = datum_to_syntax(ctx, s, cdr(p));
    return make<syntax>(ctx, cons(ctx, head, tail), s->location(), s->scopes());
  } else if (auto v = match<vector>(datum)) {
    auto result_vec = make<vector>(ctx, v->size(), ctx.constants->void_);
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
    return ctx.constants->null;

  if (!is<pair>(stx)
      && (!is<syntax>(stx) || !syntax_is<pair>(assume<syntax>(stx))))
    return nullptr;

  ptr<pair> result = make<pair>(ctx,
                                car(semisyntax_assume<pair>(ctx, stx)),
                                ctx.constants->null);
  ptr<pair> tail = result;
  ptr<> datum = cdr(semisyntax_assume<pair>(ctx, stx));

  while (ptr<pair> p = semisyntax_match<pair>(ctx, datum)) {
    auto new_pair = make<pair>(ctx, car(p), ctx.constants->null);
    tail->set_cdr(ctx.store, new_pair);
    tail = new_pair;

    datum = cdr(p);
  }

  if (!semisyntax_is<null_type>(datum))
    return nullptr;

  return result;
}

void
transformer::visit_members(member_visitor const& f) {
  f(callable_);
}

static ptr<>
syntax_location(context& ctx, ptr<syntax> s) {
  source_location loc = s->location();
  return to_scheme_list(ctx, loc.file_name, loc.line, loc.column);
}

static bool
bound_identifier_eq(context& ctx, ptr<syntax> x, ptr<syntax> y) {
  if (!is_identifier(x) || !is_identifier(y))
    throw std::runtime_error{"Expected two identifiers"};

  if (x->update_and_get_expression(ctx) != y->update_and_get_expression(ctx))
    return false;

  return scope_sets_equal(x->scopes(), y->scopes());
}

static ptr<>
syntax_scopes(context& ctx, ptr<syntax> s) {
  return make_list_from_vector(ctx, s->scopes().data());
}

static ptr<syntax>
syntax_add_scope(context& ctx, ptr<syntax> stx, ptr<scope> s) {
  return stx->add_scope(ctx.store, s);
}

static ptr<>
syntax_to_list_proc(context& ctx, ptr<> stx) {
  if (ptr<> r = syntax_to_list(ctx, stx))
    return r;
  else
    return ctx.constants->f;
}

static bool
free_identifier_eq(ptr<syntax> x, ptr<syntax> y) {
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
}

void
export_syntax(context& ctx, module_& result) {
  define_procedure<&syntax::update_and_get_expression>(ctx, "syntax-expression",
                                                       result, true);
  define_procedure<syntax_scopes>(ctx, "syntax-scopes", result, true);
  define_procedure<syntax_add_scope>(ctx, "syntax-add-scope", result, true);
  define_procedure<syntax_to_datum>(ctx, "syntax->datum", result, true);
  define_procedure<syntax_to_list_proc>(ctx, "syntax->list", result, true);
  define_procedure<datum_to_syntax>(ctx, "datum->syntax", result, true);
  define_procedure<free_identifier_eq>(ctx, "free-identifier=?", result, true);
  define_procedure<bound_identifier_eq>(ctx, "bound-identifier=?", result, true);
  define_procedure<syntax_location>(ctx, "syntax-location", result, true);
}

} // namespace insider
