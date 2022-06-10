#include "scope.hpp"

#include "compiler/variable.hpp"
#include "context.hpp"
#include "runtime/basic_types.hpp"
#include "runtime/syntax.hpp"

#include <ranges>
#include <utility>

namespace insider {

static std::vector<ptr<scope>>::iterator
insert_or_find(std::vector<ptr<scope>>& set, ptr<scope> env) {
  for (auto where = set.begin(); where != set.end(); ++where) {
    if (*where == env)
      return where;
    else if ((**where).id() > env->id()) {
      set.insert(where, env);
      return set.end();
    }
  }

  set.push_back(env);
  return set.end();
}

scope::scope(context& ctx, std::string desc)
  : description_{std::move(desc)}
  , id_{ctx.generate_scope_id()}
{ }

void
scope_set::add(ptr<scope> env) {
  insert_or_find(scopes_, env);
  assert(std::is_sorted(scopes_.begin(), scopes_.end(), scope_comparator{}));
}

void
scope_set::remove(ptr<scope> env) {
  scopes_.erase(std::remove(scopes_.begin(), scopes_.end(), env), scopes_.end());
  assert(std::is_sorted(scopes_.begin(), scopes_.end(), scope_comparator{}));
}

void
scope_set::flip(ptr<scope> env) {
  auto it = insert_or_find(scopes_, env);
  if (it != scopes_.end())
    // Was in the set, and it points to it.
    scopes_.erase(it);

  assert(std::is_sorted(scopes_.begin(), scopes_.end(), scope_comparator{}));
}

void
scope_set::visit_members(member_visitor const& f) {
  for (ptr<scope>& scope : scopes_)
    f(scope);
}

void
update_scope_set(scope_set& set, scope_set_operation op, ptr<scope> s) {
  switch (op) {
  case scope_set_operation::add:
    set.add(s);
    break;

  case scope_set_operation::remove:
    set.remove(s);
    break;

  case scope_set_operation::flip:
    set.flip(s);
    break;
  }
}

bool
scope_sets_subseteq(scope_set const& lhs, scope_set const& rhs) {
  return std::includes(rhs.begin(), rhs.end(), lhs.begin(), lhs.end(),
                       scope_comparator{});
}

bool
scope_sets_equal(scope_set const& lhs, scope_set const& rhs) {
  return std::equal(lhs.begin(), lhs.end(), rhs.begin(), rhs.end());
}

void
scope::add(free_store& store, ptr<syntax> identifier,
           std::shared_ptr<variable> var) {
  assert(is_identifier(identifier));

  if (is_redefinition(identifier, var))
    throw std::runtime_error{fmt::format("Redefinition of {}",
                                         identifier_name(identifier))};

  bindings_.emplace_back(binding{identifier, std::move(var)});
  store.notify_arc(this, identifier);
}

void
scope::add(free_store& store, ptr<syntax> identifier, ptr<transformer> tr) {
  assert(is_identifier(identifier));
  assert(tr != nullptr);

  if (is_redefinition(identifier, tr))
    throw std::runtime_error{fmt::format("Redefinition of {}",
                                         identifier_name(identifier))};

  bindings_.emplace_back(binding{identifier, tr});

  store.notify_arc(this, identifier);
  store.notify_arc(this, tr);
}

void
scope::replace(free_store& store, ptr<syntax> identifier,
               ptr<transformer> new_tr) {
  auto b = std::ranges::find_if(bindings_, [&] (binding const& b) {
    return b.id->get_symbol() == identifier->get_symbol();
  });
  assert(b != bindings_.end());

  if (b->transformer) {
    b->transformer = new_tr;
    store.notify_arc(this, new_tr);
  } else
    throw std::runtime_error{fmt::format(
      "Can't redefine {} as syntax", identifier->get_symbol()->value()
    )};
}

void
scope::visit_members(member_visitor const& f) {
  for (auto& b : bindings_) {
    f(b.id);
    f(b.transformer);
  }
}

static bool
binding_values_equal(scope::binding const& binding,
                     std::shared_ptr<variable> const& var) {
  return binding.variable == var;
}

static bool
binding_values_equal(scope::binding const& binding, ptr<transformer> tr) {
  return binding.transformer == tr;
}

bool
scope::is_redefinition(ptr<syntax> id, auto const& intended_value) const {
  return std::ranges::any_of(
    bindings_,
    [&] (binding const& b) {
      return b.id->get_symbol()->value() == id->get_symbol()->value()
             && scope_sets_equal(id->scopes(), b.id->scopes())
             && !binding_values_equal(b, intended_value);
    }
  );
}

void
add_binding(free_store& store, ptr<scope> sc, ptr<syntax> identifier,
            scope::binding const& b) {
  if (b.variable)
    sc->add(store, identifier, b.variable);
  else {
    assert(b.transformer);
    sc->add(store, identifier, b.transformer);
  }
}

static std::string
format_scope_set(scope_set const& set) {
  std::string result;
  bool first = true;
  for (ptr<scope> s : set) {
    if (!first)
      result += ", ";

    result += s->description();
    first = false;
  }

  return result;
}

void
define(free_store& fs, ptr<syntax> id, std::shared_ptr<variable> value) {
  id->scopes().back()->add(fs, id, std::move(value));
}

void
define(free_store& fs, ptr<syntax> id, ptr<transformer> value) {
  id->scopes().back()->add(fs, id, value);
}

namespace {
  struct lookup_result {
    ptr<insider::scope> scope;
    scope::binding      binding;
  };
}

static auto
candidate_bindings(ptr<scope> s, ptr<symbol> name, scope_set const& scopes) {
  return *s | std::views::filter([name, &scopes] (scope::binding const& b) {
    return b.id->get_symbol() == name
           && scope_sets_subseteq(b.id->scopes(), scopes);
  });
}

static void
throw_ambiguous_reference_error(
  ptr<symbol> id,
  scope_set const& envs,
  scope_set const& maximal_scope_set,
  scope_set const& ambiguous_other_candidate_set
) {
  throw make_error("Ambiguous reference to {}\n"
                   "  Reference scopes:     {};\n"
                   "  1st candidate scopes: {};\n"
                   "  2nd candidate scopes: {}.",
                   id->value(),
                   format_scope_set(envs),
                   format_scope_set(maximal_scope_set),
                   format_scope_set(ambiguous_other_candidate_set));
}

static std::optional<lookup_result>
lookup_scope_and_binding(ptr<symbol> id, scope_set const& envs) {
  std::optional<scope::binding> result;
  ptr<scope> binding_scope{};
  scope_set maximal_scope_set;
  std::optional<scope_set> ambiguous_other_candidate_set;

  for (ptr<scope> e : envs)
    for (scope::binding const& b : candidate_bindings(e, id, envs)) {
      scope_set const& binding_set = b.id->scopes();

      if (scope_sets_subseteq(maximal_scope_set, binding_set)) {
        ambiguous_other_candidate_set = std::nullopt;
        maximal_scope_set = binding_set;
        result = b;
        binding_scope = e;
      } else if (!scope_sets_subseteq(binding_set, maximal_scope_set))
        ambiguous_other_candidate_set = binding_set;
    }

  if (ambiguous_other_candidate_set)
    throw_ambiguous_reference_error(id,
                                    envs,
                                    maximal_scope_set,
                                    *ambiguous_other_candidate_set);

  if (result)
    return lookup_result{binding_scope, *result};
  else
    return std::nullopt;
}

void
redefine(free_store& fs, ptr<syntax> id, ptr<transformer> tr) {
  auto result = lookup_scope_and_binding(id->get_symbol(), id->scopes());
  assert(result);

  result->scope->replace(fs, id, tr);
}

std::optional<scope::binding>
lookup(ptr<symbol> id, scope_set const& envs) {
  if (auto result = lookup_scope_and_binding(id, envs))
    return result->binding;
  else
    return std::nullopt;
}

} // namespace insider
