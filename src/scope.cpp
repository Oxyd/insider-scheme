#include "scope.hpp"

#include "basic_types.hpp"
#include "context.hpp"
#include "syntax.hpp"

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
  auto elem = insert_or_find(scopes_, env);
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
scope::add(free_store& store, ptr<syntax> identifier, std::shared_ptr<variable> var) {
  assert(is_identifier(identifier));

  if (is_redefinition(identifier, var))
    throw std::runtime_error{fmt::format("Redefinition of {}", identifier_name(identifier))};

  bindings_.emplace_back(binding{identifier, std::move(var)});
  store.notify_arc(this, identifier);
}

void
scope::add(free_store& store, ptr<syntax> identifier, ptr<transformer> tr) {
  assert(is_identifier(identifier));
  assert(tr != nullptr);

  if (is_redefinition(identifier, tr))
    throw std::runtime_error{fmt::format("Redefinition of {}", identifier_name(identifier))};

  bindings_.emplace_back(binding{identifier, tr});

  store.notify_arc(this, identifier);
  store.notify_arc(this, tr);
}

void
scope::add(free_store& store, ptr<syntax> identifier, value_type const& value) {
  if (auto var = std::get_if<std::shared_ptr<variable>>(&value))
    add(store, identifier, *var);
  else
    add(store, identifier, std::get<ptr<transformer>>(value));
}

auto
scope::find_candidates(ptr<symbol> name, scope_set const& envs) const -> std::vector<binding> {
  std::vector<binding> result;
  for (binding const& e : bindings_) {
    ptr<syntax> s = std::get<ptr<syntax>>(e);
    if (s->get_symbol() == name && scope_sets_subseteq(s->scopes(), envs))
      result.push_back(e);
  }

  return result;
}

void
scope::visit_members(member_visitor const& f) {
  for (auto& [identifier, binding] : bindings_) {
    f(identifier);
    if (ptr<transformer>* tr = std::get_if<ptr<transformer>>(&binding))
      f(*tr);
  }
}

bool
scope::is_redefinition(ptr<syntax> id, value_type const& intended_value) const {
  for (binding const& b : bindings_)
    if (std::get<ptr<syntax>>(b)->get_symbol()->value() == id->get_symbol()->value()
        && scope_sets_equal(id->scopes(), std::get<ptr<syntax>>(b)->scopes())
        && std::get<value_type>(b) != intended_value)
      return true;
  return false;
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
  id->scopes().back()->add(fs, id, value);
}

void
define(free_store& fs, ptr<syntax> id, ptr<transformer> value) {
  id->scopes().back()->add(fs, id, value);
}

std::optional<scope::value_type>
lookup(ptr<symbol> name, scope_set const& envs) {
  std::optional<scope::value_type> result;
  scope_set maximal_scope_set;
  std::optional<scope_set> ambiguous_other_candidate_set;

  for (ptr<scope> e : envs)
    for (scope::binding const& b : e->find_candidates(name, envs)) {
      scope_set const& binding_set = std::get<ptr<syntax>>(b)->scopes();

      if (scope_sets_subseteq(maximal_scope_set, binding_set)) {
        ambiguous_other_candidate_set = std::nullopt;
        maximal_scope_set = binding_set;
        result = std::get<scope::value_type>(b);
      } else if (!scope_sets_subseteq(binding_set, maximal_scope_set))
        ambiguous_other_candidate_set = binding_set;
    }

  if (ambiguous_other_candidate_set)
    throw make_error("Ambiguous reference to {}\n"
                     "  Reference scopes:     {};\n"
                     "  1st candidate scopes: {};\n"
                     "  2nd candidate scopes: {}.",
                     name->value(),
                     format_scope_set(envs),
                     format_scope_set(maximal_scope_set),
                     format_scope_set(*ambiguous_other_candidate_set));

  return result;
}

} // namespace insider
