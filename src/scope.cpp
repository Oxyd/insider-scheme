#include "scope.hpp"

#include "basic_types.hpp"
#include "syntax.hpp"

namespace insider {

bool
add_scope(scope_set& set, ptr<scope> env) {
  if (std::find(set.begin(), set.end(), env) == set.end()) {
    set.push_back(env);
    return true;
  } else
    return false;
}

void
remove_scope(scope_set& set, ptr<scope> env) {
  set.erase(std::remove(set.begin(), set.end(), env), set.end());
}

bool
flip_scope(scope_set& set, ptr<scope> env) {
  auto it = std::find(set.begin(), set.end(), env);
  if (it == set.end()) {
    set.push_back(env);
    return true;
  } else {
    set.erase(it);
    return false;
  }
}

bool
scope_sets_subseteq(scope_set const& lhs, scope_set const& rhs) {
  for (ptr<scope> e : lhs)
    if (std::find(rhs.begin(), rhs.end(), e) == rhs.end())
      return false;
  return true;
}

bool
scope_sets_equal(scope_set const& lhs, scope_set const& rhs) {
  return scope_sets_subseteq(lhs, rhs) && scope_sets_subseteq(rhs, lhs);
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
    if (assume<symbol>(s->expression()) == name && scope_sets_subseteq(s->scopes(), envs))
      result.push_back(e);
  }

  return result;
}

std::vector<std::string>
scope::bound_names() const {
  std::vector<std::string> result;
  result.reserve(bindings_.size());

  for (auto const& [identifier, binding] : bindings_)
    result.push_back(identifier_name(identifier));

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
    if (assume<symbol>(std::get<ptr<syntax>>(b)->expression())->value() == assume<symbol>(id->expression())->value()
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
    throw make_error("Ambiguous reference to {} ({}). 1st candidate scopes: {}; 2nd candidate scopes: {}",
                     name->value(),
                     format_scope_set(envs),
                     format_scope_set(maximal_scope_set),
                     format_scope_set(*ambiguous_other_candidate_set));

  return result;
}

} // namespace insider
