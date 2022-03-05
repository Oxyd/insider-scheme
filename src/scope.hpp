#ifndef INSIDER_SCOPE_HPP
#define INSIDER_SCOPE_HPP

#include "object.hpp"
#include "ptr.hpp"

#include <string>
#include <variant>

namespace insider {

class scope;
class symbol;
class syntax;
class transformer;
struct variable;

using scope_set = std::vector<ptr<scope>>;

enum class scope_set_operation {
  add, remove, flip
};

bool // Was the scope actually added?
add_scope(scope_set&, ptr<scope>);

void
remove_scope(scope_set&, ptr<scope>);

bool // Was the scope added?
flip_scope(scope_set&, ptr<scope>);

void
update_scope_set(scope_set&, scope_set_operation, ptr<scope>);

bool
scope_sets_subseteq(scope_set const& lhs, scope_set const& rhs);

bool
scope_sets_equal(scope_set const& lhs, scope_set const& rhs);

void
visit_members(scope_set&, member_visitor const&);

class scope : public composite_object<scope> {
public:
  static constexpr char const* scheme_name = "insider::scope";

  using value_type = std::variant<std::shared_ptr<variable>, ptr<transformer>>;
  using binding = std::tuple<ptr<syntax>, value_type>;

  explicit
  scope(std::string desc)
    : description_{std::move(desc)}
  { }

  void
  add(free_store& store, ptr<syntax> identifier, std::shared_ptr<variable>);

  void
  add(free_store& store, ptr<syntax> identifier, ptr<transformer>);

  void
  add(free_store& store, ptr<syntax> identifier, value_type const&);

  std::vector<binding>
  find_candidates(ptr<symbol> name, scope_set const& scopess) const;

  std::vector<std::string>
  bound_names() const;

  std::string const&
  description() const { return description_; }

  auto
  begin() const { return bindings_.begin(); }

  auto
  end() const { return bindings_.end(); }

  void
  visit_members(member_visitor const& f);

private:
  std::vector<binding> bindings_;
  std::string          description_;

  bool
  is_redefinition(ptr<syntax>, value_type const& intended_value) const;
};

void
define(free_store&, ptr<syntax> id, std::shared_ptr<variable>);

void
define(free_store&, ptr<syntax> id, ptr<transformer>);

std::optional<scope::value_type>
lookup(ptr<symbol> id, scope_set const& envs);

} // namespace insider

#endif
