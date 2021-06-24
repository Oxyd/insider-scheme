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
class variable;

using scope_set = std::vector<ptr<scope>>;

bool // Was the scope actually added?
add_scope(scope_set&, ptr<scope>);

void
remove_scope(scope_set&, ptr<scope>);

bool // Was the scope added?
flip_scope(scope_set&, ptr<scope>);

bool
scope_sets_subseteq(scope_set const& lhs, scope_set const& rhs);

bool
scope_sets_equal(scope_set const& lhs, scope_set const& rhs);

class scope : public composite_object<scope> {
public:
  static constexpr char const* scheme_name = "insider::scope";

  using value_type = std::variant<std::shared_ptr<variable>, ptr<transformer>>;
  using binding = std::tuple<ptr<syntax>, value_type>;

  explicit
  scope(std::string desc, bool use_site = false)
    : description_{std::move(desc)}
    , use_site_{use_site}
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

  bool
  is_use_site() const { return use_site_; }

  auto
  begin() const { return bindings_.begin(); }

  auto
  end() const { return bindings_.end(); }

  void
  visit_members(member_visitor const& f);

  std::size_t
  hash() const;

private:
  std::vector<binding> bindings_;
  std::string          description_;
  bool                 use_site_;

  bool
  is_redefinition(ptr<syntax>, value_type const& intended_value) const;
};

std::optional<scope::value_type>
lookup(ptr<symbol> id, scope_set const& envs);

} // namespace insider

#endif
