#ifndef INSIDER_SCOPE_HPP
#define INSIDER_SCOPE_HPP

#include "object.hpp"
#include "ptr.hpp"

#include <cstdint>
#include <string>
#include <variant>

namespace insider {

class free_store;
class scope;
class symbol;
class syntax;
class transformer;
struct variable;

class scope_set {
public:
  scope_set() = default;

  explicit
  scope_set(ptr<scope> s) : scopes_{s} { }

  void
  add(ptr<scope>);

  void
  remove(ptr<scope>);

  void
  flip(ptr<scope>);

  auto
  begin() const { return scopes_.cbegin(); }

  auto
  end() const { return scopes_.cend(); }

  ptr<scope>
  back() const { return scopes_.back(); }

  std::vector<ptr<scope>> const&
  data() const { return scopes_; }

  bool
  empty() const { return scopes_.empty(); }

  std::size_t
  size() const { return scopes_.size(); }

  void
  visit_members(member_visitor const&);

private:
  std::vector<ptr<scope>> scopes_;
};

enum class scope_set_operation {
  add, remove, flip
};

void
update_scope_set(scope_set&, scope_set_operation, ptr<scope>);

bool
scope_sets_subseteq(scope_set const& lhs, scope_set const& rhs);

bool
scope_sets_equal(scope_set const& lhs, scope_set const& rhs);

class scope : public composite_object<scope> {
public:
  static constexpr char const* scheme_name = "insider::scope";

  using value_type = std::variant<std::shared_ptr<variable>, ptr<transformer>>;
  using binding = std::tuple<ptr<syntax>, value_type>;
  using id_type = std::uint64_t;

  scope(context& ctx, std::string desc);

  void
  add(free_store& store, ptr<syntax> identifier, std::shared_ptr<variable>);

  void
  add(free_store& store, ptr<syntax> identifier, ptr<transformer>);

  void
  add(free_store& store, ptr<syntax> identifier, value_type const&);

  std::vector<binding>
  find_candidates(ptr<symbol> name, scope_set const& scopess) const;

  std::string const&
  description() const { return description_; }

  auto
  begin() const { return bindings_.begin(); }

  auto
  end() const { return bindings_.end(); }

  id_type
  id() const { return id_; }

  void
  visit_members(member_visitor const& f);

private:
  std::vector<binding> bindings_;
  std::string          description_;
  id_type              id_;

  bool
  is_redefinition(ptr<syntax>, value_type const& intended_value) const;
};

void
define(free_store&, ptr<syntax> id, std::shared_ptr<variable>);

void
define(free_store&, ptr<syntax> id, ptr<transformer>);

std::optional<scope::value_type>
lookup(ptr<symbol> id, scope_set const& envs);

struct scope_comparator {
  bool
  operator () (ptr<scope> lhs, ptr<scope> rhs) const {
    return lhs->id() < rhs->id();
  }
};

} // namespace insider

#endif
