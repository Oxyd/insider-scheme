#ifndef INSIDER_PARAMETER_MAP_HPP
#define INSIDER_PARAMETER_MAP_HPP

#include "object.hpp"
#include "ptr.hpp"

namespace insider {

class free_store;
class parameter_tag;

// Flat map of parameter_tag's to values.
class parameter_map : public composite_object<parameter_map> {
public:
  static constexpr char const* scheme_name = "insider::parameter_map";

  ptr<>
  find_value(ptr<parameter_tag>);

  void
  set_value(free_store&, ptr<parameter_tag>, ptr<>);

  void
  add_value(ptr<parameter_tag>, ptr<>);

  void
  visit_members(member_visitor const&);

private:
  std::vector<std::tuple<ptr<parameter_tag>, ptr<>>> values_;
};

} // namespace insider

#endif
