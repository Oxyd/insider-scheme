#ifndef INSIDER_PARAMETER_MAP_HPP
#define INSIDER_PARAMETER_MAP_HPP

#include "object.hpp"

namespace insider {

class parameter_tag;

// Flat map of parameter_tag's to values.
class parameter_map {
public:
  ptr<>
  find_value(ptr<parameter_tag>);

  void
  set_value(ptr<parameter_tag>, ptr<>);

  void
  add_value(ptr<parameter_tag>, ptr<>);

  void
  visit_members(member_visitor const&) const;

private:
  std::vector<std::tuple<ptr<parameter_tag>, ptr<>>> values_;
};

} // namespace insider

#endif
