#ifndef INSIDER_PARAMETER_MAP_HPP
#define INSIDER_PARAMETER_MAP_HPP

#include "object.hpp"

#include <unordered_map>

namespace insider {

class parameter_tag;

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
  std::unordered_map<ptr<parameter_tag>, ptr<>> values_;
};

} // namespace insider

#endif
