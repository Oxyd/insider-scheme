#include "runtime/parameter_map.hpp"

#include "memory/free_store.hpp"

namespace insider {

ptr<>
parameter_map::find_value(ptr<parameter_tag> tag) {
  auto it = values_.find(tag);
  assert(it != values_.end());
  return it->second;
}

void
parameter_map::set_value(ptr<parameter_tag> tag, ptr<> new_value) {
  auto it = values_.find(tag);
  assert(it != values_.end());
  it->second = new_value;
}

void
parameter_map::add_value(ptr<parameter_tag> tag, ptr<> value) {
  values_.emplace(tag, value);
}

void
parameter_map::visit_members(member_visitor const& f) const {
  for (auto const& [key, value] : values_) {
    f(key);
    f(value);
  }
}

} // namespace insider
