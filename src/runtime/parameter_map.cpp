#include "runtime/parameter_map.hpp"

#include "memory/free_store.hpp"

namespace insider {

ptr<>
parameter_map::find_value(ptr<parameter_tag> tag) {
  for (auto const& [key, value] : values_)
    if (key == tag)
      return value;

  assert(!"Parameter tag not found");
  return {};
}

void
parameter_map::set_value(free_store& fs, ptr<parameter_tag> tag,
                         ptr<> new_value) {
  for (auto& [key, value] : values_)
    if (key == tag) {
      value.assign(fs, this, new_value);
      return;
    }

  assert(!"Parameter tag not found");
}

void
parameter_map::add_value(free_store& fs, ptr<parameter_tag> tag, ptr<> value) {
  values_.emplace_back(member_ptr<parameter_tag>{fs, this, tag}, 
                       member_ptr<>{fs, this, value});
}

void
parameter_map::visit_members(member_visitor const& f) {
  for (auto& [key, value] : values_) {
    key.visit_members(f);
    value.visit_members(f);
  }
}

} // namespace insider
