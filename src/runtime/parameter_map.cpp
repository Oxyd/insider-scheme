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
      value = new_value;
      fs.notify_arc(this, new_value);
      return;
    }

  assert(!"Parameter tag not found");
}

void
parameter_map::add_value(free_store& fs, ptr<parameter_tag> tag, ptr<> value) {
  values_.emplace_back(tag, value);
  fs.notify_arc(this, tag);
  fs.notify_arc(this, value);
}

void
parameter_map::visit_members(member_visitor const& f) {
  for (auto& [key, value] : values_) {
    f(key);
    f(value);
  }
}

} // namespace insider
