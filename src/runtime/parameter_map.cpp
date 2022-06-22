#include "runtime/parameter_map.hpp"

#include "memory/free_store.hpp"

namespace insider {

ptr<>
parameter_map::find_value(ptr<parameter_tag> tag) {
  for (auto& [key, value] : values_)
    if (key == tag)
      return value;

  assert(!"Can't happen");
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

  assert(!"Can't happen");
}

void
parameter_map::add_value(ptr<parameter_tag> tag, ptr<> value) {
  values_.emplace_back(tag, value);
}

void
parameter_map::visit_members(member_visitor const& f) {
  for (auto& [key, value] : values_) {
    f(key);
    f(value);
  }
}

} // namespace insider
