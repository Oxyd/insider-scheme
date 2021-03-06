#include "root_list.hpp"

#include <algorithm>
#include <optional>

namespace insider {

std::size_t
root_list::add_provider(root_provider* provider) {
  std::size_t index = find_free_index();
  providers_[index] = provider;
  return index;
}

void
root_list::remove_provider(std::size_t index) {
  assert(index < providers_.size());
  assert(providers_[index]);
  providers_[index] = nullptr;
  free_indices_.push_back(index);
}

void
root_list::update_provider(std::size_t index, root_provider* new_provider) {
  assert(index < providers_.size());
  providers_[index] = new_provider;
}

void
root_list::visit_roots(member_visitor const& f) const {
  for (root_provider* provider : providers_)
    if (provider)
      provider->visit_roots(f);
}

static std::optional<std::size_t>
find_occupied_index(std::vector<root_provider*> const& v, std::size_t start) {
  for (std::size_t i = start; i < v.size(); ++i)
    if (v[i])
      return i;
  return std::nullopt;
}

void
root_list::compact() {
  std::ranges::sort(free_indices_);

  std::size_t last_free_index = providers_.size();
  for (std::size_t free_index : free_indices_) {
    if (auto occupied_index = find_occupied_index(providers_, free_index + 1)) {
      root_provider* provider = providers_[*occupied_index];
      providers_[free_index] = provider;
      providers_[*occupied_index] = nullptr;
      provider->update_index(free_index);
    } else {
      last_free_index = free_index;
      break;
    }
  }

  if (last_free_index < providers_.size())
    providers_.erase(providers_.begin() + last_free_index, providers_.end());

  free_indices_.clear();
}

std::size_t
root_list::find_free_index() {
  if (!free_indices_.empty()) {
    std::size_t idx = free_indices_.back();
    free_indices_.pop_back();
    return idx;
  } else {
    std::size_t idx = providers_.size();
    providers_.push_back(nullptr);
    return idx;
  }
}

} // namespace insider
