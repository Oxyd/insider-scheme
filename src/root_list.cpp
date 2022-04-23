#include "root_list.hpp"

#include <optional>

namespace insider {

std::size_t
root_list::add_provider(root_provider* provider) {
  providers_.push_back(provider);
  return providers_.size() - 1;
}

void
root_list::remove_provider(std::size_t index) {
  assert(index < providers_.size());
  assert(providers_[index]);
  providers_[index] = nullptr;
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
find_free_index(std::vector<root_provider*> const& v, std::size_t start) {
  for (std::size_t i = start; i < v.size(); ++i)
    if (!v[i])
      return i;
  return std::nullopt;
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
  auto free_index = find_free_index(providers_, 0);
  while (free_index) {
    if (auto occupied_index = find_occupied_index(providers_, *free_index + 1)) {
      root_provider* provider = providers_[*occupied_index];
      providers_[*free_index] = provider;
      providers_[*occupied_index] = nullptr;
      provider->update_index(*free_index);

      free_index = find_free_index(providers_, *free_index + 1);
    } else
      break;
  }

  if (free_index)
    providers_.erase(providers_.begin() + *free_index, providers_.end());
}

} // namespace insider
