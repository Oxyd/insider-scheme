#include "ptr.hpp"

#include "free_store.hpp"

namespace insider {

tracked_ptr<>*
tracked_ptr<>::root_list(free_store& fs) noexcept {
  return fs.root_list();
}

weak_ptr<>*
weak_ptr<>::root_list(free_store& fs) noexcept {
  return fs.weak_root_list();
}

} // namespace insider
