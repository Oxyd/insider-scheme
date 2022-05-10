#include "root_provider.hpp"

#include "free_store.hpp"
#include "root_list.hpp"

namespace insider {

root_provider::root_provider(free_store& fs)
  : root_provider{fs.root_list()}
{ }

root_provider::root_provider(root_list& list)
  : list_{&list}
{
  add_to_list();
}

root_provider::root_provider(root_provider const& other)
  : list_{other.list_}
{
  add_to_list();
}

root_provider::root_provider(root_provider&& other) noexcept
  : list_{other.list_}
  , index_{other.index_}
{
  other.list_ = nullptr;
  if (list_)
    list_->update_provider(index_, this);
}


root_provider&
root_provider::operator = (root_provider&& other) noexcept {
  if (this != &other) {
    if (list_)
      remove_from_list();

    list_ = other.list_;
    index_ = other.index_;
    other.list_ = nullptr;

    if (list_)
      list_->update_provider(index_, this);
  }
  return *this;
}

root_provider::~root_provider() {
  if (list_)
    remove_from_list();
}

void
root_provider::add_to_list() {
  index_ = list_->add_provider(this);
}

void
root_provider::remove_from_list() {
  list_->remove_provider(index_);
}

} // namespace insider
