#ifndef INSIDER_MEMORY_ROOT_LIST_HPP
#define INSIDER_MEMORY_ROOT_LIST_HPP

#include "root_provider.hpp"

#include <vector>

namespace insider {

// List of GC roots, i.e. of root_provider's.
class root_list {
public:
  std::size_t
  add_provider(root_provider*);

  void
  remove_provider(std::size_t);

  void
  update_provider(std::size_t, root_provider*);

  void
  visit_roots(member_visitor const&) const;

  void
  compact();

private:
  std::vector<root_provider*> providers_;
};

} // namespace insider

#endif
