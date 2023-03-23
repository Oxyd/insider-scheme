#include "root_provider.hpp"

namespace insider {

void
root_list::visit_roots(member_visitor const& f) const {
  root_provider* p = head_.next_;
  while (p) {
    p->visit_roots(f);
    p = p->next_;
  }
}

} // namespace insider
