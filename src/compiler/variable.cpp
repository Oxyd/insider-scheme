#include "compiler/variable.hpp"

#include "memory/free_store.hpp"

namespace insider {

void
variable_base::set_constant_initialiser(free_store& fs, ptr<> self,
                                        expression e) {
  constant_initialiser_ = e;
  fs.notify_arc(self, e.get());
}

} // namespace insider
