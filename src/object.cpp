#include "object.hpp"

namespace insider {

word_type
new_type(type_descriptor d) {
  types().push_back(d);
  return types().size() - 1;
}

} // namespace insider
