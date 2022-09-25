#include "object.hpp"

#include <stdexcept>

namespace insider {

word_type
new_type(type_descriptor d) {
  type_vector& ts = types();

  if (ts.size + 1 >= max_types)
    throw std::range_error{"Too many types. Increase max_types."};

  ts.types[ts.size++] = d;
  return ts.size - 1;
}

} // namespace insider
