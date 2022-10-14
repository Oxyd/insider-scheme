#include "object.hpp"

#include <stdexcept>

namespace insider {

static word_type
new_type_with_dynamic_index(type_descriptor d) {
  type_vector& ts = types();

  if (ts.size + 1 >= max_types)
    throw std::range_error{"Too many types. Increase max_types."};

  ts.types[ts.size++] = d;
  return ts.size - 1;
}

static word_type
new_type_with_static_index(type_descriptor d, word_type index) {
  assert(index > 0);  // Reserve the 0th entry for an invalid type.
  assert(index < first_dynamic_type_index);

  type_vector& ts = types();
  assert(!ts.types[index].visit_members);

  ts.types[index] = d;
  return index;
}

word_type
new_type(type_descriptor d, std::optional<word_type> index) {
  if (index)
    return new_type_with_static_index(d, *index);
  else
    return new_type_with_dynamic_index(d);
}

} // namespace insider
