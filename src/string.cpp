#include "string.hpp"

#include "context.hpp"

namespace insider {

void
string::set(std::size_t i, char c) {
  data_[i] = c;
}

character
string::ref(std::size_t i) const {
  return character{static_cast<character::value_type>(data_[i])};
}

std::size_t
string::hash() const {
  // djb2
  std::size_t result = 5381;

  for (char c : data_)
    result = ((result << 5) + result) + c;

  return result;
}

} // namespace insider
