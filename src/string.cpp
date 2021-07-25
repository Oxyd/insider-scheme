#include "string.hpp"

#include "context.hpp"

namespace insider {

string::string(string&& other)
  : size_{other.size_}
{
  for (std::size_t i = 0; i < size_; ++i)
    storage_element(i) = other.storage_element(i);
}

void
string::set(std::size_t i, char c) {
  assert(i < size_);
  storage_element(i) = c;
}

std::string
string::value() const {
  std::string result;
  result.reserve(size_);

  for (std::size_t i = 0; i < size_; ++i)
    result += storage_element(i);

  return result;
}

std::size_t
string::hash() const {
  // djb2
  std::size_t result = 5381;

  for (std::size_t i = 0; i < size_; ++i)
    result = ((result << 5) + result) + storage_element(i);

  return result;
}

ptr<string>
make_string(context& ctx, std::string_view value) {
  auto result = make<string>(ctx, value.size());

  for (std::size_t i = 0; i < value.size(); ++i)
    result->set(i, value[i]);

  return result;
}

} // namespace insider
