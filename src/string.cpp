#include "string.hpp"

#include "context.hpp"

namespace insider {

static std::size_t
nth_code_point(std::string const& data, std::size_t n) {
  std::size_t byte_index = 0;
  std::size_t code_point_index = 0;

  while (byte_index < data.length() && code_point_index < n) {
    byte_index += utf8_code_point_byte_length(data[byte_index]);
    ++code_point_index;
  }

  if (code_point_index != n)
    throw std::runtime_error{fmt::format("Index {} out of bounds", n)};

  return byte_index;
}

void
string::set(std::size_t i, character c) {
  std::size_t byte_index = nth_code_point(data_, i);
  std::size_t old_length = utf8_code_point_byte_length(data_[byte_index]);
  std::size_t new_length = utf32_code_point_byte_length(c.value());

  if (new_length < old_length)
    data_.erase(byte_index, old_length - new_length);
  else if (new_length > old_length)
    data_.insert(byte_index, new_length - old_length, {});

  to_utf8(c, [&] (char byte) mutable { data_[byte_index++] = byte; });
}

character
string::ref(std::size_t i) const {
  std::size_t byte_index = nth_code_point(data_, i);
  return character{from_utf8(data_.begin() + byte_index, data_.end()).code_point};
}

std::size_t
string::length() const {
  std::size_t result = 0;
  std::size_t byte_index = 0;
  while (byte_index < data_.size()) {
    byte_index += utf8_code_point_byte_length(data_[byte_index]);
    ++result;
  }

  return result;
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
