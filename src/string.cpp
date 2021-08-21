#include "string.hpp"

#include "code_point_properties.hpp"
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

static void
append(std::string& data, character c) {
  to_utf8(c, [&] (char byte) { data.push_back(byte); });
}

template <typename F>
void
for_each_code_point(std::string const& data, F&& f) {
  std::size_t i = 0;
  while (i < data.length()) {
    auto result = from_utf8(data.begin() + i, data.end());
    f(result.code_point);
    i += result.length;
  }
}

ptr<string>
upcase(context& ctx, ptr<string> s) {
  std::string const& old_data = s->value();
  std::string new_data;
  new_data.reserve(old_data.length());
  for_each_code_point(old_data, [&] (char32_t cp) {
    if (auto prop = find_properties(cp))
      for (char32_t const* upcase_cp = prop->complex_uppercase; *upcase_cp; ++upcase_cp)
        append(new_data, character{*upcase_cp});
    else
      append(new_data, character{cp});
  });
  return make<string>(ctx, std::move(new_data));
}

ptr<string>
downcase(context& ctx, ptr<string> s) {
  // Note: In Unicode version 13, there are no language-independent downcasing
  // mappings that differ from the simple mappings.

  std::string const& old_data = s->value();
  std::string new_data;
  new_data.reserve(old_data.length());
  for_each_code_point(old_data, [&] (char32_t cp) {
    append(new_data, downcase(character{cp}));
  });
  return make<string>(ctx, std::move(new_data));
}

} // namespace insider
