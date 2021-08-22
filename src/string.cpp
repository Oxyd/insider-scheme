#include "string.hpp"

#include "code_point_properties.hpp"
#include "context.hpp"

#include <iterator>

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

namespace {
  class code_point_iterator {
  public:
    using value_type = char32_t;
    using difference_type = std::ptrdiff_t;
    using reference = char32_t const&;
    using pointer = char32_t const*;
    using iterator_category = std::forward_iterator_tag;

    code_point_iterator(char const* current, char const* end);

    reference
    operator * () const { return value_; }

    code_point_iterator&
    operator ++ ();

    code_point_iterator
    operator ++ (int);

    bool
    operator == (code_point_iterator other) const;

    bool
    operator != (code_point_iterator other) const { return !operator == (other); }

    char const*
    base() const { return current_; }

  private:
    char const* current_;
    char const* end_;
    value_type  value_;

    void
    read_value();
  };
}

code_point_iterator::code_point_iterator(char const* current, char const* end)
  : current_{current}
  , end_{end}
{
  read_value();
}

code_point_iterator&
code_point_iterator::operator ++ () {
  read_value();
  return *this;
}

code_point_iterator
code_point_iterator::operator ++ (int) {
  code_point_iterator result{*this};
  ++*this;
  return result;
}

bool
code_point_iterator::operator == (code_point_iterator other) const {
  return current_ == other.current_
         && end_ == other.end_
         && value_ == other.value_;
}

void
code_point_iterator::read_value() {
  if (current_ != end_) {
    from_utf8_result r = from_utf8(current_, end_);
    value_ = r.code_point;
    current_ += r.length;
  } else
    value_ = 0;
}

static code_point_iterator
code_points_begin(std::string const& data) {
  return code_point_iterator{data.data(), data.data() + data.length()};
}

static code_point_iterator
code_points_end(std::string const& data) {
  return code_point_iterator{data.data() + data.length(), data.data() + data.length()};
}

template <typename F>
void
for_each_code_point(std::string const& data, F&& f) {
  for (auto it = code_points_begin(data), e = code_points_end(data); it != e; ++it)
    f(*it);
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

static void
update_is_preceded_by_cased_letter(bool& is_preceded_by_cased_letter, char32_t cp) {
  if (auto prop = find_properties(cp)) {
    if (has_category(*prop, code_point_category::cased_letter))
      is_preceded_by_cased_letter = true;
    else if (!has_category(*prop, code_point_category::case_ignorable))
      is_preceded_by_cased_letter = false;

    // Else it's a case-ignorable letter, so the value of
    // is_preceded_by_cased_letter doesn't change.
  }
  else
    is_preceded_by_cased_letter = false;
}

static bool
is_followed_by_cased_letter(char const* begin, char const* end) {
  for (auto it = code_point_iterator{begin, end}, e = code_point_iterator{end, end}; it != e; ++it)
    if (auto prop = find_properties(*it)) {
      if (has_category(*prop, code_point_category::cased_letter))
        return true;
      else if (!has_category(*prop, code_point_category::case_ignorable))
        return false;
    }

  return false;
}

ptr<string>
downcase(context& ctx, ptr<string> s) {
  // Note: In Unicode version 13, there are no language-independent downcasing
  // mappings that differ from the simple mappings.

  std::string const& old_data = s->value();
  std::string new_data;
  new_data.reserve(old_data.length());

  // Greek capital sigma can be lowercased to either the medial or final
  // sigma. The normal conversion is to the medial variant. The conditions for
  // lowercasing capital sigma to final lowercase sigma are:
  //   1) It is preceded by a cased character optionally followed by a sequence
  //      of case-ignorable characters, and
  //   2) it is *not* followed by a sequence of zero or more case-ignorable
  //      characters, followed by a cased letter.

  bool is_preceded_by_cased_letter = false;

  for (auto cp = code_points_begin(old_data), e = code_points_end(old_data); cp != e; ++cp)
    if (*cp != uppercase_sigma) {
      update_is_preceded_by_cased_letter(is_preceded_by_cased_letter, *cp);
      append(new_data, downcase(character{*cp}));
    } else {
      if (is_preceded_by_cased_letter
          && !is_followed_by_cased_letter(cp.base(), old_data.data() + old_data.length()))
        append(new_data, character{lowercase_final_sigma});
      else
        append(new_data, character{lowercase_medial_sigma});
    }

  return make<string>(ctx, std::move(new_data));
}

} // namespace insider
