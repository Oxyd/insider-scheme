#include "string.hpp"

#include "basic_types.hpp"
#include "code_point_properties.hpp"
#include "context.hpp"
#include "define_procedure.hpp"

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
string::set(std::size_t i, char32_t c) {
  std::size_t byte_index = nth_code_point(data_, i);
  std::size_t old_length = utf8_code_point_byte_length(data_[byte_index]);
  std::size_t new_length = utf32_code_point_byte_length(c);

  if (new_length < old_length)
    data_.erase(byte_index, old_length - new_length);
  else if (new_length > old_length)
    data_.insert(byte_index, new_length - old_length, {});

  to_utf8(c, [&] (char byte) mutable { data_[byte_index++] = byte; });
}

char32_t
string::ref(std::size_t i) const {
  std::size_t byte_index = nth_code_point(data_, i);
  return from_utf8(data_.begin() + byte_index, data_.end()).code_point;
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
append(std::string& data, char32_t c) {
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
        append(new_data, *upcase_cp);
    else
      append(new_data, cp);
  });
  return make<string>(ctx, std::move(new_data));
}

static void
update_is_preceded_by_cased_letter(bool& is_preceded_by_cased_letter, char32_t cp) {
  if (auto prop = find_properties(cp)) {
    if (has_attribute(*prop, code_point_attribute::cased_letter))
      is_preceded_by_cased_letter = true;
    else if (!has_attribute(*prop, code_point_attribute::case_ignorable))
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
      if (has_attribute(*prop, code_point_attribute::cased_letter))
        return true;
      else if (!has_attribute(*prop, code_point_attribute::case_ignorable))
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
      append(new_data, downcase(*cp));
    } else {
      if (is_preceded_by_cased_letter
          && !is_followed_by_cased_letter(cp.base(), old_data.data() + old_data.length()))
        append(new_data, lowercase_final_sigma);
      else
        append(new_data, lowercase_medial_sigma);
    }

  return make<string>(ctx, std::move(new_data));
}

ptr<string>
foldcase(context& ctx, ptr<string> s) {
  std::string const& old_data = s->value();
  std::string new_data;
  new_data.reserve(old_data.length());

  for_each_code_point(old_data, [&] (char32_t cp) {
    if (auto prop = find_properties(cp))
      for (char32_t const* c = prop->complex_case_folding; *c; ++c)
        append(new_data, *c);
    else
      append(new_data, cp);
  });

  return make<string>(ctx, std::move(new_data));
}

std::u32string
foldcase(std::u32string const& s) {
  std::u32string result;
  result.reserve(s.length());

  for (char32_t cp : s) {
    if (auto prop = find_properties(cp))
      for (char32_t const* c = prop->complex_case_folding; *c; ++c)
        result.push_back(*c);
    else
      result.push_back(cp);
  }

  return result;
}

static void
check_utf8(ptr<bytevector> bv, std::size_t start, std::size_t end) {
  // from_utf8 will throw if it can't decode bytes as UTF-8.

  while (start < end)
    start += from_utf8(bv->begin() + start, bv->begin() + end).length;
}

ptr<string>
utf8_to_string(context& ctx, ptr<bytevector> bv, std::size_t start, std::size_t end) {
  assert(start <= end);
  assert(start < bv->size());
  assert(end <= bv->size());

  check_utf8(bv, start, end);
  return make<string>(ctx, std::string(bv->begin() + start, bv->begin() + end));
}

std::tuple<std::size_t, std::size_t>
find_code_point_byte_range(std::string const& data, std::size_t code_point_start, std::size_t code_point_end) {
  std::size_t byte_index = 0;
  std::size_t code_point_index = 0;

  std::size_t start_index = 0;
  std::size_t end_index = data.size();

  while (byte_index < data.size()) {
    if (code_point_index == code_point_start)
      start_index = byte_index;

    byte_index += from_utf8(data.data() + byte_index, data.data() + data.size()).length;
    ++code_point_index;

    if (code_point_index == code_point_end) {
      end_index = byte_index;
      break;
    }
  }

  return {start_index, end_index};
}

ptr<bytevector>
string_to_utf8(context& ctx, ptr<string> s, std::size_t start, std::size_t end) {
  std::string const& data = s->value();
  auto [start_index, end_index] = find_code_point_byte_range(data, start, end);

  auto result = make<bytevector>(ctx, end_index - start_index);
  for (std::size_t i = start_index, j = 0; i < end_index; ++i, ++j)
    result->set(j, data[i]);

  return result;
}

static ptr<>
make_string(context& ctx, object_span args) {
  if (args.size() < 1)
    throw std::runtime_error{"make-string: Expected at least 1 argument"};
  if (args.size() > 2)
    throw std::runtime_error{"make-string: Expected at most 2 arguments"};

  integer::value_type length = expect<integer>(args[0]).value();
  if (length < 0)
    throw std::runtime_error{"make-string: Length cannot be negative"};

  auto result = make<string>(ctx, length);

  if (args.size() == 2) {
    char32_t fill = expect<char32_t>(args[1]);
    for (std::size_t i = 0; i < static_cast<std::size_t>(length); ++i)
      result->set(i, fill);
  }

  return result;
}

static integer
string_length(ptr<string> s) {
  return static_cast<integer::value_type>(s->length());
}

static ptr<string>
string_append(context& ctx, object_span args) {
  std::string result;
  for (ptr<> s : args)
    result += expect<string>(s)->value();
  return make<string>(ctx, result);
}

static ptr<string>
symbol_to_string(context& ctx, ptr<symbol> datum) {
  return make<string>(ctx, expect<symbol>(datum)->value());
}

static ptr<symbol>
string_to_symbol(context& ctx, ptr<string> s) {
  return ctx.intern(s->value());
}

void
export_string(context& ctx, module_& result) {
  define_raw_procedure(ctx, "make-string", result, true, make_string);
  define_procedure(ctx, "string-length", result, true, string_length);
  define_raw_procedure(ctx, "string-append", result, true, string_append);
  define_procedure(ctx, "symbol->string", result, true, symbol_to_string);
  define_procedure(ctx, "string->symbol", result, true, string_to_symbol);
}

} // namespace insider
