#include "string.hpp"

#include "context.hpp"
#include "runtime/basic_types.hpp"
#include "runtime/code_point_properties.hpp"
#include "runtime/symbol.hpp"
#include "util/define_procedure.hpp"

#include <algorithm>
#include <functional>
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
  set_byte_index(byte_index, c);
}

void
string::set_byte_index(std::size_t byte_index, char32_t c) {
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

void
string::append_char(char32_t c) {
  to_utf8(c, [&] (char byte) { data_.push_back(byte); });
}

void
string::append(std::string const& more_data) {
  data_.append(more_data);
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
    operator != (code_point_iterator other) const {
      return !operator == (other);
    }

    char const*
    base() const { return current_; }

  private:
    char const* current_;
    char const* end_;
    value_type  value_{};

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
  return code_point_iterator{data.data() + data.length(),
                             data.data() + data.length()};
}

void
for_each_code_point(std::string const& data, auto&& f) {
  for (auto it = code_points_begin(data), e = code_points_end(data);
       it != e;
       ++it)
    f(*it);
}

ptr<string>
string_upcase(context& ctx, ptr<string> s) {
  std::string const& old_data = s->value();
  std::string new_data;
  new_data.reserve(old_data.length());
  for_each_code_point(old_data, [&] (char32_t cp) {
    if (auto prop = find_properties(cp))
      for (char32_t const* upcase_cp = prop->complex_uppercase;
           *upcase_cp; ++upcase_cp)
        append(new_data, *upcase_cp);
    else
      append(new_data, cp);
  });
  return make<string>(ctx, std::move(new_data));
}

static void
update_is_preceded_by_cased_letter(bool& is_preceded_by_cased_letter,
                                   char32_t cp) {
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
  auto end_it = code_point_iterator{end, end};
  for (auto it = code_point_iterator{begin, end}; it != end_it; ++it)
    if (auto prop = find_properties(*it)) {
      if (has_attribute(*prop, code_point_attribute::cased_letter))
        return true;
      else if (!has_attribute(*prop, code_point_attribute::case_ignorable))
        return false;
    }

  return false;
}

ptr<string>
string_downcase(context& ctx, ptr<string> s) {
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
  auto end_it = code_points_end(old_data);
  for (auto cp = code_points_begin(old_data); cp != end_it; ++cp)
    if (*cp != uppercase_sigma) {
      update_is_preceded_by_cased_letter(is_preceded_by_cased_letter, *cp);
      append(new_data, char_downcase(*cp));
    } else {
      if (is_preceded_by_cased_letter
          && !is_followed_by_cased_letter(cp.base(),
                                          old_data.data() + old_data.length()))
        append(new_data, lowercase_final_sigma);
      else
        append(new_data, lowercase_medial_sigma);
    }

  return make<string>(ctx, std::move(new_data));
}

ptr<string>
string_foldcase(context& ctx, ptr<string> s) {
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
string_foldcase(std::u32string const& s) {
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
utf8_to_string(context& ctx, ptr<bytevector> bv,
               std::size_t start, std::size_t end) {
  assert(start <= end);
  assert(start < bv->size());
  assert(end <= bv->size());

  check_utf8(bv, start, end);
  return make<string>(ctx, std::string(bv->begin() + start, bv->begin() + end));
}

std::tuple<std::size_t, std::size_t>
find_code_point_byte_range(std::string const& data,
                           std::size_t code_point_start,
                           std::size_t code_point_end) {
  std::size_t byte_index = 0;
  std::size_t code_point_index = 0;

  std::size_t start_index = 0;
  std::size_t end_index = data.size();

  while (byte_index < data.size()) {
    if (code_point_index == code_point_start)
      start_index = byte_index;

    byte_index += from_utf8(data.data() + byte_index,
                            data.data() + data.size()).length;
    ++code_point_index;

    if (code_point_index == code_point_end) {
      end_index = byte_index;
      break;
    }
  }

  return {start_index, end_index};
}

ptr<bytevector>
string_to_utf8_byte_indexes(context& ctx, ptr<string> s,
                            std::size_t start, std::size_t end) {
  std::string const& data = s->value();

  auto result = make<bytevector>(ctx, end - start);
  for (std::size_t i = start, j = 0; i < end; ++i, ++j)
    result->set(j, data[i]);

  return result;
}

static ptr<>
construct_string(context& ctx, object_span args) {
  std::size_t length = args.size();
  std::string result;
  result.reserve(length);

  for (std::size_t i = 0; i < length; ++i)
    to_utf8(expect<char32_t>(args[i]), [&] (char byte) {
      result.push_back(byte);
    });

  result.shrink_to_fit();
  return make<string>(ctx, std::move(result));
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

static ptr<>
make_string_byte_length(context& ctx, std::size_t length) {
  return make<string>(ctx, std::string(length, '\0'));
}

static ptr<>
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

integer
next_code_point_byte_index(ptr<string> s, std::size_t index) {
  if (index >= s->value().size())
    throw std::runtime_error{"Can't advance post-end byte index"};
  else
    return index + utf8_code_point_byte_length(s->value()[index]);
}

integer
previous_code_point_byte_index(ptr<string> s, std::size_t index) {
  if (index == 0)
    throw std::runtime_error{"Can't go before start of string"};
  else {
    do
      --index;
    while (!is_initial_byte(s->value()[index]));
    return index;
  }
}

static char32_t
string_ref(ptr<string> s, std::size_t index) {
  return s->ref(index);
}

static char32_t
string_ref_byte_index(ptr<string> s, std::size_t bi) {
  if (bi >= s->value().size())
    throw std::runtime_error{"Byte index out of range"};
  else
    return from_utf8(s->value().begin() + bi, s->value().end()).code_point;
}

ptr<string>
string_reverse(context& ctx, ptr<string> s, std::size_t begin, std::size_t end) {
  std::string const& input = s->value();

  if (begin > input.size() || end > input.size() || begin > end)
    throw std::runtime_error{"Invalid index"};

  std::string output;
  output.resize(end - begin);

  std::size_t input_index = begin;
  std::size_t output_index = output.size();
  while (input_index < end) {
    from_utf8_result r = from_utf8(input.begin() + input_index, input.end());
    assert(output_index >= r.length);

    output_index -= r.length;
    to_utf8(r.code_point,
            [&, o = output_index] (char byte) mutable {
              output[o++] = byte;
            });

    input_index += r.length;
  }

  return make<string>(ctx, std::move(output));
}

static ptr<string>
string_copy_byte_indexes(context& ctx, ptr<string> s,
                         std::size_t begin, std::size_t end) {
  std::string const& value = s->value();
  if (begin > end)
    throw std::runtime_error{"Invalid index"};

  return make<string>(ctx, value.substr(begin, end - begin));
}

static void
string_append_in_place(ptr<string> s, ptr<string> t) {
  s->append(t->value());
}

static integer
string_contains_byte_indexes(ptr<string> haystack, ptr<string> needle,
                             std::size_t haystack_start,
                             std::size_t haystack_end,
                             std::size_t needle_start,
                             std::size_t needle_end) {
  std::string const& h = haystack->value();
  std::string const& n = needle->value();

  if (haystack_start > h.size()
      || haystack_end > h.size()
      || haystack_start > haystack_end
      || needle_start > n.size()
      || needle_end > n.size()
      || needle_start > needle_end)
    throw std::runtime_error{"Invalid index"};

  auto result = h.find(n.data() + needle_start, haystack_start,
                       needle_end - needle_start);
  if (result == std::string::npos || result > haystack_end)
    return static_cast<integer::value_type>(haystack_end);
  else
    return integer{static_cast<integer::value_type>(result)};
}

static integer
string_contains_right_byte_indexes(ptr<string> haystack, ptr<string> needle,
                                   std::size_t haystack_start,
                                   std::size_t haystack_end,
                                   std::size_t needle_start,
                                   std::size_t needle_end) {
  std::string const& h = haystack->value();
  std::string const& n = needle->value();

  if (haystack_start > h.size()
      || haystack_end > h.size()
      || haystack_start > haystack_end
      || needle_start > n.size()
      || needle_end > n.size()
      || needle_start > needle_end)
    throw std::runtime_error{"Invalid index"};

  std::size_t haystack_size = haystack_end - haystack_start;
  std::size_t needle_size = needle_end - needle_start;
  if (needle_size > haystack_size)
    return static_cast<integer::value_type>(haystack_end);

  auto result = h.rfind(n.data() + needle_start, haystack_end - needle_size,
                        needle_size);
  if (result == std::string::npos || result < haystack_start)
    return static_cast<integer::value_type>(haystack_end);
  else
    return integer{static_cast<integer::value_type>(result)};
}

static bool
string_eq(ptr<string> lhs, ptr<string> rhs) {
  return lhs->value() == rhs->value();
}

static bool
string_lt(ptr<string> lhs, ptr<string> rhs) {
  return lhs->value() < rhs->value();
}

static bool
string_le(ptr<string> lhs, ptr<string> rhs) {
  return lhs->value() <= rhs->value();
}

static bool
string_gt(ptr<string> lhs, ptr<string> rhs) {
  return lhs->value() > rhs->value();
}

static bool
string_ge(ptr<string> lhs, ptr<string> rhs) {
  return lhs->value() >= rhs->value();
}

void
export_string(context& ctx, ptr<module_> result) {
  define_raw_procedure<construct_string>(ctx, "string", result);
  define_raw_procedure<make_string>(ctx, "make-string", result);
  define_procedure<make_string_byte_length>(ctx, "make-string/byte-length",
                                            result);
  define_constant_evaluable_procedure<&string::length>(ctx, "string-length",
                                                       result);
  define_raw_procedure<string_append>(ctx, "string-append", result);
  define_procedure<string_append_in_place>(ctx, "string-append!", result);
  define_procedure<symbol_to_string>(ctx, "symbol->string", result);
  define_procedure<string_to_symbol>(ctx, "string->symbol", result);
  define_constant_evaluable_procedure<string_byte_length>(
    ctx, "string-byte-length", result
  );
  define_constant_evaluable_procedure<next_code_point_byte_index>(
    ctx, "next-code-point-byte-index", result
  );
  define_constant_evaluable_procedure<previous_code_point_byte_index>(
    ctx, "previous-code-point-byte-index", result
  );
  define_constant_evaluable_procedure<string_ref>(ctx, "string-ref", result);
  define_procedure<&string::set>(ctx, "string-set!", result);
  define_procedure<&string::set_byte_index>(ctx, "string-set!/byte-index",
                                            result);
  define_procedure<&string::append_char>(ctx, "string-append-char!", result);
  define_constant_evaluable_procedure<string_ref_byte_index>(
    ctx, "string-ref/byte-index", result
  );
  define_constant_evaluable_procedure<is_string_null>(ctx, "string-null?",
                                                      result);
  define_procedure<string_reverse>(ctx, "string-reverse*", result);
  define_procedure<string_copy_byte_indexes>(ctx, "string-copy/byte-indexes",
                                             result);
  define_procedure<string_contains_byte_indexes>(
    ctx, "string-contains/byte-indexes", result
  );
  define_procedure<string_contains_right_byte_indexes>(
    ctx, "string-contains-right/byte-indexes", result
  );
  define_constant_evaluable_procedure<string_eq>(ctx, "string=?/pair", result);
  define_constant_evaluable_procedure<string_lt>(ctx, "string<?/pair", result);
  define_constant_evaluable_procedure<string_le>(ctx, "string<=?/pair", result);
  define_constant_evaluable_procedure<string_gt>(ctx, "string>?/pair", result);
  define_constant_evaluable_procedure<string_ge>(ctx, "string>=?/pair", result);
  define_procedure<string_upcase>(ctx, "string-upcase", result);
  define_procedure<string_downcase>(ctx, "string-downcase", result);
  define_procedure<
    static_cast<ptr<string> (*)(context&, ptr<string>)>(&string_foldcase)
  >(ctx, "string-foldcase", result);
  define_procedure<string_to_utf8_byte_indexes>(ctx, "string->utf8/byte-indexes",
                                                result);
  define_procedure<utf8_to_string>(ctx, "utf8->string*", result);
}

} // namespace insider
