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

std::size_t
codepoint_length(std::string const& data) {
  std::size_t result = 0;
  std::size_t byte_index = 0;
  while (byte_index < data.size()) {
    byte_index += utf8_code_point_byte_length(data[byte_index]);
    ++result;
  }

  return result;
}

string::string(std::string value)
  : data_{std::move(value)}
  , codepoint_length_{codepoint_length(data_)}
{ }

void
string::set_cursor(string_cursor i, char32_t c) {
  if (i.value >= data_.size())
    throw std::runtime_error{"Invalid string cursor"};

  std::size_t old_length = utf8_code_point_byte_length(data_[i.value]);
  std::size_t new_length = utf32_code_point_byte_length(c);

  if (new_length < old_length)
    data_.erase(i.value, old_length - new_length);
  else if (new_length > old_length)
    data_.insert(i.value, new_length - old_length, {});

  to_utf8(c, [&] (char byte) mutable { data_[i.value++] = byte; });
}

void
string::append_char(char32_t c) {
  to_utf8(c, [&] (char byte) { data_.push_back(byte); });
  ++codepoint_length_;
}

void
string::append(std::string const& more_data) {
  data_.append(more_data);
  codepoint_length_ += codepoint_length(more_data);
}

std::size_t
string::hash() const {
  // djb2
  std::size_t result = 5381;

  for (char c : data_)
    result = ((result << 5) + result) + c;

  return result;
}

string_cursor
string_cursor_prev(ptr<string> s, string_cursor c) {
  if (c.value == 0)
    throw std::runtime_error{"Can't go before start of string"};
  else {
    do
      --c.value;
    while (!is_initial_byte(s->value()[c.value]));
    return c;
  }
}

void
string_set_nth(ptr<string> s, std::size_t i, char32_t c) {
  std::size_t byte_index = nth_code_point(s->value(), i);
  s->set_cursor({byte_index}, c);
}

void
string_set(ptr<string> s, ptr<> i, char32_t c) {
  if (auto cursor = match<string_cursor>(i))
    s->set_cursor(*cursor, c);
  else
    string_set_nth(s, expect<integer>(i).value(), c);
}

char32_t
string_ref_cursor(ptr<string> s, string_cursor c) {
  return from_utf8(s->value().begin() + c.value, s->value().end()).code_point;
}

char32_t
string_ref_nth(ptr<string> s, std::size_t i) {
  std::size_t byte_index = nth_code_point(s->value(), i);
  return from_utf8(s->value().begin() + byte_index, s->value().end()).code_point;
}

char32_t
string_ref(ptr<string> s, ptr<> i) {
  if (auto c = match<string_cursor>(i))
    return string_ref_cursor(s, *c);
  else
    return string_ref_nth(s, expect<integer>(i).value());
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
string_to_utf8(context& ctx, ptr<string> s,
               string_cursor start, string_cursor end) {
  std::string const& data = s->value();

  if (start.value > data.size() || end.value > data.size())
    throw std::runtime_error{"String cursor out of range"};

  if (start.value > end.value)
    throw std::runtime_error{"Invalid range"};

  auto result = make<bytevector>(ctx, end.value - start.value);
  for (std::size_t i = start.value, j = 0; i < end.value; ++i, ++j)
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
      string_set_nth(result, i, fill);
  }

  return result;
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

std::size_t
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

ptr<string>
string_reverse_byte_indexes(context& ctx, ptr<string> s,
                            std::size_t begin, std::size_t end) {
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

static std::size_t
to_byte_index(ptr<> x) {
  if (auto c = match<string_cursor>(x))
    return c->value;
  else
    return expect<integer>(x).value();
}

static ptr<string>
string_reverse(context& ctx, ptr<string> s, ptr<> begin, ptr<> end) {
  return string_reverse_byte_indexes(ctx, s,
                                     to_byte_index(begin), to_byte_index(end));
}

ptr<string>
string_copy_byte_indexes(context& ctx, ptr<string> s,
                         std::size_t begin, std::size_t end) {
  std::string const& value = s->value();

  if (begin > value.size() || end > value.size())
    throw std::runtime_error{"Cursor out of range"};
  if (begin > end)
    throw std::runtime_error{"Invalid index"};

  return make<string>(ctx, value.substr(begin, end - begin));
}

static ptr<string>
string_copy(context& ctx, ptr<string> s, ptr<> begin, ptr<> end) {
  return string_copy_byte_indexes(ctx, s,
                                  to_byte_index(begin), to_byte_index(end));
}

static void
string_copy_mutative_forward(ptr<string> to, std::size_t at,
                             ptr<string> from,
                             std::size_t start, std::size_t end) {
  while (start != end) {
    if (at > to->value().size())
      throw std::runtime_error{"Invalid at index"};
    if (start > from->value().size())
      throw std::runtime_error{"Invalid start/end"};

    char32_t c = string_ref_cursor(from, {start});
    std::size_t c_len = utf8_code_point_byte_length(from->value()[start]);

    to->set_cursor({at}, c);

    start += c_len;
    at += c_len;
  }
}

static std::size_t
find_end_of_target_range(ptr<string> to, std::size_t at,
                         ptr<string> from,
                         std::size_t start, std::size_t end) {
  while (start != end) {
    if (at > to->value().size())
      throw std::runtime_error{"Invalid at index"};
    if (start > from->value().size())
      throw std::runtime_error{"Invalid start/end"};

    std::size_t c_len = utf8_code_point_byte_length(from->value()[start]);
    start += c_len;
    at += c_len;
  }

  return at;
}

static void
string_copy_mutative_backward(ptr<string> to, std::size_t at,
                              ptr<string> from,
                              std::size_t start, std::size_t end) {
  at = find_end_of_target_range(to, at, from, start, end);

  while (start != end) {
    if (end == 0 || end < start)
      throw std::runtime_error{"Invalid start/end"};

    std::size_t before_at = previous_code_point_byte_index(to, at);
    std::size_t before_end = previous_code_point_byte_index(from, end);

    to->set_cursor({before_at}, string_ref_cursor(from, {before_end}));

    at = before_at;
    end = before_end;
  }
}

static void
string_copy_mutative(ptr<string> to, ptr<> at,
                     ptr<string> from, ptr<> start, ptr<> end) {
  std::size_t at_idx = to_byte_index(at);
  std::size_t start_idx = to_byte_index(start);
  std::size_t end_idx = to_byte_index(end);

  if (to == from && start_idx < at_idx)
    string_copy_mutative_backward(to, at_idx, from, start_idx, end_idx);
  else
    string_copy_mutative_forward(to, at_idx, from, start_idx, end_idx);
}

static void
string_append_in_place(ptr<string> s, ptr<string> t) {
  s->append(t->value());
}

static string_cursor
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
    return {haystack_end};
  else
    return {result};
}

static string_cursor
string_contains(ptr<string> haystack, ptr<string> needle,
                ptr<> haystack_start, ptr<> haystack_end,
                ptr<> needle_start, ptr<> needle_end) {
  return string_contains_byte_indexes(haystack, needle,
                                      to_byte_index(haystack_start),
                                      to_byte_index(haystack_end),
                                      to_byte_index(needle_start),
                                      to_byte_index(needle_end));
}

static string_cursor
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
    return {haystack_end};

  auto result = h.rfind(n.data() + needle_start, haystack_end - needle_size,
                        needle_size);
  if (result == std::string::npos || result < haystack_start)
    return {haystack_end};
  else
    return {result};
}

static string_cursor
string_contains_right(ptr<string> haystack, ptr<string> needle,
                      ptr<> haystack_start, ptr<> haystack_end,
                      ptr<> needle_start, ptr<> needle_end) {
  return string_contains_right_byte_indexes(haystack, needle,
                                            to_byte_index(haystack_start),
                                            to_byte_index(haystack_end),
                                            to_byte_index(needle_start),
                                            to_byte_index(needle_end));
}

static integer
string_cursor_diff(ptr<string> s, string_cursor start, string_cursor end) {
  integer::value_type position = 0;

  while (start != end) {
    start = string_cursor_next(s, start);
    ++position;
  }

  return position;
}

static integer
string_cursor_to_index(ptr<string> s, ptr<> x) {
  if (auto c = match<string_cursor>(x))
    return string_cursor_diff(s, {0}, *c);
  else
    return expect<integer>(x);
}

static bool
string_cursor_eq(ptr<> lhs, ptr<> rhs) {
  return to_byte_index(lhs) == to_byte_index(rhs);
}

static bool
string_cursor_lt(ptr<> lhs, ptr<> rhs) {
  return to_byte_index(lhs) < to_byte_index(rhs);
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
  define_constant_evaluable_procedure<&string::length>(ctx, "string-length",
                                                       result);
  define_constant_evaluable_procedure<string_cursor_start>(
    ctx, "string-cursor-start", result
  );
  define_constant_evaluable_procedure<string_cursor_end>(
    ctx, "string-cursor-end", result
  );
  define_constant_evaluable_procedure<string_cursor_next>(
    ctx, "string-cursor-next*", result
  );
  define_constant_evaluable_procedure<string_cursor_prev>(
    ctx, "string-cursor-prev*", result
  );
  define_constant_evaluable_procedure<string_cursor_diff>(
    ctx, "string-cursor-diff*", result
  );
  define_constant_evaluable_procedure<string_cursor_to_index>(
    ctx, "string-cursor->index", result
  );
  define_raw_procedure<string_append>(ctx, "string-append", result);
  define_procedure<string_append_in_place>(ctx, "string-append!", result);
  define_procedure<symbol_to_string>(ctx, "symbol->string", result);
  define_procedure<string_to_symbol>(ctx, "string->symbol", result);
  define_constant_evaluable_procedure<string_ref>(ctx, "string-ref", result);
  define_procedure<string_set>(ctx, "string-set!", result);
  define_procedure<&string::append_char>(ctx, "string-append-char!", result);
  define_constant_evaluable_procedure<is_string_null>(ctx, "string-null?",
                                                      result);
  define_procedure<string_reverse>(ctx, "string-reverse*", result);
  define_procedure<string_copy>(ctx, "string-copy*", result);
  define_procedure<string_copy_mutative>(ctx, "string-copy!*", result);
  define_procedure<string_contains>(
    ctx, "string-contains*", result
  );
  define_procedure<string_contains_right>(
    ctx, "string-contains-right*", result
  );
  define_constant_evaluable_procedure<string_cursor_eq>(ctx, "string-cursor=?",
                                                        result);
  define_constant_evaluable_procedure<string_cursor_lt>(ctx, "string-cursor<?",
                                                        result);
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
  define_procedure<string_to_utf8>(ctx, "string->utf8*", result);
  define_procedure<utf8_to_string>(ctx, "utf8->string*", result);
}

} // namespace insider
