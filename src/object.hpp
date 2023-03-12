#ifndef INSIDER_OBJECT_HPP
#define INSIDER_OBJECT_HPP

#include <fmt/format.h>

#include <array>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <new>
#include <optional>
#include <stdexcept>
#include <string>
#include <type_traits>

namespace insider {

class context;
class integer;
class string_cursor;

using word_type = std::uint64_t;

template <typename = void> class ptr;

using member_visitor = std::function<void(ptr<>)>;

constexpr std::size_t max_types = 256;
constexpr std::size_t first_dynamic_type_index = 64;

// Pointer tagging:
// ... xxxx1 -- integer
// ... xxx00 -- pointer
// ... xx010 -- string cursor
// ... xx110 -- character

constexpr std::size_t character_payload_offset = 3;
constexpr std::size_t string_cursor_payload_offset = 3;

constexpr std::size_t character_tag = 0b110;
constexpr std::size_t string_cursor_tag = 0b010;

// object_storage<T> is standard-layout, and header is its first member. This
// means that an object_storage* and object_header* are
// pointer-interconvertible, and we can reinterpret_cast from one to the other.

using object_header = word_type;

// Object header layout:
//
// | type ... | S | R | A | A | C | C | 1
//
// S: Need to scan this object when promoting?
// R: Object is in remembered set?
// A: Age
// C: Colour
//
// The least significant bit is always 1. It's used to distinguish live objects
// from available storage in fixed-size allocators.

static constexpr word_type alive_mask      = 0b0000001;
static constexpr word_type color_mask      = 0b0000110;
static constexpr word_type age_mask        = 0b0011000;
static constexpr word_type remembered_mask = 0b0100000;
static constexpr word_type need_scan_mask  = 0b1000000;

static constexpr word_type color_shift = 1;
static constexpr word_type age_shift = 3;
static constexpr word_type remembered_shift = 5;
static constexpr word_type need_scan_shift = 6;
static constexpr word_type type_shift = 7;

inline word_type
header_type(object_header h) { return h >> type_shift; }

static constexpr word_type mature_age = 3;

enum class color : word_type {
  white = 0,
  grey = 1,
  black = 2,
};

inline word_type
header_age(object_header h) {
  return (h & age_mask) >> age_shift;
}

inline void
set_header_age(object_header& h, word_type a) {
  h = (h & ~age_mask) | (a << age_shift);
}

inline color
header_color(object_header h) {
  return static_cast<color>((h & color_mask) >> color_shift);
}

inline void
set_header_color(object_header& h, color c) {
  h = (h & ~color_mask) | (static_cast<word_type>(c) << color_shift);
}

inline bool
header_remembered(object_header h) {
  return (h & remembered_mask) != 0;
}

inline void
set_header_remembered(object_header& h, bool r) {
  h = (h & ~remembered_mask) | (static_cast<word_type>(r) << remembered_shift);
}

inline bool
header_needs_scan_on_promote(object_header h) {
  return (h & need_scan_mask) != 0;
}

inline void
set_header_needs_scan_on_promote(object_header& h, bool s) {
  h = (h & ~need_scan_mask) | (static_cast<word_type>(s) << need_scan_shift);
}

inline object_header
make_header(word_type type) {
  return (type << type_shift) | alive_mask;
}

static_assert(sizeof(object_header) == sizeof(word_type));

constexpr std::size_t object_alignment = sizeof(word_type);

template <typename T>
struct alignas(object_alignment) object_storage {
  object_header header;
  std::aligned_storage_t<sizeof(T), alignof(T)> payload_storage;

  T*
  object() {
    return std::launder(reinterpret_cast<T*>(&payload_storage));
  }

  T const*
  object() const {
    return std::launder(reinterpret_cast<T const*>(&payload_storage));
  }
};

template <typename T>
constexpr std::size_t payload_offset
  = offsetof(object_storage<T>, payload_storage);

template <typename T>
inline object_header*
object_header_address(T* o) {
  return reinterpret_cast<object_header*>(
    reinterpret_cast<std::byte*>(o) - payload_offset<T>
  );
}

inline bool
is_object_address(word_type a) {
  return (a & 0b11) == 0b00;
}

// Non-tracked pointer to a Scheme object, including to immediate values such as
// fixnums. ptr<> is a pointer to any object, ptr<T> is a pointer to an object
// of type T.
template <>
class ptr<> {
public:
  ptr() = default;

  explicit
  ptr(word_type value)
    : value_{reinterpret_cast<object_header*>(value)}
  {
    assert(!is_object_address(value));
  }

  explicit
  ptr(object_header* header)
    : value_{header}
  { }

  ptr(auto* value) {
    if (value)
      value_ = object_header_address(value);
  }

  ptr(std::nullptr_t) { }

  explicit
  operator bool () const { return value_ != nullptr; }

  void
  reset() { value_ = nullptr; }

  void
  reset(ptr<> new_value) { value_ = new_value.value_; }

  word_type
  as_word() const { return reinterpret_cast<word_type>(value_); }

  object_header*
  header() const { return value_; }

  friend auto
  operator <=> (ptr const&, ptr const&) = default;

protected:
  object_header* value_ = nullptr;
};

inline bool
operator == (ptr<> p, std::nullptr_t) {
  return p.as_word() == 0;
}

template <typename T>
class ptr : public ptr<> {
public:
  ptr() = default;

  explicit
  ptr(object_header* header) : ptr<>{header} { }

  explicit
  ptr(word_type w) : ptr<>{w} { }

  explicit
  ptr(object_storage<T>* storage)
    : ptr<>{reinterpret_cast<object_header*>(storage)}
  { }

  ptr(T* value) : ptr<>(value) { }

  ptr(std::nullptr_t) { }

  T*
  operator -> () const { return value(); }

  T&
  operator * () const { return *value(); }

  auto&
  operator ->* (auto T::* ptr) const { return value()->*ptr; }

  template <typename Ret, typename... Args>
  auto
  operator ->* (Ret (T::* fun)(Args...)) const {
    return [fun, this] (auto&&... args) {
      return (value()->*fun)(std::forward<decltype(args)>(args)...);
    };
  }

  template <typename Ret, typename... Args>
  auto
  operator ->* (Ret (T::* fun)(Args...) const) const {
    return [fun, this] (auto&&... args) {
      return (value()->*fun)(std::forward<decltype(args)>(args)...);
    };
  }

  T*
  value() const { return storage()->object(); }

  object_storage<T>*
  storage() const {
    return reinterpret_cast<object_storage<T>*>(value_);
  }
};

template <typename>
bool
is(ptr<>);

template <typename T>
ptr<T>
ptr_cast(ptr<> value) {
  assert(!value || is<T>(value));
  return ptr<T>{value.header()};
}

template <>
inline ptr<>
ptr_cast<void>(ptr<> value) { return value; }

inline bool
is_object_ptr(ptr<> o) {
  return is_object_address(o.as_word());
}

inline bool
is_fixnum(ptr<> o) {
  return (o.as_word() & 0b01) == 0b01;
}

template <std::size_t Tag>
inline bool
is_tagged(ptr<> o) {
  return (o.as_word() & 0b111) == Tag;
}

inline bool
is_character(ptr<> o) {
  return is_tagged<character_tag>(o);
}

inline bool
is_string_cursor(ptr<> o) {
  return is_tagged<string_cursor_tag>(o);
}

template <typename T>
concept has_static_type_index = requires {
  { T::static_type_index } -> std::convertible_to<word_type>;
};

struct type_descriptor {
  char const* name = "invalid";
  void (*destroy)(object_header*) = nullptr;
  void (*visit_members)(object_header const*, member_visitor const&) = nullptr;

  bool constant_size = true;
  std::size_t size = 0;
  std::size_t (*get_size)(object_header const*) = nullptr;
  std::size_t (*storage_size)(object_header const*) = nullptr;
};

struct type_vector {
  std::array<type_descriptor, max_types> types;
  std::size_t                            size;
};

inline type_vector&
types() {
  static type_vector value{{}, first_dynamic_type_index};
  return value;
}

word_type
new_type(type_descriptor, std::optional<word_type> index);

constexpr word_type invalid_type = 0;
constexpr word_type no_type = std::numeric_limits<word_type>::max();

inline word_type
type_index(object_header const* h) { return header_type(*h); }

inline word_type
object_type_index(ptr<> o) { return type_index(o.header()); }

inline std::string
type_name(word_type index) { return types().types[index].name; }

inline type_descriptor const&
object_type(object_header const* h) { return types().types[header_type(*h)]; }

inline type_descriptor const&
object_type(ptr<> o) { return object_type(o.header()); }

inline word_type
object_hash(object_header const* h) {
  return reinterpret_cast<word_type>(h);
}

inline word_type
object_hash(ptr<> o) { return object_hash(o.header()); }

inline word_type
tagged_payload(ptr<> o) {
  assert(!is_object_ptr(o));
  return o.as_word();
}

inline ptr<>
immediate_to_ptr(word_type w) noexcept {
  assert(w & 0b11);
  return ptr<>{w};
}

constexpr char const* integer_type_name = "insider::integer";
constexpr char const* character_type_name = "insider::character";
constexpr char const* string_cursor_type_name = "insider::string_cursor";

inline std::string
object_type_name(ptr<> o) {
  if (!o)
    return "<null>";
  else if (is_object_ptr(o))
    return type_name(object_type_index(o));
  else if (is_fixnum(o))
    return integer_type_name;
  else if (is_character(o))
    return character_type_name;
  else if (is_string_cursor(o))
    return string_cursor_type_name;
  else
    assert(false);
  return "";
}

inline std::size_t
storage_size(object_header const* header) {
  return object_type(header).storage_size(header);
}

template <typename T>
std::string
type_name() {
  return type_name(T::type_index);
}

template <>
inline std::string
type_name<integer>() {
  return integer_type_name;
}

template <>
inline std::string
type_name<char32_t>() {
  return character_type_name;
}

template <>
inline std::string
type_name<string_cursor>() {
  return string_cursor_type_name;
}

// Is a given object an instance of the given Scheme type?
template <has_static_type_index T>
bool
is(ptr<> x) {
  assert(x);
  return is_object_ptr(x) && object_type_index(x) == T::static_type_index;
}

template <typename T>
bool
is(ptr<> x) {
  assert(x);
  return is_object_ptr(x) && object_type_index(x) == T::type_index;
}

template <>
inline bool
is<integer>(ptr<> x) {
  return is_fixnum(x);
}

template <>
inline bool
is<char32_t>(ptr<> x) {
  return is_character(x);
}

template <>
inline bool
is<string_cursor>(ptr<> x) {
  return is_string_cursor(x);
}

namespace detail {
  constexpr std::size_t
  round_to_words(std::size_t s) {
    return (s + sizeof(word_type) - 1) & -sizeof(word_type);
  }

  template <typename T>
  void
  destroy(object_header* h) {
    auto storage = reinterpret_cast<object_storage<T>*>(h);
    storage->object()->~T();
  }

  template <typename T>
  void
  visit_members(object_header const* o, member_visitor const& f) {
    reinterpret_cast<object_storage<T> const*>(o)->object()->visit_members(f);
  }

  template <typename T, typename U>
  std::size_t
  size(object_header const* o) {
    auto storage = reinterpret_cast<object_storage<T> const*>(o);
    return sizeof(T)
           + detail::round_to_words(storage->object()->size_ * sizeof(U));
  }

  template <typename T>
  std::size_t
  storage_size(object_header const* o) {
    auto storage = reinterpret_cast<object_storage<T> const*>(o);
    return sizeof(*storage);
  }

  template <typename T, typename U>
  std::size_t
  dynamic_storage_size(object_header const* o) {
    auto storage = reinterpret_cast<object_storage<T> const*>(o);
    return sizeof(*storage)
           + detail::round_to_words(storage->object()->size_ * sizeof(U));
  }
}

// Object with no Scheme subobjects.
template <typename Derived>
struct leaf_object {
  static constexpr bool is_dynamic_size = false;
  static word_type const type_index;
};

namespace detail {
  template <has_static_type_index T>
  std::optional<word_type>
  get_type_index() { return T::static_type_index; }

  template <typename>
  std::optional<word_type>
  get_type_index() { return std::nullopt; }
}

template <typename Derived>
word_type const leaf_object<Derived>::type_index = new_type(
  type_descriptor{
    Derived::scheme_name,
    detail::destroy<Derived>,
    [] (object_header const*, member_visitor const&) { },
    true,
    detail::round_to_words(sizeof(Derived)),
    nullptr,
    detail::storage_size<Derived>
  },
  detail::get_type_index<Derived>()
);

// Object with a constant number of Scheme subobjects.
template <typename Derived>
struct composite_object {
  static constexpr bool is_dynamic_size = false;
  static word_type const type_index;
};

template <typename Derived>
word_type const composite_object<Derived>::type_index = new_type(
  type_descriptor{
    Derived::scheme_name,
    detail::destroy<Derived>,
    detail::visit_members<Derived>,
    true,
    detail::round_to_words(sizeof(Derived)),
    nullptr,
    detail::storage_size<Derived>
  },
  detail::get_type_index<Derived>()
);

// Object whose size is determined at instantiation time.
template <typename Derived, typename T>
struct alignas(T) alignas(object_alignment) dynamic_size_object {
  using element_type = T;
  static constexpr bool is_dynamic_size = true;
  static word_type const type_index;

  explicit
  dynamic_size_object(std::size_t size)
    : size_{size}
  { }

  std::size_t
  size() const { return size_; }

protected:
  template <typename, typename>
  friend std::size_t
  detail::size(object_header const*);

  template <typename, typename>
  friend std::size_t
  detail::dynamic_storage_size(object_header const*);

  T&
  storage_element(std::size_t i) {
    return reinterpret_cast<T*>(
      reinterpret_cast<std::byte*>(this) + sizeof(Derived)
    )[i];
  }

  T const&
  storage_element(std::size_t i) const {
    return reinterpret_cast<T const*>(
      reinterpret_cast<std::byte const*>(this) + sizeof(Derived)
    )[i];
  }

  std::size_t size_;
};

template <typename Derived, typename T>
word_type const dynamic_size_object<Derived, T>::type_index
  = new_type(
      type_descriptor{
        Derived::scheme_name,
        detail::destroy<Derived>,
        detail::visit_members<Derived>,
        false,
        0,
        detail::size<Derived, T>,
        detail::dynamic_storage_size<Derived, T>
      },
      detail::get_type_index<Derived>()
    );

inline word_type
object_age(ptr<> o) { return header_age(*o.header()); }

inline bool
object_remembered(ptr<> o) { return header_remembered(*o.header()); }

inline void
mark_object_remembered(ptr<> o) { set_header_remembered(*o.header(), true); }

inline void
mark_needs_scan_on_promote(ptr<> o) {
  set_header_needs_scan_on_promote(*o.header(), true);
}

template <typename T>
concept visitable = requires (T& t, member_visitor const& f) {
  t.visit_members(f);
};

inline void
visit_members(ptr<> p, member_visitor const& f) {
  f(p);
}

void
visit_members(visitable auto& x, member_visitor const& f) {
  x.visit_members(f);
}

} // namespace insider

namespace std {
  template <typename T>
  struct hash<insider::ptr<T>> {
    auto
    operator () (insider::ptr<T> value) const {
      return std::hash<insider::word_type>{}(value.as_word());
    }
  };
} // namespace std

#endif
