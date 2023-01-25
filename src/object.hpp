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
class member_visitor;
class string_cursor;

using word_type = std::uint64_t;

constexpr std::size_t object_alignment = sizeof(word_type);

// Base for any garbage-collectable Scheme object.
class alignas(object_alignment) object {
public:
  static constexpr bool is_dynamic_size = false;
};

// Live object header word:
//
// Bits:   63     37           35       33          1       0
// Fields: | type | generation | colour | hash code | alive |
//
// Things to note:
//   - alive bit is the least significant one: This is because a dead object's
//     header word will contain the forwarding address, whose least significant
//     bit will be 0.
//
//   - type is most significant: This is so it can be accessed by a simple
//     bit shift

constexpr word_type color_shift = 33;
constexpr word_type generation_shift = 35;
constexpr word_type type_shift = 37;
constexpr word_type hash_shift = 1;
constexpr word_type hash_width = 32;

constexpr word_type alive_bit = word_type{1};
constexpr word_type color_bits = (word_type{1} << color_shift)
                                      | (word_type{1} << (color_shift + 1));
constexpr word_type generation_bits
  = (word_type{1} << generation_shift)
    | (word_type{1} << (generation_shift + 1));
constexpr word_type hash_bits
  = ((word_type{1} << hash_width) - 1) << hash_shift;

inline word_type
clamp_hash(word_type h) {
  return h & ((static_cast<word_type>(1) << hash_width) - 1);
}

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

struct alignas(object_alignment) abstract_object_storage {
  word_type header;
};

template <typename T>
struct alignas(object_alignment) object_storage : abstract_object_storage {
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

// Non-tracked pointer to a Scheme object, including to immediate values such as
// fixnums. ptr<> is a pointer to any object, ptr<T> is a pointer to an object
// of type T.
template <typename = void>
class ptr;

namespace detail {
  // The free store needs to be able to update pointers when it moves objects.
  // This update makes a pointer point to the same object representing the same
  // value, except at a new location. Since this isn't morally a modification of
  // the pointer, we allow the free store to mutate this value even for const
  // ptr<>'s.

  void
  update_ptr(ptr<> const& p, ptr<> new_value);
}

inline word_type
object_header_address(object* o) {
  return reinterpret_cast<word_type>(
    reinterpret_cast<std::byte*>(o) - sizeof(word_type)
  );
}

inline object*
object_address(word_type w) {
  return reinterpret_cast<object*>(w + sizeof(word_type));
}

template <>
class ptr<> {
public:
  ptr() = default;

  explicit
  ptr(word_type value)
    : value_{value}
  { }

  explicit
  ptr(abstract_object_storage* storage)
    : value_{reinterpret_cast<word_type>(storage)}
  { }

  ptr(object* value) {
    if (value)
      value_ = object_header_address(value);
  }

  ptr(std::nullptr_t) { }

  explicit
  operator bool () const { return value_ != 0; }

  void
  reset() { value_ = 0; }

  void
  reset(ptr<> new_value) { value_ = new_value.value_; }

  word_type
  as_word() const { return value_; }

  word_type&
  header() const {
    return storage()->header;
  }

  object*
  value() const {
    if (value_)
      return object_address(value_);
    else
      return nullptr;
  }

  abstract_object_storage*
  storage() const {
    if (value_)
      return reinterpret_cast<abstract_object_storage*>(value_);
    else
      return nullptr;
  }

  friend auto
  operator <=> (ptr const&, ptr const&) = default;

protected:
  friend void detail::update_ptr(ptr<> const&, ptr<>);

  mutable word_type value_ = 0;
};

inline bool
operator == (ptr<> p, std::nullptr_t) {
  return p.value() == nullptr;
}

template <typename T>
class ptr : public ptr<> {
public:
  ptr() = default;

  explicit
  ptr(word_type w) : ptr<>{w} { }

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
    if (value_)
      return reinterpret_cast<object_storage<T>*>(value_);
    else
      return nullptr;
  }
};

template <typename>
bool
is(ptr<>);

template <typename T>
ptr<T>
ptr_cast(ptr<> value) {
  assert(!value || is<T>(value));
  return ptr<T>{value.as_word()};
}

template <>
inline ptr<>
ptr_cast<void>(ptr<> value) { return value; }

inline bool
is_object_ptr(ptr<> o) {
  return (reinterpret_cast<word_type>(o.value()) & 0b11) == 0b00;
}

inline bool
is_fixnum(ptr<> o) {
  return (reinterpret_cast<word_type>(o.value()) & 0b01) == 0b01;
}

template <std::size_t Tag>
inline bool
is_tagged(ptr<> o) {
  return (reinterpret_cast<word_type>(o.value()) & 0b111) == Tag;
}

inline bool
is_character(ptr<> o) {
  return is_tagged<character_tag>(o);
}

inline bool
is_string_cursor(ptr<> o) {
  return is_tagged<string_cursor_tag>(o);
}

inline word_type&
header_word(ptr<> o) {
  assert(is_object_ptr(o));
  return o.header();
}

template <typename T>
concept has_static_type_index = requires {
  { T::static_type_index } -> std::convertible_to<word_type>;
};

struct type_descriptor {
  char const* name = "invalid";
  void (*destroy)(abstract_object_storage*) = nullptr;
  ptr<> (*move)(abstract_object_storage*, abstract_object_storage*) = nullptr;
  void (*visit_members)(abstract_object_storage*,
                        member_visitor const&) = nullptr;

  bool constant_size = true;
  std::size_t size = 0;
  std::size_t (*get_size)(abstract_object_storage*) = nullptr;
  std::size_t (*storage_size)(abstract_object_storage*) = nullptr;
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
type_index(word_type header) { return header >> type_shift; }

inline word_type
object_type_index(ptr<> o) { return type_index(header_word(o)); }

inline std::string
type_name(word_type index) { return types().types[index].name; }

inline type_descriptor const&
object_type(word_type header) { return types().types[type_index(header)]; }

inline type_descriptor const&
object_type(ptr<> o) { return object_type(header_word(o)); }

inline type_descriptor const&
object_type(abstract_object_storage* o) { return object_type(o->header); }

inline word_type
object_hash(word_type header) {
  return (header & hash_bits) >> hash_shift;
}

inline word_type
object_hash(ptr<> o) { return object_hash(header_word(o)); }

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
storage_size(abstract_object_storage* storage) {
  return object_type(storage->header).storage_size(storage);
}

inline std::size_t
object_size(ptr<> o) {
  type_descriptor const& t = object_type(o);
  return t.constant_size ? t.size : t.get_size(o.storage());
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
  destroy(abstract_object_storage* o) {
    auto storage = static_cast<object_storage<T>*>(o);
    storage->object()->~T();
    std::uninitialized_fill_n(
      reinterpret_cast<std::byte*>(&storage->payload_storage),
      sizeof(T),
      std::byte{0xAA}
    );
  }

  template <typename T>
  ptr<>
  move(abstract_object_storage* from, abstract_object_storage* to) {
    auto from_storage = static_cast<object_storage<T>*>(from);
    auto to_storage = static_cast<object_storage<T>*>(to);
    return new (&to_storage->payload_storage) T(
      std::move(*from_storage->object())
    );
  }

  template <typename T>
  void
  visit_members(abstract_object_storage* o, member_visitor const& f) {
    static_cast<object_storage<T>*>(o)->object()->visit_members(f);
  }

  template <typename T, typename U>
  std::size_t
  size(abstract_object_storage* o) {
    auto storage = static_cast<object_storage<T>*>(o);
    return sizeof(T)
           + detail::round_to_words(storage->object()->size_ * sizeof(U));
  }

  template <typename T>
  std::size_t
  storage_size(abstract_object_storage* o) {
    auto storage = static_cast<object_storage<T>*>(o);
    return sizeof(*storage);
  }

  template <typename T, typename U>
  std::size_t
  dynamic_storage_size(abstract_object_storage* o) {
    auto storage = static_cast<object_storage<T>*>(o);
    return sizeof(*storage)
           + detail::round_to_words(storage->object()->size_ * sizeof(U));
  }
}

// Object with no Scheme subobjects.
template <typename Derived>
struct leaf_object : object {
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
    detail::move<Derived>,
    [] (abstract_object_storage*, member_visitor const&) { },
    true,
    detail::round_to_words(sizeof(Derived)),
    nullptr,
    detail::storage_size<Derived>
  },
  detail::get_type_index<Derived>()
);

// Object with a constant number of Scheme subobjects.
template <typename Derived>
struct composite_object : object {
  static word_type const type_index;
};

template <typename Derived>
word_type const composite_object<Derived>::type_index = new_type(
  type_descriptor{
    Derived::scheme_name,
    detail::destroy<Derived>,
    detail::move<Derived>,
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
struct alignas(T) alignas(object) dynamic_size_object : object {
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
  detail::size(abstract_object_storage*);

  template <typename, typename>
  friend std::size_t
  detail::dynamic_storage_size(abstract_object_storage*);

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
        detail::move<Derived>,
        detail::visit_members<Derived>,
        false,
        0,
        detail::size<Derived, T>,
        detail::dynamic_storage_size<Derived, T>
      },
      detail::get_type_index<Derived>()
    );

enum class generation : word_type {
  nursery_1,
  nursery_2,
  mature
};

inline word_type
make_object_header(word_type type, word_type hash,
                   generation gen = generation::nursery_1) {
  return word_type((type << type_shift)
                   | alive_bit
                   | (static_cast<word_type>(gen) << generation_shift)
                   | (hash << hash_shift));
}

inline void
init_object_header(std::byte* storage, word_type type, word_type hash,
                   generation gen = generation::nursery_1) {
  new (storage) word_type(make_object_header(type, hash, gen));
}

inline generation
object_generation(word_type header) {
  return static_cast<generation>((header & generation_bits) >> generation_shift);
}

inline generation
object_generation(ptr<> o) { return object_generation(header_word(o)); }

class hash_generator {
public:
  word_type
  operator () () { return clamp_hash(next_hash_++); }

private:
  word_type next_hash_ = 0;
};

} // namespace insider

namespace std {
  template <typename T>
  struct hash<insider::ptr<T>> {
    auto
    operator () (insider::ptr<T> value) const {
      return std::hash<insider::object*>{}(value.value());
    }
  };
} // namespace std

#endif
