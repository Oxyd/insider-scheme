#ifndef INSIDER_OBJECT_HPP
#define INSIDER_OBJECT_HPP

#include "memory/root_provider.hpp"
#include "ptr.hpp"

#include <fmt/format.h>

#include <cstdint>
#include <optional>
#include <stdexcept>
#include <string>

namespace insider {

class context;
class integer;

using word_type = std::uint64_t;

struct type_descriptor {
  char const* name;
  void (*destroy)(ptr<>);
  ptr<> (*move)(ptr<>, std::byte*);
  void (*visit_members)(ptr<>, member_visitor const&);

  bool constant_size;
  std::size_t size = 0;
  std::size_t (*get_size)(ptr<>) = nullptr;
};

// Base for any garbage-collectable Scheme object.
class alignas(sizeof(word_type)) object {
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

static constexpr word_type color_shift = 33;
static constexpr word_type generation_shift = 35;
static constexpr word_type type_shift = 37;
static constexpr word_type hash_shift = 1;
static constexpr word_type hash_width = 32;

static constexpr word_type alive_bit = word_type{1};
static constexpr word_type color_bits = (word_type{1} << color_shift)
                                      | (word_type{1} << (color_shift + 1));
static constexpr word_type generation_bits
  = (word_type{1} << generation_shift)
    | (word_type{1} << (generation_shift + 1));
static constexpr word_type hash_bits
  = ((word_type{1} << hash_width) - 1) << hash_shift;

inline word_type
clamp_hash(word_type h) {
  return h & ((static_cast<word_type>(1) << hash_width) - 1);
}

inline std::vector<type_descriptor>&
types() {
  static std::vector<type_descriptor> value;
  return value;
}

word_type
new_type(type_descriptor);

static constexpr word_type no_type = std::numeric_limits<word_type>::max();

// Pointer tagging:
// ... xxx1 -- integer
// ... xx10 -- character
// ... xx00 -- pointer

inline bool
is_object_ptr(ptr<> o) {
  return (reinterpret_cast<word_type>(o.value()) & 0b11) == 0b00;
}

inline bool
is_fixnum(ptr<> o) {
  return (reinterpret_cast<word_type>(o.value()) & 0b01) == 0b01;
}

inline bool
is_character(ptr<> o) {
  return (reinterpret_cast<word_type>(o.value()) & 0b11) == 0b10;
}

inline word_type&
header_word(ptr<> o) {
  assert(is_object_ptr(o));
  return *reinterpret_cast<word_type*>(
    reinterpret_cast<std::byte*>(o.value()) - sizeof(word_type)
  );
}

inline word_type
type_index(word_type header) { return header >> type_shift; }

inline word_type
object_type_index(ptr<> o) { return type_index(header_word(o)); }

inline std::string
type_name(word_type index) { return types()[index].name; }

inline type_descriptor const&
object_type(word_type header) { return types()[type_index(header)]; }

inline type_descriptor const&
object_type(ptr<> o) { return object_type(header_word(o)); }

inline word_type
object_hash(ptr<> o) { return (header_word(o) & hash_bits) >> hash_shift; }

inline word_type
tagged_payload(ptr<> o) {
  assert(!is_object_ptr(o));
  return reinterpret_cast<word_type>(o.value());
}

inline ptr<>
immediate_to_ptr(word_type w) noexcept {
  assert(w & 0b11);
  return ptr<>{reinterpret_cast<object*>(w)};
}

constexpr char const* integer_type_name = "insider::integer";
constexpr char const* character_type_name = "insider::character";

inline std::string
object_type_name(ptr<> o) {
  if (is_object_ptr(o))
    return type_name(object_type_index(o));
  else if (is_fixnum(o))
    return integer_type_name;
  else if (is_character(o))
    return character_type_name;
  else
    assert(false);
  return "";
}

inline std::size_t
object_size(ptr<> o) {
  type_descriptor const& t = object_type(o);
  return t.constant_size ? t.size : t.get_size(o);
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

// Is a given object an instance of the given Scheme type?
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

namespace detail {
  constexpr std::size_t
  round_to_words(std::size_t s) {
    return (s + sizeof(word_type) - 1) & -sizeof(word_type);
  }

  template <typename T>
  void
  destroy(ptr<> o) { static_cast<T*>(o.value())->~T(); }

  template <typename T>
  ptr<>
  move(ptr<> o, std::byte* storage) {
    return new (storage) T(std::move(*static_cast<T*>(o.value())));
  }

  template <typename T>
  void
  visit_members(ptr<> o, member_visitor const& f) {
    static_cast<T*>(o.value())->visit_members(f);
  }

  template <typename T, typename U>
  std::size_t
  size(ptr<> o) {
    return sizeof(T)
           + detail::round_to_words(
               static_cast<T*>(o.value())->size_ * sizeof(U)
             );
  }
}

// Object with no Scheme subobjects.
template <typename Derived>
struct leaf_object : object {
  static word_type const type_index;
};

template <typename Derived>
word_type const leaf_object<Derived>::type_index = new_type(type_descriptor{
  Derived::scheme_name,
  detail::destroy<Derived>,
  detail::move<Derived>,
  [] (ptr<>, member_visitor const&) { },
  true,
  detail::round_to_words(sizeof(Derived)),
  nullptr
});

// Object with a constant number of Scheme subobjects.
template <typename Derived>
struct composite_object : object {
  static word_type const type_index;
};

template <typename Derived>
word_type const composite_object<Derived>::type_index = new_type(type_descriptor{
  Derived::scheme_name,
  detail::destroy<Derived>,
  detail::move<Derived>,
  detail::visit_members<Derived>,
  true,
  detail::round_to_words(sizeof(Derived)),
  nullptr
});

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
  detail::size(ptr<>);

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
  = new_type(type_descriptor{
      Derived::scheme_name,
      detail::destroy<Derived>,
      detail::move<Derived>,
      detail::visit_members<Derived>,
      false,
      0,
      detail::size<Derived, T>
    });

enum class generation : word_type {
  stack     = 0,
  nursery_1 = 1,
  nursery_2 = 2,
  mature    = 3
};

inline void
init_object_header(std::byte* storage, word_type type, word_type hash,
                   generation gen = generation::nursery_1) {
  new (storage) word_type((type << type_shift)
                          | alive_bit
                          | (static_cast<word_type>(gen) << generation_shift)
                          | (hash << hash_shift));
}

namespace detail {
  inline generation
  get_generation(word_type header) {
    return static_cast<generation>(
      (header & generation_bits) >> generation_shift
    );
  }
}

inline generation
object_generation(ptr<> o) { return detail::get_generation(header_word(o)); }

class hash_generator {
public:
  word_type
  operator () () { return clamp_hash(next_hash_++); }

private:
  word_type next_hash_ = 0;
};

} // namespace insider

#endif
