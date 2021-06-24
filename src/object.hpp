#ifndef INSIDER_OBJECT_HPP
#define INSIDER_OBJECT_HPP

#include "ptr.hpp"

#include <fmt/format.h>

#include <cstdint>
#include <optional>
#include <stdexcept>
#include <string>

namespace insider {

class context;

using word_type = std::uint64_t;
using member_visitor = std::function<void(ptr<>&)>;

struct type_descriptor {
  char const* name;
  void (*destroy)(ptr<>);
  ptr<> (*move)(ptr<>, std::byte*);
  void (*visit_members)(ptr<>, member_visitor const&);
  std::size_t (*hash)(ptr<>);

  bool constant_size;
  std::size_t size = 0;
  std::size_t (*get_size)(ptr<>) = nullptr;
  bool permanent_root = false;
};

// Base for any garbage-collectable Scheme object.
struct alignas(sizeof(word_type)) object {
  static constexpr bool is_dynamic_size = false;
};

// Object header word:
//
// Bits:   63 ..  5    ..      3   ..   1       0
// Fields: | type | generation | colour | alive |

static constexpr word_type alive_shift = 0;
static constexpr word_type color_shift = 1;
static constexpr word_type generation_shift = 3;
static constexpr word_type type_shift = 5;

static constexpr word_type alive_bit = 1 << alive_shift;
static constexpr word_type color_bits = (1 << color_shift) | (1 << (color_shift + 1));
static constexpr word_type generation_bits = (1 << generation_shift) | (1 << (generation_shift + 1));

inline std::vector<type_descriptor>&
types() {
  static std::vector<type_descriptor> value;
  return value;
}

word_type
new_type(type_descriptor);

inline bool
is_object_ptr(ptr<> o) {
  return !(reinterpret_cast<word_type>(o.value()) & 1);
}

inline bool
is_fixnum(ptr<> o) { return !is_object_ptr(o); }

inline word_type&
header_word(ptr<> o) {
  assert(is_object_ptr(o));
  return *reinterpret_cast<word_type*>(reinterpret_cast<std::byte*>(o.value()) - sizeof(word_type));
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
tagged_payload(ptr<> o) {
  assert(!is_object_ptr(o));
  return reinterpret_cast<word_type>(o.value());
}

inline ptr<>
immediate_to_ptr(word_type w) noexcept {
  assert(w & 1);
  return ptr<>{reinterpret_cast<object*>(w)};
}

constexpr char const* integer_type_name = "insider::fixnum";

inline std::string
object_type_name(ptr<> o) {
  return is_object_ptr(o) ? type_name(object_type_index(o)) : integer_type_name;
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

  template <typename T>
  std::size_t
  hash(ptr<> o) {
    return static_cast<T const*>(o.value())->hash();
  }

  template <typename T, typename U>
  std::size_t
  size(ptr<> o) {
    return sizeof(T) + detail::round_to_words(static_cast<T*>(o.value())->size() * sizeof(U));
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
  detail::hash<Derived>,
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
  detail::hash<Derived>,
  true,
  detail::round_to_words(sizeof(Derived)),
  nullptr
});

// Object that is allocated directly in the mature generation and is always
// considered a source of roots for all generations.
template <typename Derived>
struct composite_root_object : object {
  static word_type const type_index;
};

template <typename Derived>
word_type const composite_root_object<Derived>::type_index = new_type(type_descriptor{
  Derived::scheme_name,
  detail::destroy<Derived>,
  detail::move<Derived>,
  detail::visit_members<Derived>,
  detail::hash<Derived>,
  true,
  detail::round_to_words(sizeof(Derived)),
  nullptr,
  true
});

// Object whose size is determined at instantiation time.
template <typename Derived, typename T, bool PermanentRoot = false>
struct alignas(T) alignas(object) dynamic_size_object : object {
  using element_type = T;
  static constexpr bool is_dynamic_size = true;
  static word_type const type_index;

protected:
  T&
  storage_element(std::size_t i) {
    return reinterpret_cast<T*>(reinterpret_cast<std::byte*>(this) + sizeof(Derived))[i];
  }

  T const&
  storage_element(std::size_t i) const {
    return reinterpret_cast<T const*>(reinterpret_cast<std::byte const*>(this) + sizeof(Derived))[i];
  }
};

template <typename Derived, typename T, bool PermanentRoot>
word_type const dynamic_size_object<Derived, T, PermanentRoot>::type_index = new_type(type_descriptor{
  Derived::scheme_name,
  detail::destroy<Derived>,
  detail::move<Derived>,
  detail::visit_members<Derived>,
  detail::hash<Derived>,
  false,
  0,
  detail::size<Derived, T>,
  PermanentRoot
});

template <typename T>
bool
is(tracked_ptr<> const& x) {
  return is<T>(x.get());
}

enum class generation : word_type {
  stack     = 0,
  nursery_1 = 1,
  nursery_2 = 2,
  mature    = 3
};

inline void
init_object_header(std::byte* storage, word_type type, generation gen = generation::nursery_1) {
  new (storage) word_type((type << type_shift)
                          | alive_bit
                          | (static_cast<word_type>(gen) << generation_shift));
}

namespace detail {
  inline generation
  get_generation(word_type header) {
    return static_cast<generation>((header & generation_bits) >> generation_shift);
  }
}

inline generation
object_generation(ptr<> o) { return detail::get_generation(header_word(o)); }

generation
object_generation(ptr<>);

} // namespace insider

#endif
