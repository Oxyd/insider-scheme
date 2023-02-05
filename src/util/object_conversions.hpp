#ifndef INSIDER_UTIL_OBJECT_CONVERSIONS_HPP
#define INSIDER_UTIL_OBJECT_CONVERSIONS_HPP

#include "memory/root_ptr.hpp"
#include "object.hpp"
#include "runtime/character.hpp"
#include "runtime/error.hpp"
#include "runtime/integer.hpp"
#include "runtime/string.hpp"

namespace insider {

namespace detail {
  template <typename T>
  struct expect_helper {
    static ptr<T>
    expect(ptr<> x, std::string_view message) {
      if (is<T>(x))
        return ptr_cast<T>(x);
      else {
        if (!message.empty())
          throw std::runtime_error{std::string(message)};
        else
          throw make_type_error<T>(x);
      }
    }

    static root_ptr<T>
    expect(root_ptr<> const& x, std::string_view message) {
      return {x.list(), expect(x.get(), message)};
    }
  };

  template <>
  struct expect_helper<void> {
    static ptr<>
    expect(ptr<> x, std::string_view) {
      return x;
    }

    static root_ptr<>
    expect(root_ptr<> const& x, std::string_view) {
      return x;
    }
  };

  template <typename ImmediateT, auto Converter>
  struct immediate_expect_helper {
    static ImmediateT
    expect(ptr<> x, std::string_view message) {
      if (is<ImmediateT>(x))
        return Converter(x);
      else
        throw !message.empty()
              ? std::runtime_error{std::string(message)}
              : make_type_error<ImmediateT>(x);
    }

    static ImmediateT
    expect(root_ptr<> const& x, std::string_view message) {
      return expect(x.get(), message);
    }
  };

  template <>
  struct expect_helper<integer>
    : immediate_expect_helper<integer, ptr_to_integer>
  { };

  template <>
  struct expect_helper<char32_t>
    : immediate_expect_helper<char32_t, ptr_to_character>
  { };

  template <>
  struct expect_helper<string_cursor>
    : immediate_expect_helper<string_cursor, ptr_to_string_cursor>
  { };
}

template <typename T>
struct value_type_for {
  using type = ptr<T>;
};

template <>
struct value_type_for<integer> {
  using type = integer;
};

template <>
struct value_type_for<char32_t> {
  using type = char32_t;
};

template <typename T>
using value_type_for_t = typename value_type_for<T>::type;

// Expect an object to be of given type and return the apropriate typed pointer
// to the object. Throws type_error if the object isn't of the required type.
template <typename T>
auto
expect(ptr<> x) {
  return detail::expect_helper<T>::expect(x, {});
}

template <typename T>
auto
expect(root_ptr<> const& x) {
  return detail::expect_helper<T>::expect(x, {});
}

// Same as expect, but throws a runtime_error with the given message if the
// actual type isn't the expected one.
template <typename T>
auto
expect(ptr<> x, std::string_view message) {
  return detail::expect_helper<T>::expect(x, message);
}

template <typename T>
auto
expect(root_ptr<> const& x, std::string_view message) {
  return detail::expect_helper<T>::expect(x, message);
}

namespace detail {
  template <typename T>
  struct assume_helper {
    static ptr<T>
    assume(ptr<> x) {
      assert(!x || is<T>(x));
      return ptr_cast<T>(x);
    }

    static root_ptr<T>
    assume(root_ptr<> const& x) {
      assert(!x || is<T>(x));
      return {x.list(), static_cast<T*>(x.get())};
    }
  };

  template <typename ImmediateT, auto Converter>
  struct immediate_assume_helper {
    static ImmediateT
    assume(ptr<> x) {
      assert(is<ImmediateT>(x));
      return Converter(x);
    }

    static ImmediateT
    assume(root_ptr<> const& x) {
      assert(is<ImmediateT>(x));
      return Converter(x.get());
    }
  };

  template <>
  struct assume_helper<integer>
    : immediate_assume_helper<integer, ptr_to_integer>
  { };

  template <>
  struct assume_helper<char32_t>
    : immediate_assume_helper<char32_t, ptr_to_character>
  { };

  template <>
  struct assume_helper<string_cursor>
    : immediate_assume_helper<string_cursor, ptr_to_string_cursor>
  { };
}

// Assert that an object is of a given type and return the appropriate typed
// pointer. It is undefined behaviour if the actual type doesn't match the
// specified type.
template <typename T>
auto
assume(ptr<> x) {
  return detail::assume_helper<T>::assume(x);
}

template <typename T>
auto
assume(root_ptr<> const& x) {
  return detail::assume_helper<T>::assume(x);
}

namespace detail {
  template <typename T>
  struct match_helper {
    static ptr<T>
    match(ptr<> x) {
      if (is<T>(x))
        return ptr_cast<T>(x);
      else
        return {};
    }

    static root_ptr<T>
    match(root_ptr<> const& x) {
      return {x.list(), match(x.get())};
    }
  };

  template <typename ImmediateT, auto Converter>
  struct immediate_match_helper {
    static std::optional<ImmediateT>
    match(ptr<> x) {
      if (is<ImmediateT>(x))
        return Converter(x);
      else
        return std::nullopt;
    }
  };

  template <>
  struct match_helper<integer>
    : immediate_match_helper<integer, ptr_to_integer>
  { };

  template <>
  struct match_helper<char32_t>
    : immediate_match_helper<char32_t, ptr_to_character>
  { };

  template <>
  struct match_helper<string_cursor>
    : immediate_match_helper<string_cursor, ptr_to_string_cursor>
  { };
}

// If an object is of the given type, return the typed pointer to it; otherwise,
// return null.
template <typename T>
auto
match(ptr<> x) {
  return detail::match_helper<T>::match(x);
}

template <typename T>
auto
match(root_ptr<> const& x) {
  return detail::match_helper<T>::match(x);
}

} // namespace insider

#endif
