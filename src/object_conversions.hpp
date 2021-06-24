#ifndef INSIDER_OBJECT_CONVERSIONS_HPP
#define INSIDER_OBJECT_CONVERSIONS_HPP

#include "integer.hpp"
#include "object.hpp"

namespace insider {

class error : public std::runtime_error {
public:
  // Format an error message using fmtlib and append the action stack to it.
  template <typename... Args>
  error(std::string_view fmt, Args&&... args)
    : std::runtime_error{fmt::format(fmt, std::forward<Args>(args)...)}
  { }
};

std::string
format_error(context& ctx, std::runtime_error const&);

template <typename Expected>
error
make_type_error(ptr<> actual) {
  throw error{"Invalid type: expected {}, got {}", type_name<Expected>(), object_type_name(actual)};
}

namespace detail {
  template <typename T>
  struct expect_helper {
    static ptr<T>
    expect(ptr<> x, std::string_view message) {
      if (is<T>(x))
        return ptr_cast<T>(x);
      else
        throw !message.empty() ? error{message} : make_type_error<T>(x);
    }

    static tracked_ptr<T>
    expect(tracked_ptr<> const& x, std::string_view message) {
      return {x.store(), expect(x.get(), message)};
    }
  };

  template <>
  struct expect_helper<integer> {
    static integer
    expect(ptr<> x, std::string_view message) {
      if (is<integer>(x))
        return ptr_to_integer(x);
      else
        throw !message.empty() ? error{message} : make_type_error<integer>(x);
    }

    static integer
    expect(tracked_ptr<> const& x, std::string_view message) {
      return expect(x.get(), message);
    }
  };
}

// Expect an object to be of given type and return the apropriate typed pointer
// to the object. Throws type_error if the object isn't of the required type.
template <typename T>
auto
expect(ptr<> x) {
  return detail::expect_helper<T>::expect(x, {});
}

template <typename T>
auto
expect(tracked_ptr<> const& x) {
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
expect(tracked_ptr<> const& x, std::string_view message) {
  return detail::expect_helper<T>::expect(x, message);
}

namespace detail {
  template <typename T>
  struct assume_helper {
    static ptr<T>
    assume(ptr<> x) {
      assert(is<T>(x));
      return ptr_cast<T>(x);
    }

    static tracked_ptr<T>
    assume(tracked_ptr<> const& x) {
      assert(is<T>(x));
      return {x.store(), static_cast<T*>(x.get())};
    }
  };

  template <>
  struct assume_helper<integer> {
    static integer
    assume(ptr<> x) {
      assert(is<integer>(x));
      return ptr_to_integer(x);
    }

    static integer
    assume(tracked_ptr<> const& x) {
      assert(is<integer>(x));
      return ptr_to_integer(x.get());
    }
  };
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
assume(tracked_ptr<> const& x) {
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

    static tracked_ptr<T>
    match(tracked_ptr<> const& x) {
      return {x.store(), match(x.get())};
    }
  };

  template <>
  struct match_helper<integer> {
    static std::optional<integer>
    match(ptr<> x) {
      if (is<integer>(x))
        return ptr_to_integer(x);
      else
        return std::nullopt;
    }

    static std::optional<integer>
    match(tracked_ptr<> const& x) {
      return match(x.get());
    }
  };
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
match(tracked_ptr<> const& x) {
  return detail::match_helper<T>::match(x);
}

} // namespace insider

#endif
