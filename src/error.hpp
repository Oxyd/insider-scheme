#ifndef INSIDER_ERROR_HPP
#define INSIDER_ERROR_HPP

#include "object.hpp"
#include "ptr.hpp"

#include <exception>
#include <stdexcept>

namespace insider {

class context;

template <typename... Args>
std::runtime_error
make_error(std::string_view fmt, Args&&... args) {
  return std::runtime_error{fmt::format(fmt, std::forward<Args>(args)...)};
}

template <typename Expected>
std::runtime_error
make_type_error(ptr<> actual) {
  throw make_error("Invalid type: expected {}, got {}", type_name<Expected>(), object_type_name(actual));
}

// C++ exception type wrapping a Scheme exception.
class scheme_exception : public std::runtime_error {
public:
  tracked_ptr<> object;

  explicit
  scheme_exception(context&, ptr<> o);
};

// Scheme exception type wrapping a C++ exception.
class cxx_exception : public leaf_object<cxx_exception> {
public:
  static constexpr char const* scheme_name = "insider::cxx_exception";

  explicit
  cxx_exception(std::exception_ptr e)
    : exception_{std::move(e)}
  { }

  [[noreturn]] void
  rethrow() const { std::rethrow_exception(exception_); }

private:
  std::exception_ptr exception_;
};

} // namespace insider

#endif
