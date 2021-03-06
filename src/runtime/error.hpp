#ifndef INSIDER_RUNTIME_ERROR_HPP
#define INSIDER_RUNTIME_ERROR_HPP

#include "memory/tracked_ptr.hpp"
#include "object.hpp"
#include "ptr.hpp"
#include "util/named_runtime_error.hpp"

#include <exception>
#include <stdexcept>

namespace insider {

class context;
class module_;
class string;

template <typename Error = std::runtime_error, typename... Args>
Error
make_error(std::string_view fmt, Args&&... args) {
  return Error{fmt::format(fmt::runtime(fmt), std::forward<Args>(args)...)};
}

using type_error = named_runtime_error<class type_error_tag>;

template <typename Expected>
auto
make_type_error(ptr<> actual) {
  return make_error<type_error>("Invalid type: expected {}, got {}",
                                type_name<Expected>(),
                                object_type_name(actual));
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

  std::string
  message() const;

private:
  std::exception_ptr exception_;
};

// C++ exception that can be translated into a Scheme exception.
class translatable_runtime_error : public std::runtime_error {
public:
  using std::runtime_error::runtime_error;

  virtual ptr<>
  translate(context&) const = 0;
};

} // namespace insider

#endif
