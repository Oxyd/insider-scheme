#ifndef INSIDER_ERROR_HPP
#define INSIDER_ERROR_HPP

#include "object.hpp"
#include "ptr.hpp"

#include <exception>
#include <stdexcept>

namespace insider {

class context;

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
