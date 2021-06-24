#ifndef INSIDER_ERROR_HPP
#define INSIDER_ERROR_HPP

#include "ptr.hpp"

#include <stdexcept>

namespace insider {

class context;

class scheme_exception : public std::runtime_error {
public:
  tracked_ptr<> object;

  explicit
  scheme_exception(context&, ptr<> o);
};

} // namespace insider

#endif
