#ifndef INSIDER_UTIL_NAMED_RUNTIME_ERROR_HPP
#define INSIDER_UTIL_NAMED_RUNTIME_ERROR_HPP

#include <stdexcept>

namespace insider {

template <typename>
class named_runtime_error : public std::runtime_error {
public:
  using std::runtime_error::runtime_error;
};

} // namespace insider

#endif
