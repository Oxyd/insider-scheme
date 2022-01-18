#ifndef INSIDER_OBJECT_SPAN_HPP
#define INSIDER_OBJECT_SPAN_HPP

#include "object_conversions.hpp"
#include "ptr.hpp"

#include <fmt/format.h>

#include <cstddef>
#include <stdexcept>
#include <vector>

namespace insider {

class object;

// A view into a sequence of ptr<>'s stored contiguously.
class object_span {
public:
  object_span(ptr<> const* begin, std::size_t size)
    : begin_{begin}
    , size_{size}
  { }

  explicit
  object_span(std::vector<ptr<>> const& v)
    : begin_{v.data()}
    , size_{v.size()}
  { }

  ptr<>
  operator [] (std::size_t i) const { return begin_[i]; }

  ptr<> const*
  begin() const { return begin_; }

  ptr<> const*
  end() const { return begin_ + size_; }

  ptr<>
  front() const { return begin_[0]; }

  ptr<>
  back() const { return begin_[size_ - 1]; }

  std::size_t
  size() const { return size_; }

  bool
  empty() const { return size_ == 0; }

private:
  ptr<> const* begin_;
  std::size_t  size_;
};

inline void
require_arg_count(object_span args, std::size_t min) {
  if (args.size() < min)
    throw std::runtime_error{fmt::format("Expected at least {} arguments, got {}",
                                         min, args.size())};
}

inline void
require_arg_count(object_span args, std::size_t min, std::size_t max) {
  if (args.size() < min)
    throw std::runtime_error{fmt::format("Expected at least {} arguments, got {}",
                                         min, args.size())};
  if (args.size() > max)
    throw std::runtime_error{fmt::format("Expected at most {} arguments, got {}",
                                         max, args.size())};
}

template <typename T>
auto
require_arg(object_span args, std::size_t i) {
  if (args.size() <= i)
    throw std::runtime_error{fmt::format("Expected at least {} arguments, got {}",
                                         i + 1, args.size())};
  else
    return expect<T>(args[i]);
}

template <typename T>
auto
optional_arg(object_span args, std::size_t i, value_type_for_t<T> def) {
  if (args.size() <= i)
    return def;
  else
    return expect<T>(args[i]);
}

} // namespace insider

#endif
