#ifndef INSIDER_OBJECT_SPAN_HPP
#define INSIDER_OBJECT_SPAN_HPP

#include "ptr.hpp"

#include <cstddef>
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

} // namespace insider

#endif
