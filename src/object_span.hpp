#ifndef INSIDER_OBJECT_SPAN_HPP
#define INSIDER_OBJECT_SPAN_HPP

#include <cstddef>
#include <vector>

namespace insider {

class object;

// A view into a sequence of object*'s stored contiguously.
class object_span {
public:
  object_span(object* const* begin, std::size_t size)
    : begin_{begin}
    , size_{size}
  { }

  explicit
  object_span(std::vector<object*> const& v)
    : begin_{&v[0]}
    , size_{v.size()}
  { }

  object*
  operator [] (std::size_t i) const { return begin_[i]; }

  object* const*
  begin() const { return begin_; }

  object* const*
  end() const { return begin_ + size_; }

  object*
  front() const { return begin_[0]; }

  object*
  back() const { return begin_[size_ - 1]; }

  std::size_t
  size() const { return size_; }

  bool
  empty() const { return size_ == 0; }

private:
  object* const* begin_;
  std::size_t    size_;
};

} // namespace insider

#endif
