#ifndef INSIDER_LIST_ITERATOR_HPP
#define INSIDER_LIST_ITERATOR_HPP

#include "object_conversions.hpp"
#include "ptr.hpp"

#include <stdexcept>

namespace insider {

class null_type;
class pair;

namespace detail {
  template <typename T, typename SimilarTo>
  struct pointer_like;

  template <typename T>
  struct pointer_like<T, ptr<>> {
    using type = ptr<T>;
  };

  template <typename T>
  struct pointer_like<T, tracked_ptr<>> {
    using type = tracked_ptr<T>;
  };

  template <typename T, typename SimilarTo>
  using pointer_like_t = typename pointer_like<T, SimilarTo>::type;
}

// Iterator over Scheme lists. Will throw an exception if the list turns out to
// be improper (dotted list).
template <typename Pointer>
class list_iterator {
public:
  using difference_type = std::ptrdiff_t;
  using value_type = Pointer;
  using pointer = value_type;
  using reference = value_type;
  using iterator_category = std::forward_iterator_tag;

  list_iterator() = default;

  explicit
  list_iterator(Pointer x) {
    if (is<pair>(x))
      current_ = assume<pair>(x);
    else if (!is<null_type>(x))
      throw std::runtime_error{"Expected list"};
  }

  reference
  operator * () const { return car(current_); }

  pointer
  operator -> () const { return car(current_); }

  list_iterator&
  operator ++ () {
    Pointer next = cdr(current_);
    if (is<pair>(next))
      current_ = assume<pair>(next);
    else if (is<null_type>(next))
      current_ = {};
    else
      throw std::runtime_error{"Expected list"};

    return *this;
  }

  list_iterator
  operator ++ (int) { list_iterator result{*this}; operator ++ (); return result; }

  bool
  operator == (list_iterator const& other) const { return current_ == other.current_; }

  bool
  operator != (list_iterator const& other) { return !operator == (other); }

private:
  detail::pointer_like_t<pair, Pointer> current_{};
};

// Helper to allow range-based for iteration over a Scheme list.
template <typename Pointer>
class in_list {
public:
  explicit
  in_list(Pointer lst) : head_{lst} { }

  list_iterator<Pointer>
  begin() const { return list_iterator{head_}; }

  list_iterator<Pointer>
  end() const { return {}; }

private:
  Pointer head_;
};

} // namespace insider

#endif
