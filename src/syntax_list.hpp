#ifndef INSIDER_SYNTAX_LIST_HPP
#define INSIDER_SYNTAX_LIST_HPP

#include "basic_types.hpp"
#include "syntax.hpp"

namespace insider {

inline ptr<syntax>
syntax_car(ptr<> stx) {
  return expect<syntax>(car(semisyntax_expect<pair>(stx)));
}

inline ptr<>
syntax_cdr(ptr<> stx) {
  return cdr(semisyntax_expect<pair>(stx));
}

inline ptr<syntax>
syntax_cadr(ptr<> stx) {
  return expect<syntax>(car(semisyntax_expect<pair>(syntax_cdr(stx))));
}

inline ptr<>
syntax_cddr(ptr<> stx) {
  return syntax_cdr(syntax_cdr(stx));
}

// Iterator over syntax lists.
class syntax_list_iterator {
public:
  using difference_type = std::ptrdiff_t;
  using value_type = ptr<syntax>;
  using pointer = value_type;
  using reference = value_type;
  using iterator_category = std::forward_iterator_tag;

  syntax_list_iterator() = default;

  syntax_list_iterator(ptr<> x, source_location loc)
    : location_{std::move(loc)}
  {
    if (semisyntax_is<pair>(x))
      current_ = semisyntax_assume<pair>(x);
    else if (!is<null_type>(x))
      throw make_syntax_error(location_, "Expected list");
  }

  reference
  operator * () const { return syntax_car(current_); }

  pointer
  operator -> () const { return syntax_car(current_); }

  ptr<>
  base() const { return current_; }

  syntax_list_iterator&
  operator ++ () {
    ptr<> next = syntax_cdr(current_);
    if (semisyntax_is<pair>(next))
      current_ = semisyntax_assume<pair>(next);
    else if (is<null_type>(next))
      current_ = {};
    else
      throw make_syntax_error(location_, "Expected list");

    return *this;
  }

  syntax_list_iterator
  operator ++ (int) { syntax_list_iterator result{*this}; operator ++ (); return result; }

  bool
  operator == (syntax_list_iterator const& other) const { return current_ == other.current_; }

  bool
  operator != (syntax_list_iterator const& other) { return !operator == (other); }

private:
  ptr<>           current_{};
  source_location location_;
};

// Helper to allow range-based for iteration over a Scheme list.
class in_syntax_list {
public:
  explicit
  in_syntax_list(ptr<syntax> lst) : head_{lst}, location_{lst->location()} { }

  in_syntax_list(ptr<> head, source_location loc) : head_{head}, location_{std::move(loc)} { }

  in_syntax_list(ptr<> head, ptr<syntax> stx) : head_{head}, location_{stx->location()} { }

  syntax_list_iterator
  begin() const { return syntax_list_iterator{head_, location_}; }

  syntax_list_iterator
  end() const { return {}; }

private:
  ptr<> head_;
  source_location location_;
};

} // namespace insider

#endif
