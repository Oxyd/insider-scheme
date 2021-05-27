#ifndef INSIDER_CALL_STACK_HPP
#define INSIDER_CALL_STACK_HPP

#include "integer.hpp"
#include "object.hpp"
#include "object_span.hpp"
#include "ptr.hpp"

#include <memory>

namespace insider {

class stack_frame : public dynamic_size_object<stack_frame, ptr<>, true> {
public:
  static constexpr char const* scheme_name = "insider::stack_frame";

  integer::value_type previous_pc = 0;
  ptr<stack_frame>    parent;
  ptr<>               callable;

  static std::size_t
  extra_elements(std::size_t num_locals, ptr<>, ptr<stack_frame> = nullptr, integer::value_type = 0) {
    return num_locals;
  }

  stack_frame(std::size_t num_locals, ptr<> callable, ptr<stack_frame> parent = nullptr,
              integer::value_type previous_pc = 0)
    : previous_pc{previous_pc}
    , parent{parent}
    , callable{callable}
    , num_locals_{num_locals}
  { }

  stack_frame(stack_frame&&);

  ptr<>
  ref(std::size_t i) const { return storage_element(i); }

  void
  set(std::size_t i, ptr<> value) { storage_element(i) = value; }

  object_span
  span(std::size_t begin, std::size_t size) { return {&storage_element(begin), size}; }

  void
  visit_members(member_visitor const&);

  std::size_t
  size() const { return num_locals_; }

  std::size_t
  hash() const { return 0; }

private:
  std::size_t num_locals_;
};

class stack_cache {
public:
  stack_cache();

  stack_cache(stack_cache const&) = delete;
  void operator = (stack_cache const&) = delete;

  std::byte*
  allocate(std::size_t);

  void
  deallocate(std::byte*, std::size_t object_size);

  bool
  empty() const { return top_ == 0; }

  template <typename F>
  void
  for_all(F const& f) const {
    std::size_t i = 0;
    while (i < top_) {
      std::byte* storage = storage_.get() + i;
      ptr<> o{reinterpret_cast<object*>(storage + sizeof(word_type))};
      std::size_t size = object_size(o);

      f(o);

      i += size + sizeof(word_type);
    }
  }

private:
  std::unique_ptr<std::byte[]> storage_;
  std::size_t top_ = 0;
};

}

#endif
