#ifndef INSIDER_CALL_STACK_HPP
#define INSIDER_CALL_STACK_HPP

#include "integer.hpp"
#include "object.hpp"
#include "object_span.hpp"
#include "ptr.hpp"

#include <memory>
#include <type_traits>

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

  void
  init(std::size_t i, ptr<> value) { new (&storage_element(i)) ptr<>(value); }

  void
  set_rest_to_null(std::size_t from) {
    for (std::size_t i = from; i < num_locals_; ++i)
      storage_element(i) = nullptr;
  }

  object_span
  span(std::size_t begin, std::size_t size) { return {&storage_element(begin), size}; }

  void
  visit_members(member_visitor const&);

  std::size_t
  size() const { return num_locals_; }

  void
  resize(std::size_t new_num_locals) { num_locals_ = new_num_locals; }

  std::size_t
  hash() const { return 0; }

private:
  std::size_t num_locals_;
};

static_assert(std::is_trivially_destructible_v<stack_frame>);

class stack_cache {
public:
  stack_cache();

  stack_cache(stack_cache const&) = delete;
  void operator = (stack_cache const&) = delete;

  ptr<stack_frame>
  make(std::size_t num_locals, ptr<> callable, ptr<stack_frame> parent = nullptr,
       integer::value_type previous_pc = 0);

  void
  deallocate(ptr<stack_frame>);

  void
  shorten(std::byte* new_end_of_stack) { top_ = new_end_of_stack; }

  bool
  empty() const { return top_ == storage_.get(); }

  template <typename F>
  void
  for_all(F const& f) const {
    std::byte* storage = storage_.get();
    while (storage < top_) {
      ptr<> o{reinterpret_cast<object*>(storage + sizeof(word_type))};
      std::size_t size = object_size(o);

      f(o);

      storage += size + sizeof(word_type);
    }
  }

private:
  std::unique_ptr<std::byte[]> storage_;
  std::byte* top_;
};

}

#endif
