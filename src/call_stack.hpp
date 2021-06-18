#ifndef INSIDER_CALL_STACK_HPP
#define INSIDER_CALL_STACK_HPP

#include "integer.hpp"
#include "object.hpp"
#include "object_span.hpp"
#include "ptr.hpp"
#include "page_allocator.hpp"

#include <functional>
#include <memory>
#include <stdexcept>
#include <type_traits>

namespace insider {

class context;
class free_store;
class parameter_map;

using native_continuation_type = std::function<ptr<>(context&, ptr<>)>;

class stack_frame_extra_data : public composite_object<stack_frame_extra_data> {
public:
  static constexpr char const* scheme_name = "insider::stack_frame_extra_data";

  ptr<parameter_map>                    parameters;
  std::vector<native_continuation_type> native_continuations;
  bool                                  allow_jump_out = true;
  bool                                  allow_jump_in  = true;

  void
  visit_members(member_visitor const& f) {
    f(parameters);
  }

  std::size_t
  hash() const { return 0; }
};

class stack_frame : public dynamic_size_object<stack_frame, ptr<>, true> {
public:
  static constexpr char const* scheme_name = "insider::stack_frame";

  integer::value_type         previous_pc = 0;
  ptr<stack_frame>            parent;
  ptr<>                       callable;
  ptr<stack_frame_extra_data> extra;

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
  explicit
  stack_cache(free_store&);

  stack_cache(stack_cache const&) = delete;
  void operator = (stack_cache const&) = delete;

  ptr<stack_frame>
  make(std::size_t num_locals, ptr<> callable, ptr<stack_frame> parent = nullptr,
       integer::value_type previous_pc = 0) {
    std::size_t size = sizeof(word_type) + sizeof(stack_frame) + num_locals * sizeof(ptr<>);

    if (size > page_size - used_size())
      transfer_to_nursery();

    std::byte* storage = top_;
    top_ += size;

    init_object_header(storage, stack_frame::type_index, generation::stack);
    return new (storage + sizeof(word_type)) stack_frame(num_locals, callable, parent, previous_pc);
  }

  void
  deallocate(ptr<stack_frame> frame) {
    if (object_generation(frame) != generation::stack)
      return;

    assert(reinterpret_cast<std::byte*>(frame.value()) + object_size(frame) == top_);
    top_ = reinterpret_cast<std::byte*>(frame.value()) - sizeof(word_type);
  }

  void
  shorten(std::byte* new_end_of_stack) { top_ = new_end_of_stack; }

  void
  clear() { top_ = storage_.get(); }

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

  void
  transfer_to_nursery();

private:
  free_store&          store_;
  page_allocator::page storage_;
  std::byte*           top_;

  std::size_t
  used_size() const { return top_ - storage_.get(); }
};

}

#endif
