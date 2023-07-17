#ifndef INSIDER_UTIL_DEFINE_STRUCT_HPP
#define INSIDER_UTIL_DEFINE_STRUCT_HPP

#include "context.hpp"
#include "module.hpp"
#include "util/define_procedure.hpp"

#include <format>
#include <string>
#include <type_traits>

namespace insider {

template <typename T>
class struct_definer {
public:
  struct_definer(context& ctx, std::string name, ptr<insider::module_> m)
    : ctx_{ctx}
    , name_{std::move(name)}
    , module_{m}
  {
    define_procedure<&struct_definer::predicate>(
      ctx, make_predicate_name(), m
    );
  }

  template <auto Member>
  requires std::is_member_object_pointer_v<decltype(Member)>
  struct_definer&
  field(std::string const& name) {
    define_procedure<object_getter<Member>>(
      ctx_, make_getter_name(name), module_
    );
    return *this;
  }

  template <auto Member>
  requires std::is_member_object_pointer_v<decltype(Member)>
  struct_definer&
  mutable_field(std::string const& name) {
    define_procedure<object_getter<Member>>(
      ctx_, make_getter_name(name), module_
    );
    define_procedure<object_setter<Member>>(
      ctx_, make_setter_name(name), module_
    );
    return *this;
  }

  template <auto Getter>
  requires std::is_member_function_pointer_v<decltype(Getter)>
  struct_definer&
  field(std::string const& name) {
    define_procedure<function_getter<Getter>>(
      ctx_, make_getter_name(name), module_
    );
    return *this;
  }

  template <auto Getter, auto Setter>
  requires std::is_member_function_pointer_v<decltype(Getter)>
           && std::is_member_function_pointer_v<decltype(Setter)>
  struct_definer&
  mutable_field(std::string const& name) {
    define_procedure<function_getter<Getter>>(
      ctx_, make_getter_name(name), module_
    );
    define_procedure<function_setter<Setter>>(
      ctx_, make_setter_name(name), module_
    );
    return *this;
  }

  template <auto NonMemberGetter>
  struct_definer&
  field(std::string const& name) {
    define_procedure<NonMemberGetter>(ctx_, make_getter_name(name), module_);
    return *this;
  }

private:
  template <typename>
  struct member_type;

  template <typename U>
  struct member_type<U T::*> {
    using type = U;
  };

  template <typename U>
  using member_type_t = typename member_type<U>::type;

  template <typename>
  struct setter_type;

  template <typename U>
  struct setter_type<void (T::*)(U)> {
    using type = U;
  };

  template <typename U>
  using setter_type_t = typename setter_type<U>::type;

  context&              ctx_;
  std::string           name_;
  ptr<insider::module_> module_;

  static bool
  predicate(ptr<> x) {
    return is<T>(x);
  }

  template <auto Member>
  static auto
  object_getter(ptr<T> x) {
    return x.value()->*Member;
  }

  template <auto Member>
  static void
  object_setter(ptr<T> x, member_type_t<decltype(Member)> value) {
    x.value()->*Member = std::move(value);
  }

  template <auto Getter>
  static auto
  function_getter(context& ctx, ptr<T> x) {
    using fun_type = decltype(Getter);
    if constexpr (std::is_invocable_v<fun_type, T*>)
      return std::invoke(Getter, x.value());
    else
      return std::invoke(Getter, x.value(), ctx);
  }

  template <auto Setter>
  static void
  function_setter(ptr<T> x, setter_type_t<decltype(Setter)> value) {
    (x.value()->*Setter)(std::move(value));
  }

  std::string
  make_predicate_name() {
    return std::format("{}?", name_);
  }

  std::string
  make_getter_name(std::string const& field_name) {
    return std::format("{}-{}", name_, field_name);
  }

  std::string
  make_setter_name(std::string const& field_name) {
    return std::format("{}-{}-set!", name_, field_name);
  }
};

template <typename T>
struct_definer<T>
define_struct(context& ctx, std::string name, ptr<insider::module_> m) {
  return struct_definer<T>{ctx, std::move(name), m};
}

} // namespace insider

#endif
