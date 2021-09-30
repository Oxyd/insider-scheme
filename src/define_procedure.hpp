#ifndef INSIDER_DEFINE_PROCEDURE_HPP
#define INSIDER_DEFINE_PROCEDURE_HPP

#include "basic_types.hpp"
#include "context.hpp"
#include "from_scheme.hpp"
#include "integer.hpp"
#include "string.hpp"
#include "to_scheme.hpp"
#include "vm.hpp"

#include <fmt/format.h>

#include <type_traits>

namespace insider {

namespace detail {
  template <typename FunctionType>
  struct make_native_procedure_object;

  template <typename R, typename... Args>
  struct make_native_procedure_object<R(Args...)> {
    template <typename Callable, std::size_t... Is>
    static auto
    make(context& ctx, char const* name, Callable const& f, std::index_sequence<Is...>) {
      return insider::make<native_procedure>(
        ctx,
        [=] (context& ctx, object_span args) {
          if (args.size() != sizeof...(Args))
            throw std::runtime_error{fmt::format(
              "{} called with incorrect number of arguments: {} required; {} given",
              name, sizeof...(Args), args.size()
            )};

          if constexpr (std::is_same_v<R, void>) {
            f(ctx, from_scheme<Args>(ctx, args[Is])...);
            return ctx.constants->void_.get();
          }
          else
            return to_scheme(ctx, f(ctx, from_scheme<Args>(ctx, args[Is])...));
        },
        name
      );
    }
  };

  template <typename FunctionType>
  struct define_typed_procedure;

  template <typename R, typename... Args>
  struct define_typed_procedure<R(context&, Args...)> {
    template <typename Callable>
    static operand
    define(context& ctx, char const* name, module& m, bool export_, Callable const& f) {
      auto proc = make_native_procedure_object<R(Args...)>::make(
        ctx, name, f, std::index_sequence_for<Args...>{}
      );
      return define_top_level(ctx, std::string(name), m, export_, proc);
    }
  };

  template <typename R, typename... Args>
  struct define_typed_procedure<R(Args...)> {
    template <typename Callable>
    static operand
    define(context& ctx, char const* name, module& m, bool export_, Callable const& f) {
      return detail::define_typed_procedure<R(context&, Args...)>::define(
        ctx, name, m, export_,
        [=] (context&, Args... args) { return f(args...); }
      );
    }
  };
}

// Define a given procedure with the given Scheme name in the given module. The
// procedure is given as the template parameter and has to be a pointer to a
// free or member function.
template <typename R, typename... Args>
operand
define_procedure(context& ctx, char const* name, module& m, bool export_,
                 R (*f)(Args...)) {
  return detail::define_typed_procedure<R(Args...)>::define(ctx, name, m, export_, f);
}

template <typename R, typename C, typename... Args>
operand
define_procedure(context& ctx, char const* name, module& m, bool export_,
                 R (C::* f)(context&, Args...)) {
  return detail::define_typed_procedure<R(context&, C*, Args...)>::define(ctx, name, m, export_,
                                                                          [=] (context& ctx, C* c, Args... args) {
                                                                            return (c->*f)(ctx, args...);
                                                                          });
}

template <typename R, typename C, typename... Args>
operand
define_procedure(context& ctx, char const* name, module& m, bool export_,
                 R (C::* f)(context&, Args...) const) {
  return detail::define_typed_procedure<R(context&, ptr<C>, Args...)>::define(ctx, name, m, export_,
                                                                              [=] (context& ctx, ptr<C> c, Args... args) {
                                                                                return (c.value()->*f)(ctx, args...);
                                                                              });
}

template <typename R, typename C, typename... Args>
operand
define_procedure(context& ctx, char const* name, module& m, bool export_,
                 R (C::* f)(Args...)) {
  return detail::define_typed_procedure<R(context&, ptr<C>, Args...)>::define(ctx, name, m, export_,
                                                                              [=] (context&, ptr<C> c, Args... args) {
                                                                                return (c.value()->*f)(args...);
                                                                              });
}

template <typename R, typename C, typename... Args>
operand
define_procedure(context& ctx, char const* name, module& m, bool export_,
                 R (C::* f)(Args...) const) {
  return detail::define_typed_procedure<R(context&, ptr<C>, Args...)>::define(ctx, name, m, export_,
                                                                              [=] (context&, ptr<C> c, Args... args) {
                                                                                return (c.value()->*f)(args...);
                                                                              });
}

template <typename Callable>
operand
define_procedure(context& ctx, char const* name, module& m, bool export_, Callable const& f) {
  return define_procedure(ctx, name, m, export_, +f);
}

template <typename FunctionType, typename Callable>
operand
define_procedure(context& ctx, char const* name, module& m, bool export_, Callable const& f) {
  return detail::define_typed_procedure<FunctionType>::define(ctx, name, m, export_, f);
}

// Like define_procedure, but the procedure receives its arguments as an
// object_span with no conversion to C++ types.
template <typename F>
operand
define_raw_procedure(context& ctx, char const* name, module& m, bool export_, F const& f) {
  auto proc = make<native_procedure>(ctx, f, name);
  return define_top_level(ctx, std::string(name), m, export_, proc);
}

} // namespace insider

#endif
