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
    template <std::size_t NumDefaults>
    static void
    check_args_size(char const* name, object_span args) {
      constexpr std::size_t min_args = sizeof...(Args) - NumDefaults;
      constexpr std::size_t max_args = sizeof...(Args);

      if (args.size() < min_args)
        throw std::runtime_error{fmt::format(
          "{}: Wrong number of arguments, expected {}{}, got {}",
          name, min_args != max_args ? "at least" : "",
          min_args, args.size()
        )};

      if (args.size() > max_args)
        throw std::runtime_error{fmt::format(
          "{}: Wrong number of arguments, expected at most {}, got {}",
          name, max_args, args.size()
        )};
    }

    template <std::size_t N, bool Stop, typename... OuterArgs>
    struct call_with_defaults;

    template <typename... OuterArgs>
    struct call_with_defaults<0, true, OuterArgs...> {
      template <typename... CallArgs>
      struct call {
        template <typename Callable, typename... Defaults, std::size_t... Is>
        static auto
        do_call(context& ctx, Callable const& f, [[maybe_unused]] object_span args, std::index_sequence<Is...>,
                Defaults&... defaults) {
          return f(ctx, from_scheme<CallArgs>(ctx, args[Is])..., defaults(ctx)...);
        }

        template <typename Callable, typename... Defaults>
        static auto
        do_call(context& ctx, Callable const& f, object_span args, Defaults&... defaults) {
          return do_call(ctx, f, args, std::index_sequence_for<CallArgs...>{}, defaults...);
        }
      };
    };

    template <std::size_t N, typename OuterArg, typename... OuterArgs>
    struct call_with_defaults<N, false, OuterArg, OuterArgs...> {
      template <typename... CallArgs>
      struct call {
        template <typename Callable, typename... Defaults>
        static auto
        do_call(context& ctx, Callable const& f, object_span args, Defaults&... defaults) {
          return call_with_defaults<N - 1, N - 1 == 0, OuterArgs...>::template call<CallArgs..., OuterArg>::do_call(ctx, f, args, defaults...);
        }
      };
    };

    template <typename Callable, std::size_t... Is>
    static auto
    call(context& ctx, Callable const& f, [[maybe_unused]] object_span args, std::index_sequence<Is...>) {
      assert(args.size() == sizeof...(Is));
      return f(ctx, from_scheme<Args>(ctx, args[Is])...);
    }

    template <typename Callable, std::size_t... Is, typename DefaultFirst, typename... DefaultsRest>
    static auto
    call(context& ctx, Callable const& f, object_span args, std::index_sequence<Is...>,
         DefaultFirst& first_default, DefaultsRest&... defaults_rest) {
      constexpr std::size_t num_args_for_this_overload = sizeof...(Is);
      if (args.size() == num_args_for_this_overload)
        return call_with_defaults<num_args_for_this_overload, num_args_for_this_overload == 0, Args...>::template call<>::do_call(ctx, f, args, first_default, defaults_rest...);
      else
        return call(ctx, f, args, std::make_index_sequence<sizeof...(Is) + 1>{}, defaults_rest...);
    }

    template <typename Callable, std::size_t... Is, typename... Defaults>
    static auto
    make(context& ctx, char const* name, Callable const& f, Defaults... defaults) {
      return insider::make<native_procedure>(
        ctx,
        [=, ...defaults = std::move(defaults)] (context& ctx, object_span args) {
          check_args_size<sizeof...(Defaults)>(name, args);
          constexpr std::size_t min_args = sizeof...(Args) - sizeof...(Defaults);

          if constexpr (std::is_same_v<R, void>) {
            call(ctx, f, args, std::make_index_sequence<min_args>{}, defaults...);
            return ctx.constants->void_.get();
          }
          else
            return to_scheme(ctx, call(ctx, f, args, std::make_index_sequence<min_args>{}, defaults...));
        },
        name
      );
    }
  };

  template <typename FunctionType>
  struct define_typed_procedure;

  template <typename R, typename... Args>
  struct define_typed_procedure<R(context&, Args...)> {
    template <typename Callable, typename... Defaults>
    static operand
    define(context& ctx, char const* name, module_& m, bool export_,
           Callable const& f, Defaults... defaults) {
      auto proc = make_native_procedure_object<R(Args...)>::make(
        ctx, name, f, std::move(defaults)...
      );
      return define_top_level(ctx, std::string(name), m, export_, proc);
    }
  };

  template <typename R, typename... Args>
  struct define_typed_procedure<R(Args...)> {
    template <typename Callable, typename... Defaults>
    static operand
    define(context& ctx, char const* name, module_& m, bool export_,
           Callable const& f, Defaults... defaults) {
      return detail::define_typed_procedure<R(context&, Args...)>::define(
        ctx, name, m, export_,
        [=] (context&, Args... args) { return f(args...); },
        std::move(defaults)...
      );
    }
  };
}

// Define a given procedure with the given Scheme name in the given module. The
// procedure is given as the template parameter and has to be a pointer to a
// free or member function.
template <typename R, typename... Args, typename... Defaults>
operand
define_procedure(context& ctx, char const* name, module_& m, bool export_,
                 R (*f)(Args...),
                 Defaults... defaults) {
  return detail::define_typed_procedure<R(Args...)>::define(ctx, name, m, export_, f, std::move(defaults)...);
}

template <typename R, typename C, typename... Args, typename... Defaults>
operand
define_procedure(context& ctx, char const* name, module_& m, bool export_,
                 R (C::* f)(context&, Args...), Defaults... defaults) {
  return detail::define_typed_procedure<R(context&, ptr<C>, Args...)>::define(
    ctx, name, m, export_,
    [=] (context& ctx, ptr<C> c, Args... args) {
      return (c.value()->*f)(ctx, args...);
    },
    std::move(defaults)...
  );
}

template <typename R, typename C, typename... Args, typename... Defaults>
operand
define_procedure(context& ctx, char const* name, module_& m, bool export_,
                 R (C::* f)(context&, Args...) const, Defaults... defaults) {
  return detail::define_typed_procedure<R(context&, ptr<C>, Args...)>::define(
    ctx, name, m, export_,
    [=] (context& ctx, ptr<C> c, Args... args) {
      return (c.value()->*f)(ctx, args...);
    },
    std::move(defaults)...
  );
}

template <typename R, typename C, typename... Args, typename... Defaults>
operand
define_procedure(context& ctx, char const* name, module_& m, bool export_,
                 R (C::* f)(Args...), Defaults... defaults) {
  return detail::define_typed_procedure<R(context&, ptr<C>, Args...)>::define(
    ctx, name, m, export_,
    [=] (context&, ptr<C> c, Args... args) {
      return (c.value()->*f)(args...);
    },
    std::move(defaults)...
  );
}

template <typename R, typename C, typename... Args, typename... Defaults>
operand
define_procedure(context& ctx, char const* name, module_& m, bool export_,
                 R (C::* f)(Args...) const, Defaults... defaults) {
  return detail::define_typed_procedure<R(context&, ptr<C>, Args...)>::define(
    ctx, name, m, export_,
    [=] (context&, ptr<C> c, Args... args) {
      return (c.value()->*f)(args...);
    },
    std::move(defaults)...
  );
}

template <typename Callable, typename... Defaults>
operand
define_procedure(context& ctx, char const* name, module_& m, bool export_,
                 Callable const& f, Defaults... defaults) {
  return define_procedure(ctx, name, m, export_, +f, std::move(defaults)...);
}

template <typename FunctionType, typename Callable, typename... Defaults>
operand
define_procedure(context& ctx, char const* name, module_& m, bool export_,
                 Callable const& f, Defaults... defaults) {
  return detail::define_typed_procedure<FunctionType>::define(ctx, name, m, export_, f, std::move(defaults)...);
}

// Like define_procedure, but the procedure receives its arguments as an
// object_span with no conversion to C++ types.
template <typename F>
operand
define_raw_procedure(context& ctx, char const* name, module_& m, bool export_, F const& f) {
  auto proc = make<native_procedure>(ctx, f, name);
  return define_top_level(ctx, std::string(name), m, export_, proc);
}

} // namespace insider

#endif
