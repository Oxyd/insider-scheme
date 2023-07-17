#ifndef INSIDER_UTIL_DEFINE_PROCEDURE_HPP
#define INSIDER_UTIL_DEFINE_PROCEDURE_HPP

#include "context.hpp"
#include "runtime/basic_types.hpp"
#include "util/from_scheme.hpp"
#include "util/to_scheme.hpp"
#include "vm/vm.hpp"

#include <format>
#include <type_traits>

namespace insider {

namespace detail {
  constexpr inline struct no_closure_tag { } no_closure;

  template <auto Callable>
  constexpr bool callable_is_pointer_v
    = !std::is_same_v<decltype(Callable), bool>;

  template <auto Callable, typename FunctionType>
  struct make_native_procedure_object;

  template <auto Callable, typename R, typename... Args>
  struct make_native_procedure_object<Callable, R(Args...)> {
    template <std::size_t NumDefaults>
    static void
    check_args_size(std::string const& name, object_span args) {
      constexpr std::size_t min_args = sizeof...(Args) - NumDefaults;
      constexpr std::size_t max_args = sizeof...(Args);

      if (args.size() < min_args)
        throw std::runtime_error{std::format(
          "{}: Wrong number of arguments, expected {}{}, got {}",
          name, min_args != max_args ? "at least " : "",
          min_args, args.size()
        )};

      if (args.size() > max_args)
        throw std::runtime_error{std::format(
          "{}: Wrong number of arguments, expected {}{}, got {}",
          name, min_args != max_args ? "at most " : "",
          max_args, args.size()
        )};
    }

    template <std::size_t N, bool Stop, typename... OuterArgs>
    struct call_with_defaults;

    template <typename... OuterArgs>
    struct call_with_defaults<0, true, OuterArgs...> {
      template <typename... CallArgs>
      struct call {
        template <typename Closure, typename... Defaults, std::size_t... Is>
        static auto
        do_call(vm& state, [[maybe_unused]] object_span args,
                [[maybe_unused]] Closure const& closure,
                std::index_sequence<Is...>, Defaults&... defaults) {
          if constexpr (callable_is_pointer_v<Callable>)
            return Callable(state,
                            from_scheme<CallArgs>(state.ctx, args[Is])...,
                            defaults(state)...);
          else
            return closure(state,
                           from_scheme<CallArgs>(state.ctx, args[Is])...,
                           defaults(state)...);
        }

        template <typename Closure, typename... Defaults>
        static auto
        do_call(vm& state, object_span args, Closure const& closure,
                Defaults&... defaults) {
          return do_call(state, args, closure,
                         std::index_sequence_for<CallArgs...>{},
                         defaults...);
        }
      };
    };

    template <std::size_t N, typename OuterArg, typename... OuterArgs>
    struct call_with_defaults<N, false, OuterArg, OuterArgs...> {
      template <typename... CallArgs>
      struct call {
        template <typename Closure, typename... Defaults>
        static auto
        do_call(vm& state, object_span args, Closure const& closure,
                Defaults&... defaults) {
          return call_with_defaults<
            N - 1, N - 1 == 0, OuterArgs...
          >::template call<CallArgs..., OuterArg>::do_call(
            state, args, closure, defaults...
          );
        }
      };
    };

    template <typename Closure, std::size_t... Is>
    static auto
    call(vm& state, [[maybe_unused]] object_span args,
         [[maybe_unused]] Closure const& closure,
         std::index_sequence<Is...>) {
      assert(args.size() == sizeof...(Is));
      if constexpr (callable_is_pointer_v<Callable>)
        return Callable(state, from_scheme<Args>(state.ctx, args[Is])...);
      else
        return closure(state, from_scheme<Args>(state.ctx, args[Is])...);
    }

    template <typename Closure, std::size_t... Is, typename DefaultFirst,
              typename... DefaultsRest>
    static auto
    call(vm& state, object_span args, Closure const& closure,
         std::index_sequence<Is...>,
         DefaultFirst& first_default, DefaultsRest&... defaults_rest) {
      constexpr std::size_t num_args_for_this_overload = sizeof...(Is);
      if (args.size() == num_args_for_this_overload)
        return call_with_defaults<
          num_args_for_this_overload, num_args_for_this_overload == 0, Args...
        >::template call<>::do_call(
          state, args, closure, first_default, defaults_rest...
        );
      else
        return call(state, args, closure,
                    std::make_index_sequence<sizeof...(Is) + 1>{},
                    defaults_rest...);
    }

    template <typename Closure, std::size_t... Is, std::size_t... Js,
              typename... Defaults>
    static auto
    call(vm& state, object_span args, Closure const& closure,
         std::index_sequence<Is...> is,
         std::index_sequence<Js...>, std::tuple<Defaults...>& defaults) {
      return call(state, args, closure, is, std::get<Js>(defaults)...);
    }

    template <typename Closure, std::size_t... Is, typename... Defaults>
    static auto
    call(vm& state, object_span args, Closure const& closure,
         std::index_sequence<Is...> is,
         std::tuple<Defaults...>& defaults) {
      return call(state, args, closure, is,
                  std::index_sequence_for<Defaults...>{}, defaults);
    }

    template <std::size_t... Is>
    static auto
    make_trivial(context& ctx, std::string name, bool constant_evaluable) {
      return insider::make<native_procedure>(
        ctx,
        [] (vm& state, ptr<native_procedure> f, object_span args) -> ptr<> {
          check_args_size<0>(f->name, args);
          constexpr std::size_t min_args = sizeof...(Args);

          if constexpr (std::is_same_v<R, void>) {
            call(state, args, no_closure,
                 std::make_index_sequence<min_args>{});
            return state.ctx.constants->void_;
          }
          else
            return to_scheme(state.ctx,
                             call(state, args, no_closure,
                                  std::make_index_sequence<min_args>{}));
        },
        constant_evaluable,
        std::move(name)
      );
    }

    template <typename Closure, typename... Defaults>
    class complex_data : public native_procedure::extra_data {
    public:
      Closure closure;
      std::tuple<Defaults...> defaults;

      complex_data(Closure closure, Defaults... defaults)
        : closure{std::move(closure)}
        , defaults{std::move(defaults)...}
      { }
    };

    template <typename Closure, std::size_t... Is, typename... Defaults>
    static auto
    make_complex(context& ctx, std::string name, bool constant_evaluable,
                 Closure const& closure,
                 Defaults... defaults) {
      auto data = std::make_unique<complex_data<Closure, Defaults...>>(
        closure, std::move(defaults)...
      );
      return insider::make<native_procedure>(
        ctx,
        [] (vm& state, ptr<native_procedure> f, object_span args) -> ptr<> {
          check_args_size<sizeof...(Defaults)>(f->name, args);
          constexpr std::size_t min_args = sizeof...(Args) - sizeof...(Defaults);

          auto* data
            = static_cast<complex_data<Closure, Defaults...>*>(f->extra.get());

          if constexpr (std::is_same_v<R, void>) {
            call(state, args, data->closure,
                 std::make_index_sequence<min_args>{}, data->defaults);
            return state.ctx.constants->void_;
          }
          else
            return to_scheme(
              state.ctx,
              call(state, args, data->closure,
                   std::make_index_sequence<min_args>{}, data->defaults)
            );
        },
        constant_evaluable,
        std::move(name),
        std::move(data)
      );
    }

    template <typename Closure, std::size_t... Is, typename... Defaults>
    static auto
    make(context& ctx, std::string name, bool constant_evaluable,
         [[maybe_unused]] Closure const& closure,
         Defaults... defaults) {
      if constexpr (sizeof...(Defaults) == 0
                    && !std::is_same_v<decltype(Callable), bool>)
        return make_trivial(ctx, std::move(name), constant_evaluable);
      else
        return make_complex(ctx, std::move(name), constant_evaluable, closure,
                            std::move(defaults)...);
    }
  };

  template <auto Callable, typename FunctionType>
  struct define_typed_procedure {
    static_assert(sizeof(Callable) == 0,
                  "Argument to define_procedure must be a pointer to function, "
                  "or pointer-to-member function.");
  };

  template <auto Callable, typename R, typename... Args>
  struct define_typed_procedure<Callable, R (*)(vm&, Args...)> {
    template <typename... Defaults>
    static operand
    define(context& ctx, std::string name, bool constant_evaluable,
           ptr<module_> m, Defaults... defaults) {
      auto proc = make_native_procedure_object<Callable, R(Args...)>::make(
        ctx, name, constant_evaluable, no_closure, std::move(defaults)...
      );
      return define_top_level(ctx, std::move(name), m, true, proc);
    }
  };

  template <auto Callable, typename R, typename... Args>
  struct define_typed_procedure<Callable, R (*)(context&, Args...)> {
    template <typename... Defaults>
    static operand
    define(context& ctx, std::string name, bool constant_evaluable,
           ptr<module_> m, Defaults... defaults) {
      return detail::define_typed_procedure<
        [] (vm& state, Args... args) { return Callable(state.ctx, args...); },
        R (*)(vm&, Args...)
      >::define(ctx, std::move(name), constant_evaluable, m,
                std::move(defaults)...);
    }
  };

  template <auto Callable, typename R, typename... Args>
  struct define_typed_procedure<Callable, R (*)(Args...)> {
    template <typename... Defaults>
    static operand
    define(context& ctx, std::string name, bool constant_evaluable,
           ptr<module_> m, Defaults... defaults) {
      return detail::define_typed_procedure<
        [] (vm&, Args... args) { return Callable(args...); },
        R (*)(vm&, Args...)
      >::define(ctx, std::move(name), constant_evaluable, m,
                std::move(defaults)...);
    }
  };

  template <auto Callable, typename C, typename R, typename... Args>
  struct define_typed_procedure<Callable, R (C::*)(context&, Args...)> {
    template <typename... Defaults>
    static operand
    define(context& ctx, std::string name, bool constant_evaluable,
           ptr<module_> m, Defaults... defaults) {
      return detail::define_typed_procedure<
        [] (context& ctx, ptr<C> c, Args... args) {
          return (c->*Callable)(ctx, args...);
        },
        R (*)(context&, ptr<C>, Args...)
      >::define(ctx, std::move(name), constant_evaluable, m,
                std::move(defaults)...);
    }
  };

  template <auto Callable, typename C, typename R, typename... Args>
  struct define_typed_procedure<Callable, R (C::*)(Args...)> {
    template <typename... Defaults>
    static operand
    define(context& ctx, std::string name, bool constant_evaluable,
           ptr<module_> m, Defaults... defaults) {
      return detail::define_typed_procedure<
        [] (ptr<C> c, Args... args) { return (c->*Callable)(args...); },
        R (*)(ptr<C>, Args...)
      >::define(ctx, std::move(name), constant_evaluable, m,
                std::move(defaults)...);
    }
  };

  template <auto Callable, typename C, typename R, typename... Args>
  struct define_typed_procedure<Callable, R (C::*)(context&, Args...) const> {
    template <typename... Defaults>
    static operand
    define(context& ctx, std::string name, bool constant_evaluable,
           ptr<module_> m, Defaults... defaults) {
      return detail::define_typed_procedure<
        [] (context& ctx, ptr<C> c, Args... args) {
          return (c->*Callable)(ctx, args...);
        },
        R (*)(context&, ptr<C>, Args...)
      >::define(ctx, std::move(name), constant_evaluable, m,
                std::move(defaults)...);
    }
  };

  template <auto Callable, typename C, typename R, typename... Args>
  struct define_typed_procedure<Callable, R (C::*)(Args...) const> {
    template <typename... Defaults>
    static operand
    define(context& ctx, std::string name, bool constant_evaluable,
           ptr<module_> m,
           Defaults... defaults) {
      return detail::define_typed_procedure<
        [] (ptr<C> c, Args... args) { return (c->*Callable)(args...); },
        R (*)(ptr<C>, Args...)
      >::define(ctx, std::move(name), constant_evaluable, m,
                std::move(defaults)...);
    }
  };

  template <typename Type>
  struct define_typed_closure;

  template <typename R, typename... Args>
  struct define_typed_closure<R(vm&, Args...)> {
    template <typename Closure, typename... Defaults>
    static operand
    define(context& ctx, std::string name, bool constant_evaluable,
           ptr<module_> m, Closure const& closure, Defaults... defaults) {
      auto proc = detail::make_native_procedure_object<false, R(Args...)>::make(
        ctx, name, constant_evaluable, closure, std::move(defaults)...
      );
      return define_top_level(ctx, std::move(name), m, true, proc);
    }
  };

  template <typename R, typename... Args>
  struct define_typed_closure<R(context&, Args...)> {
    template <typename Closure, typename... Defaults>
    static operand
    define(context& ctx, std::string name, bool constant_evaluable,
           ptr<module_> m, Closure const& closure, Defaults... defaults) {
      return define_typed_closure<R(vm&, Args...)>::define(
        ctx, std::move(name), constant_evaluable, m,
        [=] (vm& state, Args... args) {
          return closure(state.ctx, args...);
        },
        std::move(defaults)...
      );
    }
  };

  template <typename R, typename... Args>
  struct define_typed_closure<R(Args...)> {
    template <typename Closure, typename... Defaults>
    static operand
    define(context& ctx, std::string name, bool constant_evaluable,
           ptr<module_> m, Closure const& closure, Defaults... defaults) {
      return define_typed_closure<R(vm&, Args...)>::define(
        ctx, std::move(name), constant_evaluable, m,
        [=] (vm&, Args... args) {
          return closure(args...);
        },
        std::move(defaults)...
      );
    }
  };
}

// Define a given procedure with the given Scheme name in the given module. The
// procedure is given as the template parameter and has to be a pointer to a
// free or member function.
template <auto Callable, typename... Defaults>
operand
define_procedure(context& ctx, std::string name, ptr<module_> m,
                 Defaults... defaults) {
  return detail::define_typed_procedure<
    Callable, std::decay_t<decltype(Callable)>
  >::define(ctx, std::move(name), false, m, std::move(defaults)...);
}

template <auto Callable, typename... Defaults>
operand
define_constant_evaluable_procedure(context& ctx, std::string name,
                                    ptr<module_> m, Defaults... defaults) {
  return detail::define_typed_procedure<
    Callable, std::decay_t<decltype(Callable)>
  >::define(ctx, std::move(name), true, m, std::move(defaults)...);
}

// Like define_procedure, but instead of requiring the callable to be a pointer
// to function passed through a template parameter, it can be any callable, such
// as a capturing lambda expression. This uses a less efficient implementation
// using std::function. The intended signature of the callable has to be passed
// explicitly.
template <typename Type, typename Closure, typename... Defaults>
operand
define_closure(context& ctx, std::string name, ptr<module_> m,
               Closure const& closure, Defaults... defaults) {
  return detail::define_typed_closure<Type>::define(
    ctx, std::move(name), false, m, closure, std::move(defaults)...
  );
}

namespace detail {
  template <auto (*F) (vm&, ptr<native_procedure>, object_span)>
  ptr<>
  regularise_native_proc(vm& state, ptr<native_procedure> proc,
                         object_span args) {
    return F(state, proc, args);
  }

  template <auto (*F) (vm&, object_span)>
  ptr<>
  regularise_native_proc(vm& state, ptr<native_procedure>, object_span args) {
    return F(state, args);
  }

  template <auto (*F) (context&, ptr<native_procedure>, object_span)>
  ptr<>
  regularise_native_proc(vm& state, ptr<native_procedure> proc,
                         object_span args) {
    return F(state.ctx, proc, args);
  }

  template <auto (*F) (context&, object_span)>
  ptr<>
  regularise_native_proc(vm& state, ptr<native_procedure>, object_span args) {
    return F(state.ctx, args);
  }
} // namespace detail

// Like define_procedure, but the procedure receives its arguments as an
// object_span with no conversion to C++ types.
template <auto F>
inline operand
define_raw_procedure(context& ctx, std::string name, ptr<module_> m) {
  auto proc = make<native_procedure>(ctx, detail::regularise_native_proc<F>,
                                     false, name);
  return define_top_level(ctx, name, m, true, proc);
}

template <auto F>
inline operand
define_constant_evaluable_raw_procedure(context& ctx, std::string name,
                                        ptr<module_> m) {
  auto proc = make<native_procedure>(ctx, detail::regularise_native_proc<F>,
                                     true, name);
  return define_top_level(ctx, name, m, true, proc);
}

} // namespace insider

#endif
