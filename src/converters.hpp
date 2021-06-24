#ifndef INSIDER_CONVERTERS_HPP
#define INSIDER_CONVERTERS_HPP

#include "basic_types.hpp"
#include "context.hpp"
#include "integer.hpp"
#include "vm.hpp"

#include <fmt/format.h>

#include <type_traits>

namespace insider {

template <typename T, typename Enable = void>
struct to_scheme_converter;

template <typename T>
ptr<>
to_scheme(context& ctx, T const& t) {
  return to_scheme_converter<T>::convert(ctx, t);
}

template <>
struct to_scheme_converter<ptr<>> {
  static ptr<>
  convert(context&, ptr<> o) { return o; }
};

template <>
struct to_scheme_converter<tracked_ptr<>> {
  static ptr<>
  convert(context&, tracked_ptr<> const& p) { return p.get(); }
};

template <typename T>
struct to_scheme_converter<ptr<T>> {
  static ptr<>
  convert(context&, ptr<T> o) { return o; }
};

template <typename T>
struct to_scheme_converter<tracked_ptr<T>> {
  static ptr<>
  convert(context&, tracked_ptr<T> const& p) { return p.get(); }
};

template <>
struct to_scheme_converter<integer> {
  static ptr<>
  convert(context&, integer i) { return integer_to_ptr(i); }
};

template <typename T>
struct to_scheme_converter<T, std::enable_if_t<std::is_integral_v<T> && !std::is_same_v<T, bool>>> {
  static ptr<>
  convert(context&, T t) { return integer_to_ptr(integer{t}); }
};

template <>
struct to_scheme_converter<double> {
  static ptr<>
  convert(context& ctx, double value) { return make<floating_point>(ctx, value); }
};

template <>
struct to_scheme_converter<bool> {
  static ptr<>
  convert(context& ctx, bool b) { return b ? ctx.constants->t.get() : ctx.constants->f.get(); }
};

template <>
struct to_scheme_converter<std::string> {
  static ptr<>
  convert(context& ctx, std::string const& s) { return make_string(ctx, s); }
};

template <typename T, typename Enable = void>
struct from_scheme_converter;

template <typename T>
auto
from_scheme(context& ctx, ptr<> o) {
  return from_scheme_converter<std::remove_cv_t<std::remove_reference_t<T>>>::convert(ctx, o);
}

template <typename T>
auto
from_scheme(context& ctx, tracked_ptr<> const& x) {
  return from_scheme<T>(ctx, x.get());
}

template <>
struct from_scheme_converter<ptr<>> {
  static ptr<>
  convert(context&, ptr<> o) { return o; }
};

template <>
struct from_scheme_converter<tracked_ptr<>> {
  static tracked_ptr<>
  convert(context& ctx, ptr<> o) { return track(ctx, o); }
};

template <typename T>
struct from_scheme_converter<ptr<T>> {
  static ptr<T>
  convert(context&, ptr<> o) { return expect<T>(o); }
};

template <typename T>
struct from_scheme_converter<tracked_ptr<T>> {
  static tracked_ptr<T>
  convert(context& ctx, ptr<> o) { return track(ctx, expect<T>(o)); }
};

template <>
struct from_scheme_converter<bool> {
  static bool
  convert(context& ctx, ptr<> o) {
    ptr<boolean> b = expect<boolean>(o);
    return b == ctx.constants->t.get();
  }
};

template <typename T>
struct from_scheme_converter<T, std::enable_if_t<std::is_integral_v<T>>> {
  static T
  convert(context&, ptr<> o) { return expect<integer>(o).value(); }
};

template <>
struct from_scheme_converter<std::string> {
  static std::string
  convert(context&, ptr<> o) { return expect<string>(o)->value(); }
};

template <typename T>
struct from_scheme_converter<std::vector<T>> {
  static std::vector<T>
  convert(context& ctx, ptr<> o) {
    std::vector<T> result;

    if (auto v = match<vector>(o)) {
      result.reserve(v->size());
      for (std::size_t i = 0; i < v->size(); ++i)
        result.emplace_back(from_scheme<T>(ctx, v->ref(i)));
    }
    else if (is_list(o)) {
      ptr<> elem = o;
      while (elem != ctx.constants->null.get()) {
        auto p = assume<pair>(elem);
        result.emplace_back(from_scheme<T>(ctx, car(p)));
        elem = cdr(p);
      }
    }
    else
      throw std::runtime_error{"Expected vector or list"};

    return result;
  }
};

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

// Like define_procedure, but the procedure receives its arguments as a
// vector<ptr<>> with no conversion to C++ types.
template <typename F>
operand
define_raw_procedure(context& ctx, char const* name, module& m, bool export_, F const& f) {
  auto proc = make<native_procedure>(ctx, f, name);
  return define_top_level(ctx, std::string(name), m, export_, proc);
}

// Wrapper around a Scheme procedure. Acts as a C++ function of type T. When
// called, converts its arguments from C++ types to Scheem values, then converts
// the Scheme value back to a C++ one.
template <typename T>
class scheme_procedure;

template <typename Ret, typename... Args>
class scheme_procedure<Ret(Args...)> {
public:
  explicit
  scheme_procedure(tracked_ptr<procedure> const& f) : f_{f} { }

  explicit
  scheme_procedure(tracked_ptr<> const& f) : f_{expect<procedure>(f)} { }

  Ret
  operator () (context& ctx, Args&&... args) {
    std::vector<ptr<>> arguments{{to_scheme(ctx, std::forward<Args>(args))...}};
    return from_scheme<Ret>(ctx, call(ctx, f_.get(), arguments));
  }

private:
  tracked_ptr<procedure> f_;
};

}

#endif
