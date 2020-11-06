#ifndef INSIDER_CONVERTERS_HPP
#define INSIDER_CONVERTERS_HPP

#include "numeric.hpp"
#include "scheme.hpp"
#include "vm.hpp"

#include <fmt/format.h>

#include <type_traits>

namespace insider {

template <typename T, typename Enable = void>
struct to_scheme_converter;

template <typename T>
object*
to_scheme(context& ctx, T const& t) {
  return to_scheme_converter<T>::convert(ctx, t);
}

template <>
struct to_scheme_converter<object*> {
  static object*
  convert(context&, object* o) { return o; }
};

template <>
struct to_scheme_converter<generic_tracked_ptr> {
  static object*
  convert(context&, generic_tracked_ptr const& p) { return p.get(); }
};

template <typename T>
struct to_scheme_converter<T*> {
  static object*
  convert(context&, T* o) { return o; }
};

template <typename T>
struct to_scheme_converter<tracked_ptr<T>> {
  static object*
  convert(context&, tracked_ptr<T> const& p) { return p.get(); }
};

template <>
struct to_scheme_converter<integer> {
  static object*
  convert(context&, integer i) { return integer_to_ptr(i); }
};

template <typename T>
struct to_scheme_converter<T, std::enable_if_t<std::is_integral_v<T> && !std::is_same_v<T, bool>>> {
  static object*
  convert(context&, T t) { return integer_to_ptr(integer{t}); }
};

template <>
struct to_scheme_converter<bool> {
  static object*
  convert(context& ctx, bool b) { return b ? ctx.constants->t.get() : ctx.constants->f.get(); }
};

template <>
struct to_scheme_converter<std::string> {
  static object*
  convert(context& ctx, std::string const& s) { return make_string(ctx, s); }
};

template <typename T, typename Enable = void>
struct from_scheme_converter;

template <typename T>
auto
from_scheme(context& ctx, object* o) {
  return from_scheme_converter<std::remove_cv_t<std::remove_reference_t<T>>>::convert(ctx, o);
}

template <typename T>
auto
from_scheme(context& ctx, generic_tracked_ptr const& x) {
  return from_scheme<T>(ctx, x.get());
}

template <>
struct from_scheme_converter<object*> {
  static object*
  convert(context&, object* o) { return o; }
};

template <>
struct from_scheme_converter<generic_tracked_ptr> {
  static generic_tracked_ptr
  convert(context& ctx, object* o) { return track(ctx, o); }
};

template <typename T>
struct from_scheme_converter<T*> {
  static T*
  convert(context&, object* o) { return expect<T>(o); }
};

template <typename T>
struct from_scheme_converter<tracked_ptr<T>> {
  static tracked_ptr<T>
  convert(context& ctx, object* o) { return track(ctx, expect<T>(o)); }
};

template <typename T>
struct from_scheme_converter<T, std::enable_if_t<std::is_integral_v<T>>> {
  static T
  convert(context&, object* o) { return expect<integer>(o).value(); }
};

template <>
struct from_scheme_converter<std::string> {
  static std::string
  convert(context&, object* o) { return expect<string>(o)->value(); }
};

template <typename T>
struct from_scheme_converter<std::vector<T>> {
  static std::vector<T>
  convert(context& ctx, object* o) {
    std::vector<T> result;

    if (auto v = match<vector>(o)) {
      result.reserve(v->size());
      for (std::size_t i = 0; i < v->size(); ++i)
        result.emplace_back(from_scheme<T>(ctx, v->ref(i)));
    }
    else if (is_list(o)) {
      object* elem = o;
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
  template <typename T>
  struct lambda_definer;

  template <typename R, typename... Args>
  struct lambda_definer<R(context&, Args...)> {
    template <typename F>
    static void
    define(context& ctx, module& m, std::string const& name, F f, bool export_) {
      define(ctx, m, name, std::move(f), export_, std::index_sequence_for<Args...>{});
    }

    template <typename F, std::size_t... Is>
    static void
    define(context& ctx, module& m, std::string const& name, F f, bool export_,
           std::index_sequence<Is...>) {
      auto proc = make<native_procedure>(
        ctx,
        [name, f = std::move(f)] (context& ctx, std::vector<object*> const& args) {
          if (args.size() != sizeof...(Args))
            throw std::runtime_error{fmt::format(
              "{} called with incorrect number of arguments: {} required; {} given",
              name, sizeof...(Args), args.size()
            )};

          if constexpr (std::is_same_v<R, void>) {
            f(ctx, from_scheme<Args>(ctx, args[Is])...);
            return ctx.constants->void_.get();
          } else
            return to_scheme(ctx, f(ctx, from_scheme<Args>(ctx, args[Is])...));
        },
        name
      );

      define_top_level(ctx, m, name, proc, export_);
    }
  };

  template <typename R, typename... Args>
  struct lambda_definer<R(Args...)> {
    template <typename F>
    static void
    define(context& ctx, module& m, std::string const& name, F f, bool export_) {
      define(ctx, m, name, std::move(f), export_, std::index_sequence_for<Args...>{});
    }

    template <typename F, std::size_t... Is>
    static void
    define(context& ctx, module& m, std::string const& name, F f, bool export_,
           std::index_sequence<Is...>) {
      auto proc = make<native_procedure>(
        ctx,
        [name, f = std::move(f)] (context& ctx, std::vector<object*> const& args) {
          if (args.size() != sizeof...(Args))
            throw std::runtime_error{fmt::format(
              "{} called with incorrect number of arguments: {} required; {} given",
              name, sizeof...(Args), args.size()
            )};

          if constexpr (std::is_same_v<R, void>) {
            f(from_scheme<Args>(ctx, args[Is])...);
            return ctx.constants->void_.get();
          } else
            return to_scheme(ctx, f(from_scheme<Args>(ctx, args[Is])...));
        },
        name
      );

      define_top_level(ctx, m, name, proc, export_);
    }
  };

  template <typename C, typename R, typename... Args>
  struct lambda_definer<R (C::*)(Args...)> {
    static void
    define(context& ctx, module& m, std::string const& name, R (C::* f)(Args...), bool export_) {
      lambda_definer<R(C*, Args...)>::define(ctx, m, name,
                                             [=] (C* c, Args... args) { return (c->*f)(args...); },
                                             export_);
    }
  };

  template <typename C, typename R, typename... Args>
  struct lambda_definer<R (C::*)(Args...) const> {
    static void
    define(context& ctx, module& m, std::string const& name, R (C::* f)(Args...) const, bool export_) {
      lambda_definer<R(C const*, Args...)>::define(ctx, m, name,
                                                   [=] (C const* c, Args... args) { return (c->*f)(args...); },
                                                   export_);
    }
  };

} // namespace detail

// Define a native procedure in the given module with the given name. The C++
// function type has to be given explicitly as the first template argument
// because it cannot, in general, be deduced from the callable (which may be
// callable with different argument types).
template <typename T, typename F>
void
define_lambda(context& ctx, module& m, std::string const& name, bool export_, F f) {
  detail::lambda_definer<T>::define(ctx, m, name, std::move(f), export_);
}

template <auto F>
void
define_lambda(context& ctx, module& m, std::string const& name, bool export_) {
  detail::lambda_definer<std::remove_pointer_t<decltype(F)>>::define(ctx, m, name, F, export_);
}

template <typename F>
void
define_raw_lambda(context& ctx, module& m, std::string const& name, bool export_, F f) {
  define_top_level(ctx, m, name, make<native_procedure>(ctx, std::move(f), name), export_);
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
  scheme_procedure(generic_tracked_ptr const& f) : f_{expect<procedure>(f)} { }

  Ret
  operator () (context& ctx, Args&&... args) {
    std::vector<object*> arguments{{to_scheme(ctx, std::forward<Args>(args))...}};
    return from_scheme<Ret>(ctx, call(ctx, f_.get(), arguments));
  }

private:
  tracked_ptr<procedure> f_;
};

}

#endif
