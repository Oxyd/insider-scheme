#ifndef SCHEME_CONVERTERS_HPP
#define SCHEME_CONVERTERS_HPP

#include "scheme.hpp"
#include "vm.hpp"

#include <fmt/format.h>

#include <type_traits>

namespace scm {

template <typename T, typename Enable = void>
struct to_scheme_converter;

template <typename T>
generic_ptr
to_scheme(context& ctx, T const& t) {
  return to_scheme_converter<T>::convert(ctx, t);
}

template <>
struct to_scheme_converter<generic_ptr> {
  static generic_ptr
  convert(context&, generic_ptr const& p) { return p; }
};

template <typename T>
struct to_scheme_converter<ptr<T>> {
  static generic_ptr
  convert(context&, ptr<T> const& p) { return p; }
};

template <typename T>
struct to_scheme_converter<T, std::enable_if_t<std::is_integral_v<T>>> {
  static generic_ptr
  convert(context& ctx, T t) { return make<integer>(ctx, t); }
};

template <>
struct to_scheme_converter<std::string> {
  static generic_ptr
  convert(context& ctx, std::string const& s) { return make_string(ctx, s); }
};

template <typename T, typename Enable = void>
struct from_scheme_converter;

template <typename T>
auto
from_scheme(context& ctx, generic_ptr const& x) {
  return from_scheme_converter<std::remove_cv_t<std::remove_reference_t<T>>>::convert(ctx, x);
}

template <>
struct from_scheme_converter<generic_ptr> {
  static generic_ptr
  convert(context&, generic_ptr const& x) { return x; }
};

template <typename T>
struct from_scheme_converter<ptr<T>> {
  static ptr<T>
  convert(context&, generic_ptr const& x) { return expect<T>(x); }
};

template <typename T>
struct from_scheme_converter<T, std::enable_if_t<std::is_integral_v<T>>> {
  static T
  convert(context&, generic_ptr const& x) { return expect<integer>(x)->value(); }
};

template <>
struct from_scheme_converter<std::string> {
  static std::string
  convert(context&, generic_ptr const& x) { return expect<string>(x)->value(); }
};

template <typename T>
struct from_scheme_converter<std::vector<T>> {
  static std::vector<T>
  convert(context& ctx, generic_ptr const& x) {
    std::vector<T> result;

    if (auto v = match<vector>(x)) {
      result.reserve(v->size());
      for (std::size_t i = 0; i < v->size(); ++i)
        result.emplace_back(from_scheme<T>(vector_ref(v, i)));
    }
    else if (is_list(x)) {
      generic_ptr elem = x;
      while (elem != ctx.constants->null) {
        auto p = assume<pair>(elem);
        result.emplace_back(from_scheme<T>(car(p)));
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
        [name, f = std::move(f)] (context& ctx, std::vector<generic_ptr> const& args) {
          if (args.size() != sizeof...(Args))
            throw std::runtime_error{fmt::format(
              "{} called with incorrect number of arguments: {} required; {} given",
              name, sizeof...(Args), args.size()
            )};

          if constexpr (std::is_same_v<R, void>) {
            f(ctx, from_scheme<Args>(ctx, args[Is])...);
            return ctx.constants->void_;
          } else
            return to_scheme(ctx, f(ctx, from_scheme<Args>(ctx, args[Is])...));
        }
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
        [name, f = std::move(f)] (context& ctx, std::vector<generic_ptr> const& args) {
          if (args.size() != sizeof...(Args))
            throw std::runtime_error{fmt::format(
              "{} called with incorrect number of arguments: {} required; {} given",
              name, sizeof...(Args), args.size()
            )};

          if constexpr (std::is_same_v<R, void>) {
            f(from_scheme<Args>(ctx, args[Is])...);
            return ctx.constants->void_;
          } else
            return to_scheme(ctx, f(from_scheme<Args>(ctx, args[Is])...));
        }
      );

      define_top_level(ctx, m, name, proc, export_);
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

// Wrapper around a Scheme procedure. Acts as a C++ function of type T. When
// called, converts its arguments from C++ types to Scheem values, then converts
// the Scheme value back to a C++ one.
template <typename T>
class scheme_procedure;

template <typename Ret, typename... Args>
class scheme_procedure<Ret(Args...)> {
public:
  explicit
  scheme_procedure(ptr<procedure> const& f) : f_{f} { }

  explicit
  scheme_procedure(generic_ptr const& f) : f_{expect<procedure>(f)} { }

  Ret
  operator () (context& ctx, Args&&... args) {
    std::vector<generic_ptr> arguments{{to_scheme(ctx, std::forward<Args>(args))...}};
    return from_scheme<Ret>(ctx, call(ctx, f_, arguments));
  }

private:
  ptr<procedure> f_;
};

}

#endif
