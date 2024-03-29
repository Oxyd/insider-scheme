#ifndef INSIDER_RUNTIME_ACTION_HPP
#define INSIDER_RUNTIME_ACTION_HPP

#include "context.hpp"
#include "io/write.hpp"

#include <fmt/format.h>

#include <exception>
#include <string_view>
#include <utility>

namespace insider {

// An action of the interpreter, used in error messages.
template <typename Derived>
class action {
public:
  explicit
  action(context& ctx) : ctx_{ctx} { }

  action(action const&) = delete;
  void operator = (action const&) = delete;

  ~action() = default;

protected:
  context& ctx_;

  void
  check() {
    if (std::uncaught_exceptions()) {
      if (!ctx_.action_backtrace.empty())
        ctx_.action_backtrace += '\n';
      ctx_.action_backtrace += static_cast<Derived*>(this)->format();
    }
  }
};

// Action described by a string, possibly accompanied with a datum.
template <typename... Args>
class simple_action : public action<simple_action<Args...>> {
  using base = action<simple_action<Args...>>;

public:
  simple_action(context& ctx, std::string_view format, Args&&... args)
    : base{ctx}
    , format_{format}
    , args_{std::forward<Args>(args)...}
    , irritant_{ctx.store.root_list()}
  { }

  simple_action(context& ctx, root_ptr<> const& irritant,
                std::string_view format, Args... args)
    : base{ctx}
    , format_{format}
    , args_{std::move(args)...}
    , irritant_{irritant}
  { }

  simple_action(context& ctx, ptr<> irritant, std::string_view format,
                Args... args)
    : base{ctx}
    , format_{format}
    , args_{std::move(args)...}
    , irritant_{ctx.store.root_list(), irritant}
  { }

  ~simple_action() { this->check(); }

  std::string
  format() const {
    return format_helper(std::index_sequence_for<Args...>{});
  }

private:
  std::string_view    format_;
  std::tuple<Args...> args_;
  root_ptr<>       irritant_;

  template <std::size_t... Is>
  std::string
  format_helper(std::index_sequence<Is...>) const {
    if (irritant_)
      return fmt::format("{}: {}",
                         fmt::format(fmt::runtime(format_),
                                     std::get<Is>(args_)...),
                         datum_to_string(this->ctx_, irritant_.get()));
    else
      return fmt::format(fmt::runtime(format_), std::get<Is>(args_)...);
  }
};

}

#endif
