#ifndef SCHEME_ERROR_HPP
#define SCHEME_ERROR_HPP

#include "io.hpp"
#include "scheme.hpp"

namespace insider {

// Format an error message using fmtlib and append the action stack to it.
template <typename Error, typename... Args>
Error
make_error(context& ctx, std::string format, Args&&... args) {
  std::string result = fmt::format(format, std::forward<Args>(args)...);

  for (auto a = ctx.actions.rbegin(); a != ctx.actions.rend(); ++a) {
    result += "\n... " + a->message;

    if (a->irritant)
      result += ": " + datum_to_string(ctx, a->irritant);
  }

  return Error(result);
}

} // namespace insider

#endif
