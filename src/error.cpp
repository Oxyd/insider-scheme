#include "error.hpp"

#include "context.hpp"
#include "io.hpp"

#include <fmt/format.h>

namespace insider {

scheme_exception::scheme_exception(context& ctx, ptr<> o)
  : std::runtime_error{fmt::format("Scheme error: {}", datum_to_string(ctx, o))}
  , object{track(ctx, o)}
{ }

} // namespace insider
