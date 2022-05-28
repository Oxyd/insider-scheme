#include "error.hpp"

#include "context.hpp"
#include "io/write.hpp"
#include "util/define_procedure.hpp"

#include <fmt/format.h>

namespace insider {

scheme_exception::scheme_exception(context& ctx, ptr<> o)
  : std::runtime_error{fmt::format("Scheme error: {}", datum_to_string(ctx, o))}
  , object{track(ctx, o)}
{ }

std::string
cxx_exception::message() const {
  try {
    std::rethrow_exception(exception_);
  } catch (std::exception const& e) {
    return e.what();
  } catch (...) {
    return "";
  }
}

void
export_error(context& ctx, ptr<module_> result) {
  define_procedure<&cxx_exception::message>(ctx, "cxx-exception-message",
                                            result);
}

} // namespace insider
