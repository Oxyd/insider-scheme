#include "context.hpp"
#include "define_procedure.hpp"
#include "module.hpp"

#include <chrono>

namespace insider {

static double
current_second() {
  using namespace std::literals;

  // Current TAI-UTC offset in 2021. In the future, GCC will hopefully implement
  // std::chrono::tai_clock and this entire nonsense will be removed.
  constexpr auto offset = 37s;

  auto now = (std::chrono::system_clock::now() + offset).time_since_epoch();
  using period = std::chrono::system_clock::period;
  return static_cast<double>(now.count()) * period::num / period::den;
}

using jiffy_clock = std::chrono::steady_clock;
using jiffy_duration = jiffy_clock::duration;

static jiffy_duration::rep
current_jiffy() {
  return jiffy_clock::now().time_since_epoch().count();
}

static jiffy_duration::rep
jiffies_per_second() {
  return jiffy_clock::period::den;
}

void
export_time(context& ctx, module_& result) {
  define_procedure(ctx, "current-second", result, true, current_second);
  define_procedure(ctx, "current-jiffy", result, true, current_jiffy);
  define_procedure(ctx, "jiffies-per-second", result, true, jiffies_per_second);
}

} // namespace insider
