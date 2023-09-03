#include "runtime/time.hpp"

#include "context.hpp"
#include "module.hpp"
#include "util/define_procedure.hpp"

#include <chrono>

namespace insider {

// Current TAI-UTC offset in 2021. In the future, GCC will hopefully implement
// std::chrono::tai_clock and this entire nonsense will be removed.
static constexpr auto utc_to_tai_offset = std::chrono::seconds{37};

static double
sys_timepoint_to_scheme(auto tp) {
  using namespace std::literals;

  auto now = (tp + utc_to_tai_offset).time_since_epoch();
  using period = clock::period;
  return static_cast<double>(now.count()) * period::num / period::den;
}

double
system_to_scheme(clock::time_point tp) {
  return sys_timepoint_to_scheme(tp);
}

double
filesystem_to_scheme(std::chrono::file_clock::time_point tp) {
  return sys_timepoint_to_scheme(std::chrono::file_clock::to_sys(tp));
}

template <typename Clock>
static typename Clock::time_point
scheme_to_clock(double tp) {
  using period = clock::period;
  auto ticks = static_cast<typename Clock::duration::rep>(
    tp * period::den / period::num
  );
  return typename Clock::time_point{
    typename Clock::duration{ticks} - utc_to_tai_offset
  };
}

clock::time_point
scheme_to_system(double tp) {
  return scheme_to_clock<clock>(tp);
}

std::chrono::file_clock::time_point
scheme_to_filesystem(double tp) {
  return scheme_to_clock<std::chrono::file_clock>(tp);
}

static double
current_second() {
  return system_to_scheme(clock::now());
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
export_time(context& ctx, ptr<module_> result) {
  define_procedure<current_second>(ctx, "current-second", result);
  define_procedure<current_jiffy>(ctx, "current-jiffy", result);
  define_procedure<jiffies_per_second>(ctx, "jiffies-per-second", result);
}

} // namespace insider
