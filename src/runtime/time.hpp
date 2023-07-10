#ifndef INSIDER_RUNTIME_TIME_HPP
#define INSIDER_RUNTIME_TIME_HPP

#include <chrono>

namespace insider {

using clock = std::chrono::system_clock;

double
system_to_scheme(clock::time_point);

clock::time_point
scheme_to_system(double);

} // namespace insider

#endif