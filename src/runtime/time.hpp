#ifndef INSIDER_RUNTIME_TIME_HPP
#define INSIDER_RUNTIME_TIME_HPP

#include <chrono>

namespace insider {

using clock = std::chrono::system_clock;

double
system_to_scheme(clock::time_point);

double
filesystem_to_scheme(std::chrono::file_clock::time_point);

clock::time_point
scheme_to_system(double);

std::chrono::file_clock::time_point
scheme_to_filesystem(double);

} // namespace insider

#endif
