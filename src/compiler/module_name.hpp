#ifndef INSIDER_MODULE_NAME_HPP
#define INSIDER_MODULE_NAME_HPP

#include <string>
#include <vector>

namespace insider {

using module_name = std::vector<std::string>;

std::string
module_name_to_string(module_name const& name);

} // namespace insider

#endif
