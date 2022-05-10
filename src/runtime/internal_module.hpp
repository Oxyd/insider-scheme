#ifndef INSIDER_RUNTIME_INTERNAL_MODULE_HPP
#define INSIDER_RUNTIME_INTERNAL_MODULE_HPP

#include "ptr.hpp"

namespace insider {

class context;
class module_;

ptr<module_>
make_internal_module(context&);

} // namespace insider

#endif
