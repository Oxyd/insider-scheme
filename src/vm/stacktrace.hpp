#ifndef INSIDER_VM_STACKTRACE_HPP
#define INSIDER_VM_STACKTRACE_HPP

#include <string>
#include <vector>

namespace insider {

class context;
class execution_state;

struct stacktrace_record {
  enum class kind { scheme, native };

  std::string name;
  kind        kind;
};

using stacktrace = std::vector<stacktrace_record>;

stacktrace
make_stacktrace(execution_state& state);

stacktrace
make_stacktrace(context&);

std::string
format_stacktrace(stacktrace const&);

} // namespace insider

#endif
