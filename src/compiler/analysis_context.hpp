#ifndef INSIDER_ANALYSIS_CONTEXT_HPP
#define INSIDER_ANALYSIS_CONTEXT_HPP

namespace insider {

enum class analysis_context {
  closed, // Closed module, definitions may not be added after analysis
  open    // Open module, such as the REPL; definitions may be added later
};

} // namespace insider

#endif
