#ifndef INSIDER_UTIL_PARAMETERIZE_HPP
#define INSIDER_UTIL_PARAMETERIZE_HPP

#include "ptr.hpp"
#include "runtime/basic_types.hpp"
#include "vm/call_stack.hpp"
#include "vm/vm.hpp"

#include <type_traits>
#include <utility>

namespace insider {

class context;

// Call given C++ procedure with re-entry continuation barrier and with the
// given parameter set to the given value.
template <typename F>
ptr<>
parameterize(vm& state, parameter_assignments const& params, F&& f) {
  struct closure : native_procedure::extra_data {
    F& f;

    explicit
    closure(F& f) : f{f} { }
  };
  auto proc = make<native_procedure>(
    state.ctx,
    [] (vm& state, ptr<native_procedure> np, object_span) -> ptr<> {
      if constexpr (std::is_invocable_v<F>)
        return static_cast<closure*>(np->extra.get())->f();
      else if constexpr (std::is_invocable_v<F, vm&>)
        return static_cast<closure*>(np->extra.get())->f(state);
      else
        static_assert(sizeof(F) == 0,
                      "Callable must take either no parameters, or vm& as its "
                      "only parameter");
    },
    std::make_unique<closure>(f)
  );
  return call_parameterized_with_continuation_barrier(state, params, proc, {});
}

// Like parameterize, but create a new VM and call the callable in it.
template <typename F>
ptr<>
parameterize_root(context& ctx, parameter_assignments const& params, F&& f) {
  vm state{ctx};
  return parameterize(state, params, std::forward<F>(f));
}

} // namespace insider

#endif
