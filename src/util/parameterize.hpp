#ifndef INSIDER_UTIL_PARAMETERIZE_HPP
#define INSIDER_UTIL_PARAMETERIZE_HPP

#include "ptr.hpp"
#include "runtime/basic_types.hpp"
#include "vm/call_stack.hpp"
#include "vm/vm.hpp"

namespace insider {

class context;

// Call given C++ procedure with continuation barrier and with the given
// parameter set to the given value.
template <typename F>
ptr<>
parameterize(context& ctx, parameter_assignments const& params,
             F&& f) {
  struct closure : native_procedure::extra_data {
    F& f;

    explicit
    closure(F& f) : f{f} { }
  };
  auto proc = make<native_procedure>(
    ctx,
    [] (context&, ptr<native_procedure> np, object_span) -> ptr<> {
      return static_cast<closure*>(np->extra.get())->f();
    },
    std::make_unique<closure>(f)
  );
  return call_parameterized_with_continuation_barrier(ctx, params, proc, {});
}

} // namespace insider

#endif
