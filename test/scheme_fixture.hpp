#ifndef INSIDER_TEST_SCHEME_FIXTURE_HPP
#define INSIDER_TEST_SCHEME_FIXTURE_HPP

#include "basic_types.hpp"
#include "compiler.hpp"
#include "context.hpp"
#include "io.hpp"
#include "numeric.hpp"
#include "vm.hpp"

#include <gtest/gtest.h>

struct scheme_fixture : testing::Test {
  insider::context ctx;

  insider::ptr<>
  read(std::string const& expr) {
    return insider::read(ctx, expr);
  }

  insider::ptr<>
  eval(std::string const& expr) {
    insider::module m{ctx};
    import_all_exported(ctx, m, ctx.internal_module);
    auto f = compile_expression(ctx, read_syntax(ctx, expr), m);
    return call_with_continuation_barrier(ctx, f, {}).get();
  }

  insider::ptr<>
  eval_module(std::string const& expr) {
    insider::module m = compile_main_module(ctx, read_syntax_multiple(ctx, expr));
    return execute(ctx, m).get();
  }

  void
  add_library(std::string const& body) {
    ctx.load_library_module(read_syntax_multiple(ctx, body));
  }

  bool
  num_equal(insider::ptr<> lhs, insider::ptr<> rhs) {
    return arith_equal(ctx, lhs, rhs) == ctx.constants->t.get();
  }

  insider::ptr<insider::fraction>
  make_fraction(int n, int d) {
    return insider::make<insider::fraction>(ctx,
                                            integer_to_ptr(insider::integer{n}),
                                            integer_to_ptr(insider::integer{d}));
  }

  insider::ptr<insider::floating_point>
  make_float(double value) {
    return insider::make<insider::floating_point>(ctx, value);
  }
};

#endif
