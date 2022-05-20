#ifndef INSIDER_TEST_SCHEME_FIXTURE_HPP
#define INSIDER_TEST_SCHEME_FIXTURE_HPP

#include "compiler/compiler.hpp"
#include "compiler/source_code_provider.hpp"
#include "context.hpp"
#include "io/read.hpp"
#include "runtime/basic_types.hpp"
#include "runtime/numeric.hpp"
#include "runtime/syntax.hpp"
#include "vm/vm.hpp"

#include <gtest/gtest.h>

struct scheme_fixture : testing::Test {
  insider::context ctx;
  insider::virtual_filesystem_source_code_provider* vfs_provider = nullptr;

  scheme_fixture() {
    auto provider = std::make_unique<insider::virtual_filesystem_source_code_provider>();
    vfs_provider = provider.get();
    ctx.module_resolver().append_source_code_provider(std::move(provider));
  }

  insider::ptr<>
  read(std::string const& expr) {
    return insider::read(ctx, expr);
  }

  insider::ptr<>
  eval(std::string const& expr) {
    auto m = insider::make_tracked<insider::module_>(ctx, ctx);
    import_all_exported(ctx, m, ctx.internal_module_tracked());

    insider::null_source_code_provider provider;
    insider::ptr<> expr_stx = read_syntax(ctx, expr);
    if (expr_stx == ctx.constants->eof)
      throw std::runtime_error{"EOF"};
    auto f = compile_expression(ctx,
                                insider::assume<insider::syntax>(expr_stx),
                                m,
                                {&provider, "<unit test expression>"});
    return call_with_continuation_barrier(ctx, f, {}).get();
  }

  insider::ptr<>
  eval_module(std::string const& expr) {
    insider::null_source_code_provider provider;
    insider::tracked_ptr<insider::module_> m = compile_module(
      ctx, read_syntax_multiple(ctx, expr),
      {&provider, "<unit test main module>"}
    );
    return execute(ctx, m).get();
  }

  void
  add_source_file(std::filesystem::path const& name, std::string body) {
    vfs_provider->add(name, std::move(body));
  }

  bool
  num_equal(insider::ptr<> lhs, insider::ptr<> rhs) {
    return arith_equal(ctx, lhs, rhs) == ctx.constants->t;
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

  testing::AssertionResult
  equal(insider::ptr<> x, insider::ptr<> y);
};

#endif
