#ifndef INSIDER_TEST_SCHEME_FIXTURE_HPP
#define INSIDER_TEST_SCHEME_FIXTURE_HPP

#include "compiler/ast_transforms.hpp"
#include "compiler/compilation_config.hpp"
#include "compiler/compiler.hpp"
#include "compiler/source_code_provider.hpp"
#include "context.hpp"
#include "io/read.hpp"
#include "runtime/basic_types.hpp"
#include "runtime/numeric.hpp"
#include "runtime/syntax.hpp"
#include "vm/vm.hpp"

#include <gtest/gtest.h>

#include <memory>
#include <utility>

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
  eval(insider::ptr<insider::syntax> expr_stx,
       insider::pass_list passes = insider::all_passes) {
    auto m = insider::make_root<insider::module_>(ctx, ctx);
    import_all_exported(ctx, m, ctx.internal_module_tracked());

    insider::null_source_code_provider provider;
    insider::compilation_config config{
      std::move(passes),
      insider::null_diagnostic_sink::instance
    };
    auto f = compile_expression(ctx,
                                insider::assume<insider::syntax>(expr_stx),
                                m,
                                {&provider, "<unit test expression>"},
                                config);
    return call_root(ctx, f, {});
  }

  insider::ptr<>
  eval(std::string const& expr,
       insider::pass_list passes = insider::all_passes) {
    auto expr_stx = read_syntax(ctx, expr);
    if (expr_stx == ctx.constants->eof)
      throw std::runtime_error{"EOF"};
    return eval(insider::assume<insider::syntax>(expr_stx), std::move(passes));
  }

  insider::ptr<>
  eval_module(std::string const& expr,
              insider::pass_list passes = insider::all_passes) {
    insider::null_source_code_provider provider;
    insider::compilation_config config{
      std::move(passes),
      insider::null_diagnostic_sink::instance
    };
    insider::root_ptr<insider::module_> m = compile_module(
      ctx, read_syntax_multiple(ctx, expr),
      {&provider, "<unit test main module>"},
      config
    );
    return execute(ctx, m);
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
