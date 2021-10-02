#include "scheme_fixture.hpp"

#include "define_procedure.hpp"
#include "vm.hpp"

using namespace insider;

struct define_procedure_fixture : scheme_fixture { };

static int
times_2(int x) {
  return 2 * x;
}

TEST_F(define_procedure_fixture, simple_procedure) {
  define_procedure(ctx, "times-2", ctx.internal_module, true, times_2);
  auto result = eval("(times-2 4)");
  EXPECT_EQ(expect<integer>(result).value(), 8);
}

TEST_F(define_procedure_fixture, procedure_with_one_default) {
  define_procedure(ctx, "add", ctx.internal_module, true,
                   [] (int x, int y) { return x + y; },
                   [] (context&) { return 0; });
  auto result1 = eval("(add 2 3)");
  EXPECT_EQ(expect<integer>(result1).value(), 5);

  auto result2 = eval("(add 2)");
  EXPECT_EQ(expect<integer>(result2).value(), 2);

  EXPECT_THROW(eval("(add)"), std::runtime_error);
  EXPECT_THROW(eval("(add 2 3 4)"), std::runtime_error);
}

TEST_F(define_procedure_fixture, procedure_with_two_defaults) {
  define_procedure(ctx, "add", ctx.internal_module, true,
                   [] (int x, int y) { return x + y; },
                   [] (context&) { return 0; },
                   [] (context&) { return 0; });
  EXPECT_EQ(expect<integer>(eval("(add)")).value(), 0);
  EXPECT_EQ(expect<integer>(eval("(add 1)")).value(), 1);
  EXPECT_EQ(expect<integer>(eval("(add 1 2)")).value(), 3);
  EXPECT_THROW(eval("(add 1 2 3)"), std::runtime_error);
}

TEST_F(define_procedure_fixture, procedure_with_parameter_as_the_default) {
  auto n = track(ctx, create_parameter_tag(ctx, integer_to_ptr(0)));
  define_top_level(ctx, "n", ctx.internal_module, true, n.get());
  define_procedure(ctx, "add", ctx.internal_module, true,
                   [] (int x, int y) { return x + y; },
                   [=] (context& ctx) {
                     return expect<integer>(find_parameter_value(ctx, n.get())).value();
                   });

  EXPECT_EQ(expect<integer>(eval("(add 1)")).value(), 1);
  EXPECT_EQ(expect<integer>(eval("(add 1 2)")).value(), 3);

  auto result = eval(R"(
    (call-parameterized n 5
      (lambda ()
        (add 1)))
  )");
  EXPECT_EQ(expect<integer>(result).value(), 6);
}
