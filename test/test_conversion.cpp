#include "runtime/integer.hpp"
#include "scheme_fixture.hpp"

#include "util/from_scheme.hpp"
#include "util/symbolic_enum.hpp"
#include <stdexcept>

using namespace insider;

struct conversion : scheme_fixture { };

struct foo_enum_def {
  enum class values {
    x, y, z
  };

  static constexpr std::tuple<char const*, values> mapping[]{
    {"x", values::x},
    {"y", values::y},
    {"z", values::z}
  };
};

using foo_enum = symbolic_enum<foo_enum_def>;

TEST_F(conversion, symbol_to_symbolic_enum) {
  auto x = from_scheme<foo_enum>(ctx, ctx.intern("x"));
  EXPECT_EQ(x.value(), foo_enum_def::values::x);
  
  auto y = from_scheme<foo_enum>(ctx, ctx.intern("y"));
  EXPECT_EQ(y.value(), foo_enum_def::values::y);
  
  auto z = from_scheme<foo_enum>(ctx, ctx.intern("z"));
  EXPECT_EQ(z.value(), foo_enum_def::values::z);

  EXPECT_THROW(from_scheme<foo_enum>(ctx, ctx.intern("w")),
               std::runtime_error);
  EXPECT_THROW(from_scheme<foo_enum>(ctx, integer_to_ptr(2)),
               std::runtime_error);
}

TEST_F(conversion, symbolic_enum_to_symbol) {
  auto y = to_scheme<foo_enum>(ctx, foo_enum{foo_enum_def::values::y});
  EXPECT_EQ(expect<symbol>(y)->value(), "y");
}
