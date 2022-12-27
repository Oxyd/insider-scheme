#include "scheme_fixture.hpp"

#include "util/sum_type.hpp"

#include "runtime/string.hpp"
#include "util/define_procedure.hpp"

using namespace insider;

struct sum_type_fixture : scheme_fixture { };

using symbol_or_string = sum_type<symbol, string>;

TEST_F(sum_type_fixture, sum_type_holds_last_assigned_type) {
  symbol_or_string value = ctx.intern("foo");
  EXPECT_TRUE(is<symbol>(value));
  EXPECT_FALSE(is<string>(value));

  value = make<string>(ctx, "foo");
  EXPECT_FALSE(is<symbol>(value));
  EXPECT_TRUE(is<string>(value));
}

using integer_or_string = sum_type<integer, string>;

TEST_F(sum_type_fixture, can_test_for_integer) {
  integer_or_string value = integer_to_ptr(12);
  EXPECT_TRUE(is<integer>(value));
  EXPECT_FALSE(is<string>(value));
}

TEST_F(sum_type_fixture, expect_retrieves_value) {
  symbol_or_string value = ctx.intern("foo");
  auto sym = expect<symbol>(value);
  EXPECT_EQ(sym, ctx.intern("foo"));
}

TEST_F(sum_type_fixture, can_retreive_stored_value) {
  ptr<string> s = make<string>(ctx, "foo");
  symbol_or_string value = s;
  EXPECT_EQ(value.get(), s);
}

TEST_F(sum_type_fixture, can_compare_with_ptr) {
  ptr<symbol> s = ctx.intern("foo");
  symbol_or_string value = s;
  EXPECT_TRUE(value == s);
}

TEST_F(sum_type_fixture, can_be_assigned_untyped_ptr) {
  ptr<> s = ctx.intern("foo");
  symbol_or_string value = s;
  EXPECT_EQ(value, s);
}

TEST_F(sum_type_fixture, assignment_from_wrong_type_throws) {
  ptr<> v = make<vector>(ctx, 2, ctx.constants->f);
  symbol_or_string value;
  EXPECT_THROW(value = v, type_error);
}

TEST_F(sum_type_fixture, convertible_to_bool) {
  symbol_or_string value;
  EXPECT_FALSE(value);

  value = ctx.intern("foo");
  EXPECT_TRUE(value);
}

TEST_F(sum_type_fixture, visit_calls_correct_overload) {
  struct visitor {
    bool called = false;

    void
    operator () (ptr<symbol>) {
      called = true;
    }

    void
    operator () (ptr<string>) {
      FAIL();
    }
  } v;

  symbol_or_string value = ctx.intern("foo");
  visit(v, value);
  EXPECT_TRUE(v.called);
}

TEST_F(sum_type_fixture, visit_returns_value) {
  std::string result = visit([] (auto x) { return x->value(); },
                             symbol_or_string{ctx.intern("foo")});
  EXPECT_EQ(result, "foo");
}

struct sum_holder : composite_object<sum_holder> {
  static constexpr char const* scheme_name = "insider::sum_holder";

  symbol_or_string value;

  void
  visit_members(member_visitor const& f) const {
    value.visit_members(f);
  }
};

TEST_F(sum_type_fixture, can_be_member) {
  auto holder = make_tracked<sum_holder>(ctx);
  holder->value = make<string>(ctx, "foo");
  ctx.store.collect_garbage(true);

  EXPECT_EQ(expect<string>(holder->value)->value(), "foo");
}

static std::string
symbol_or_string_value(symbol_or_string x) {
  return visit([] (auto s) { return s->value(); }, x);
}

TEST_F(sum_type_fixture,
       function_taking_sum_type_can_be_used_with_define_procedure) {
  define_procedure<symbol_or_string_value>(ctx, "symbol-or-string-value",
                                           ctx.internal_module());
  auto result = eval("(symbol-or-string-value 'foo)");
  EXPECT_EQ(expect<string>(result)->value(), "foo");
}

TEST_F(sum_type_fixture, tracked_sum_type_stores_sum_type) {
  auto s = make<string>(ctx, "foo");
  tracked_sum_type<symbol_or_string> value{ctx.store, s};
  EXPECT_EQ(value.get(), s);
}

TEST_F(sum_type_fixture, tracked_sum_type_provides_gc_root) {
  tracked_sum_type<symbol_or_string> value{ctx.store, make<string>(ctx, "foo")};
  ctx.store.collect_garbage(true);
  EXPECT_EQ(expect<string>(value.get())->value(), "foo");
}

TEST_F(sum_type_fixture, can_match_correct_type) {
  symbol_or_string value = ctx.intern("foo");
  EXPECT_FALSE(match<string>(value));
  EXPECT_TRUE(match<symbol>(value));
}

TEST_F(sum_type_fixture, match_on_empty_sum_doesnt_match_any_type) {
  symbol_or_string empty;
  EXPECT_FALSE(match<string>(empty));
  EXPECT_FALSE(match<symbol>(empty));
}
