#include "scheme_fixture.hpp"

#include "util/define_struct.hpp"

using namespace insider;

struct define_struct_fixture : scheme_fixture { };

struct s : leaf_object<s> {
  static constexpr char const* scheme_name = "insider::s";

  int number;

  explicit
  s(int n) : number{n} { }

  int
  get_number() const { return number; }

  void
  set_number(int value) { number = value; }
};

static int
s_number(context&, ptr<s> s) {
  return s->number;
}

TEST_F(define_struct_fixture, defines_predicate) {
  define_struct<s>(ctx, "s", ctx.internal_module());
  auto f = eval("s?");
  EXPECT_EQ(call_with_continuation_barrier(ctx, f, {make<s>(ctx, 0)}).get(),
            ctx.constants->t);
  EXPECT_EQ(call_with_continuation_barrier(ctx, f, {ctx.intern("s")}).get(),
            ctx.constants->f);
}

TEST_F(define_struct_fixture, accessor_for_member) {
  define_struct<s>(ctx, "s", ctx.internal_module())
    .field<&s::number>("number")
    ;
  auto f = eval("s-number");
  EXPECT_EQ(
    expect<integer>(
      call_with_continuation_barrier(ctx, f, {make<s>(ctx, 12)}).get()
    ).value(),
    12
  );
}

TEST_F(define_struct_fixture, accessor_for_getter) {
  define_struct<s>(ctx, "s", ctx.internal_module())
    .field<&s::get_number>("number")
    ;
  auto f = eval("s-number");
  EXPECT_EQ(
    expect<integer>(
      call_with_continuation_barrier(ctx, f, {make<s>(ctx, 12)}).get()
    ).value(),
    12
  );
}

TEST_F(define_struct_fixture, setter_for_member) {
  define_struct<s>(ctx, "s", ctx.internal_module())
    .mutable_field<&s::number>("number")
    ;
  auto f = eval("(lambda (x) (s-number-set! x (* (s-number x) 2)) x)");
  auto value = expect<s>(
    call_with_continuation_barrier(ctx, f, {make<s>(ctx, 6)}).get()
  );
  EXPECT_EQ(value->number, 12);
}

TEST_F(define_struct_fixture, setter_for_setter) {
  define_struct<s>(ctx, "s", ctx.internal_module())
    .mutable_field<&s::get_number, &s::set_number>("number")
    ;
  auto f = eval("(lambda (x) (s-number-set! x (* (s-number x) 2)) x)");
  auto value = expect<s>(
    call_with_continuation_barrier(ctx, f, {make<s>(ctx, 6)}).get()
  );
  EXPECT_EQ(value->number, 12);
}

TEST_F(define_struct_fixture, non_member_setter) {
  define_struct<s>(ctx, "s", ctx.internal_module())
    .field<&s_number>("number");
  auto f = eval("s-number");
  EXPECT_EQ(
    expect<integer>(
      call_with_continuation_barrier(ctx, f, {make<s>(ctx, 12)}).get()
    ).value(),
    12
  );
}
