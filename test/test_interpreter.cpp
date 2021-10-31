#include "scheme_fixture.hpp"

#include "define_procedure.hpp"

using namespace insider;

template <typename T, typename... Args>
operand
make_static(context& ctx, Args&&... args) {
  if constexpr (std::is_same_v<T, integer>)
    return ctx.intern_static(track(ctx, integer_to_ptr(integer{args...})));
  else
    return ctx.intern_static(make_tracked<T>(ctx, std::forward<Args>(args)...));
}

static operand
make_static_procedure(context& ctx, bytecode const& bc, unsigned locals_size, unsigned min_args) {
  return ctx.intern_static(track(ctx, make_procedure(ctx, bc, locals_size, min_args)));
}

static bytecode
make_bytecode(std::vector<instruction> const& instr) {
  bytecode bc;
  for (instruction const& i : instr)
    encode_instruction(bc, i);

  return bc;
}

struct interpreter : scheme_fixture { };

TEST_F(interpreter, exec_arithmetic) {
  // 2 * (3 + 6). The input constants are stored in statics, result is stored in
  // local register 0.
  auto two = make_static<integer>(ctx, 2);
  auto three = make_static<integer>(ctx, 3);
  auto six = make_static<integer>(ctx, 6);

  auto proc = make_procedure(
    ctx,
    make_bytecode({instruction{opcode::load_static, two,   operand{2}},
                   instruction{opcode::load_static, three, operand{3}},
                   instruction{opcode::load_static, six,   operand{4}},
                   instruction{opcode::add,         operand{3}, operand{4}, operand{1}},
                   instruction{opcode::multiply,    operand{2}, operand{1}, operand{0}},
                   instruction{opcode::ret,         operand{0}}}),
    5,
    0
  );
  auto result = call_with_continuation_barrier(ctx, proc, {});
  EXPECT_EQ(assume<integer>(result).value(), 18);
}

TEST_F(interpreter, exec_calls) {
  // f(x, y) = 2 * x + y
  // Evaluate: 3 * f(5, 7) + f(2, f(3, 4))

  auto two = make_static<integer>(ctx, 2);

  auto f = make_static_procedure(
    ctx,
    make_bytecode({instruction{opcode::load_static, two,        operand{3}},
                   instruction{opcode::multiply,    operand{3}, operand{0}, operand{2}},
                   instruction{opcode::add,         operand{2}, operand{1}, operand{2}},
                   instruction{opcode::ret,         operand{2}}}),
    4,
    2
  );

  auto three = make_static<integer>(ctx, 3);
  auto five = make_static<integer>(ctx, 5);
  auto seven = make_static<integer>(ctx, 7);
  auto four = make_static<integer>(ctx, 4);

  auto global = make_procedure(
    ctx,
    make_bytecode({instruction{opcode::load_static, five,  operand{3}},
                   instruction{opcode::load_static, seven, operand{4}},
                   instruction{opcode::load_static, f,     operand{5}},
                   instruction{opcode::call,        operand{5}, operand{0}, operand{3}, operand{4}},
                   instruction{opcode::load_static, three, operand{6}},
                   instruction{opcode::multiply,    operand{6}, operand{0}, operand{0}},
                   instruction{opcode::load_static, four,  operand{7}},
                   instruction{opcode::call,        operand{5}, operand{2}, operand{6}, operand{7}},
                   instruction{opcode::load_static, two,   operand{8}},
                   instruction{opcode::call,        operand{5}, operand{1}, operand{8}, operand{2}},
                   instruction{opcode::add,         operand{0}, operand{1}, operand{0}},
                   instruction{opcode::ret,         operand{0}}}),
    9,
    0
  );
  auto result = call_with_continuation_barrier(ctx, global, {});

  auto native_f = [] (int x, int y) { return 2 * x + y; };
  EXPECT_EQ(assume<integer>(result).value(),
            3 * native_f(5, 7) + native_f(2, native_f(3, 4)));
}

TEST_F(interpreter, exec_tail_calls) {
  // f(x) = g(x)
  // g(x) = 2 * x
  auto two = make_static<integer>(ctx, 2);
  auto g = make_static_procedure(
    ctx,
    make_bytecode({instruction{opcode::load_static, two, operand{2}},
                   instruction{opcode::multiply,    operand{2}, operand{0}, operand{1}},
                   instruction{opcode::ret,         operand{1}}}),
    3,
    1
  );
  auto f = make_static_procedure(
    ctx,
    make_bytecode({instruction{opcode::load_static, g, operand{1}},
                  {instruction{opcode::tail_call,   operand{1}, operand{0}}}}),
    2,
    1
  );
  auto six = make_static<integer>(ctx, 6);
  auto global = make_procedure(
    ctx,
    make_bytecode({instruction{opcode::load_static, f,   operand{1}},
                   instruction{opcode::load_static, six, operand{2}},
                   instruction{opcode::call,        operand{1}, operand{0}, operand{2}},
                   instruction{opcode::ret,         operand{0}}}),
    3,
    0
  );
  auto result = call_with_continuation_barrier(ctx, global, {});
  EXPECT_EQ(assume<integer>(result).value(), 12);
}

TEST_F(interpreter, exec_loop) {
  // sum = 0
  // i = 0
  // while i < 10
  //   sum += i
  //   i += 1

  auto zero = make_static<integer>(ctx, 0);
  auto ten = make_static<integer>(ctx, 10);
  auto one = make_static<integer>(ctx, 1);
  auto global = make_procedure(
    ctx,
    make_bytecode({instruction{opcode::load_static, zero,       operand{3}},
                   instruction{opcode::load_static, ten,        operand{4}},
                   instruction{opcode::load_static, one,        operand{5}},
                   instruction{opcode::set,         operand{3}, operand{0}},
                   instruction{opcode::set,         operand{3}, operand{1}},
                   instruction{opcode::less,        operand{1}, operand{4}, operand{2}},
                   instruction{opcode::jump_unless, operand{2}, operand{10}},
                   instruction{opcode::add,         operand{0}, operand{1}, operand{0}},
                   instruction{opcode::add,         operand{1}, operand{5}, operand{1}},
                   instruction{opcode::jump_back,   operand{17}},
                   instruction{opcode::ret,         operand{0}}}),
    6,
    0
  );
  auto result = call_with_continuation_barrier(ctx, global, {});
  EXPECT_EQ(assume<integer>(result).value(), 45);
}

TEST_F(interpreter, exec_native_call) {
  auto native = [] (context&, object_span args) {
    return integer_to_ptr(integer{2 * expect<integer>(args[0]).value()
                                  + 3 * expect<integer>(args[1]).value()
                                  + 5 * expect<integer>(args[2]).value()});
  };
  auto native_static = make_static<native_procedure>(ctx, native);
  auto ten = make_static<integer>(ctx, 10);
  auto twenty = make_static<integer>(ctx, 20);
  auto thirty = make_static<integer>(ctx, 30);
  auto global = make_procedure(
    ctx,
    make_bytecode({instruction{opcode::load_static, ten,           operand{1}},
                   instruction{opcode::load_static, twenty,        operand{2}},
                   instruction{opcode::load_static, thirty,        operand{3}},
                   instruction{opcode::load_static, native_static, operand{4}},
                   instruction{opcode::call,        operand{4},    operand{0}, operand{1}, operand{2}, operand{3}},
                   instruction{opcode::ret,         operand{0}}}),
    5,
    0
  );
  auto result = call_with_continuation_barrier(ctx, global, {});
  EXPECT_EQ(assume<integer>(result).value(),
            2 * 10 + 3 * 20 + 5 * 30);
}

TEST_F(interpreter, exec_closure_ref) {
  auto add = make_static_procedure(
    ctx,
    make_bytecode({instruction{opcode::add, operand{1}, operand{0}, operand{0}},
                   instruction{opcode::ret, operand{0}}}),
    2,
    1
  );
  auto three = make_static<integer>(ctx, 3);
  auto five = make_static<integer>(ctx, 5);
  auto global = make_procedure(
    ctx,
    make_bytecode({instruction{opcode::load_static, add,   operand{2}},
                   instruction{opcode::load_static, three, operand{3}},
                   instruction{opcode::load_static, five,  operand{4}},
                   instruction{opcode::make_closure, operand{2}, operand{1}, operand{3}},
                   instruction{opcode::call,         operand{1}, operand{0}, operand{4}},
                   instruction{opcode::ret,          operand{0}}}),
    5, 0
  );
  auto result = call_with_continuation_barrier(ctx, global, {});
  EXPECT_EQ(assume<integer>(result).value(), 5 + 3);
}

TEST_F(interpreter, exec_cons) {
  auto one = make_static<integer>(ctx, 1);
  auto two = make_static<integer>(ctx, 2);
  auto three = make_static<integer>(ctx, 3);
  auto global = make_procedure(
    ctx,
    make_bytecode({instruction{opcode::load_static, ctx.statics.null, operand{1}},
                   instruction{opcode::load_static, one,              operand{2}},
                   instruction{opcode::load_static, two,              operand{3}},
                   instruction{opcode::load_static, three,            operand{4}},
                   instruction{opcode::cons,        operand{4},       operand{1}, operand{0}},
                   instruction{opcode::cons,        operand{3},       operand{0}, operand{0}},
                   instruction{opcode::cons,        operand{2},       operand{0}, operand{0}},
                   instruction{opcode::ret,         operand{0}}}),
    5, 0
  );
  auto result = call_with_continuation_barrier(ctx, global, {});
  EXPECT_TRUE(equal(result.get(), read("(1 2 3)")));
}

TEST_F(interpreter, exec_make_vector) {
  auto one = make_static<integer>(ctx, 1);
  auto two = make_static<integer>(ctx, 2);
  auto three = make_static<integer>(ctx, 3);
  auto global = make_procedure(
    ctx,
    make_bytecode({instruction{opcode::load_static, one,   operand{1}},
                   instruction{opcode::load_static, two,   operand{2}},
                   instruction{opcode::load_static, three, operand{3}},
                   instruction{opcode::make_vector, operand{0}, operand{1}, operand{2}, operand{3}},
                   instruction{opcode::ret,         operand{0}}}),
    4, 0
  );

  auto result = call_with_continuation_barrier(ctx, global, {});
  EXPECT_TRUE(equal(result.get(), read("#(1 2 3)")));
}

TEST_F(interpreter, scheme_to_native_to_scheme) {
  define_procedure(ctx, "apply-and-double", ctx.internal_module, true,
                   [] (context& ctx, ptr<procedure> f, ptr<> arg) {
                     return 2 * expect<integer>(call_with_continuation_barrier(ctx, f, {arg})).value();
                   });
  ptr<> result1 = eval_module(
    R"(
      (import (insider internal))

      (define add-3
        (lambda (x)
          (+ x 3)))

      (apply-and-double add-3 5)
   )"
  );
  EXPECT_EQ(expect<integer>(result1).value(), 2 * (5 + 3));

  ptr<> result2 = eval_module(
    R"(
      (import (insider internal))

      (define add-3
        (lambda (x)
          (+ x 3)))

      (let ((result (apply-and-double add-3 5)))
        result)
   )"
  );
  EXPECT_EQ(expect<integer>(result2).value(), 2 * (5 + 3));
}

TEST_F(interpreter, call_variadic_scheme_procedure_from_native) {
  ptr<> f = eval("(lambda args args)");
  ptr<> result = call_with_continuation_barrier(ctx, f, {to_scheme(ctx, 0), to_scheme(ctx, 1), to_scheme(ctx, 2)}).get();
  EXPECT_TRUE(equal(result, read("(0 1 2)")));
}

TEST_F(interpreter, native_tail_calls) {
  define_procedure(
    ctx, "f", ctx.internal_module, true,
    [] (context& ctx, int i, int accum, ptr<> recurse, ptr<> base) {
      if (i == 0)
        return tail_call(ctx, base, {to_scheme(ctx, accum)});
      else
        return tail_call(ctx, recurse, {to_scheme(ctx, i - 1), to_scheme(ctx, accum + i), recurse, base});
    }
  );

  ptr<> result1 = eval_module(
    R"(
      (import (insider internal))

      (f 10 0 f (lambda (x) (* 2 x)))
   )"
  );
  EXPECT_EQ(expect<integer>(result1).value(), 2 * 55);

  ptr<> result2 = eval_module(
    R"(
      (import (insider internal))

      (let ((result (f 10 0 f (lambda (x) (* 2 x)))))
        result)
   )"
  );
  EXPECT_EQ(expect<integer>(result2).value(), 2 * 55);
}
