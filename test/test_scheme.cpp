#include <gtest/gtest.h>

#include "bytecode.hpp"
#include "compiler.hpp"
#include "converters.hpp"
#include "io.hpp"
#include "scheme.hpp"
#include "vm.hpp"

using namespace scm;

struct scheme : testing::Test {
  context ctx;

  generic_ptr
  read(std::string const& expr) {
    return scm::read(ctx, expr);
  }

  generic_ptr
  eval(std::string const& expr) {
    auto m = make<module>(ctx);
    import_all(m, ctx.constants.internal);
    auto f = compile_expression(ctx, read(expr), m);
    auto state = make_state(ctx, f);
    return run(state);
  }

  generic_ptr
  eval_module(std::string const& expr) {
    auto m = compile_module(ctx, read_multiple(ctx, expr));
    auto state = make_state(ctx, module_top_level_procedure(m));
    return run(state);
  }
};

struct aaa : object {
  bool* alive;

  explicit
  aaa(bool* alive) : alive{alive} { *alive = true; }

  ~aaa() override { *alive = false; }
};

TEST_F(scheme, collect_direct_garbage) {
  bool one{}, two{}, three{};
  ptr<aaa> a = ctx.store.make<aaa>(&one);
  ptr<aaa> b = ctx.store.make<aaa>(&two);
  ptr<aaa> c = ctx.store.make<aaa>(&three);

  ctx.store.collect_garbage();

  EXPECT_TRUE(one);
  EXPECT_TRUE(two);
  EXPECT_TRUE(three);

  a.reset();
  ctx.store.collect_garbage();

  EXPECT_FALSE(one);
  EXPECT_TRUE(two);
  EXPECT_TRUE(three);

  b.reset();
  c.reset();
  ctx.store.collect_garbage();

  EXPECT_FALSE(one);
  EXPECT_FALSE(two);
  EXPECT_FALSE(three);
}

struct bbb : object {
  bool* alive;
  object* child = nullptr;

  explicit
  bbb(bool* alive) : alive{alive} { *alive = true; }

  ~bbb() override { *alive = false; }

  void
  for_each_subobject(std::function<void(object*)> const& f) override {
    f(child);
  }
};

TEST_F(scheme, collect_indirect_garbage) {
  bool one{}, two{}, three{}, four{};
  ptr<bbb> root = ctx.store.make<bbb>(&one);
  root->child = ctx.store.make<bbb>(&two).get();
  static_cast<bbb*>(root->child)->child = ctx.store.make<bbb>(&three).get();
  static_cast<bbb*>(static_cast<bbb*>(root->child)->child)->child = ctx.store.make<bbb>(&four).get();

  ctx.store.collect_garbage();
  EXPECT_TRUE(one);
  EXPECT_TRUE(two);
  EXPECT_TRUE(three);
  EXPECT_TRUE(four);

  static_cast<bbb*>(root->child)->child = nullptr;
  ctx.store.collect_garbage();
  EXPECT_TRUE(one);
  EXPECT_TRUE(two);
  EXPECT_FALSE(three);
  EXPECT_FALSE(four);

  root.reset();
  ctx.store.collect_garbage();
  EXPECT_FALSE(one);
  EXPECT_FALSE(two);
  EXPECT_FALSE(three);
  EXPECT_FALSE(four);
}

TEST_F(scheme, collect_circles) {
  bool one{}, two{};
  ptr<bbb> a = ctx.store.make<bbb>(&one);
  a->child = ctx.store.make<bbb>(&two).get();
  static_cast<bbb*>(a->child)->child = a.get();

  ctx.store.collect_garbage();
  EXPECT_TRUE(one);
  EXPECT_TRUE(two);

  a.reset();
  ctx.store.collect_garbage();
  EXPECT_FALSE(one);
  EXPECT_FALSE(two);
}

TEST_F(scheme, weak_ptr) {
  bool one{};
  ptr<aaa> a = make<aaa>(ctx, &one);
  weak_ptr<aaa> w = a;

  ctx.store.collect_garbage();
  EXPECT_TRUE(one);
  EXPECT_TRUE(w);

  a.reset();
  ctx.store.collect_garbage();
  EXPECT_FALSE(one);
  EXPECT_FALSE(w);

  ptr<aaa> b = w.lock();
  EXPECT_FALSE(b);
}

TEST_F(scheme, type_predicates) {
  ptr<pair> p = make<pair>(ctx, ctx.constants.null, ctx.constants.null);
  generic_ptr x = p;
  generic_ptr null = ctx.constants.null;

  EXPECT_TRUE(is<pair>(x));
  EXPECT_FALSE(is<pair>(null));
  EXPECT_TRUE(expect<pair>(x) == p);
  EXPECT_THROW(expect<pair>(null), type_error);

  if (ptr<pair> q = match<pair>(x))
    SUCCEED();
  else
    ADD_FAILURE();

  if (ptr<pair> q = match<pair>(null))
    ADD_FAILURE();
  else
    SUCCEED();
}

TEST_F(scheme, is_list) {
  // (1 . 2)
  ptr<pair> l1 = make<pair>(ctx, make<integer>(ctx, 1), make<integer>(ctx, 2));
  EXPECT_FALSE(is_list(ctx, l1));

  // (1 2)
  ptr<pair> l2 = make<pair>(ctx,
                            make<integer>(ctx, 1),
                            make<pair>(ctx,
                                       make<integer>(ctx, 2),
                                       ctx.constants.null));
  EXPECT_TRUE(is_list(ctx, l2));

  // (0 1 2)
  ptr<pair> l3 = make<pair>(ctx, make<integer>(ctx, 0), l2);
  EXPECT_TRUE(is_list(ctx, l3));
}

TEST_F(scheme, make_list) {
  generic_ptr empty = make_list(ctx);
  EXPECT_TRUE(empty == ctx.constants.null);

  generic_ptr l = make_list(ctx,
                            make<integer>(ctx, 1),
                            make<integer>(ctx, 2),
                            make<integer>(ctx, 3));
  auto first = expect<pair>(l);
  EXPECT_EQ(expect<integer>(car(first))->value(), 1);

  auto second = expect<pair>(cdr(first));
  EXPECT_EQ(expect<integer>(car(second))->value(), 2);

  auto third = expect<pair>(cdr(second));
  EXPECT_EQ(expect<integer>(car(third))->value(), 3);

  EXPECT_EQ(cdr(third), ctx.constants.null);
}

TEST_F(scheme, intern) {
  ptr<symbol> a_1 = ctx.intern("a");
  ptr<symbol> b_1 = ctx.intern("b");
  ptr<symbol> a_2 = ctx.intern("a");

  EXPECT_TRUE(a_1);
  EXPECT_TRUE(b_1);
  EXPECT_TRUE(a_2);

  EXPECT_EQ(a_1, a_2);
  EXPECT_NE(a_1, b_1);

  b_1.reset();
  ctx.store.collect_garbage();

  ptr<symbol> b_2 = ctx.intern("b");
  ptr<symbol> b_3 = ctx.intern("b");
  EXPECT_TRUE(b_2);
  EXPECT_TRUE(b_3);
  EXPECT_EQ(b_2, b_3);
}

TEST_F(scheme, vector) {
  ptr<vector> v1 = make<vector>(ctx, 3);
  v1->set(0, make<integer>(ctx, 1));
  v1->set(1, make<integer>(ctx, 2));
  v1->set(2, make<integer>(ctx, 3));

  EXPECT_EQ(v1->size(), 3u);

  EXPECT_EQ(expect<integer>(vector_ref(v1, 0))->value(), 1);
  EXPECT_EQ(expect<integer>(vector_ref(v1, 1))->value(), 2);
  EXPECT_EQ(expect<integer>(vector_ref(v1, 2))->value(), 3);

  EXPECT_THROW(vector_ref(v1, 3), std::runtime_error);
  EXPECT_THROW(v1->set(4, make<integer>(ctx, 4)), std::runtime_error);

  ptr<vector> v2 = make<vector>(ctx, 2);
  bool one{}, two{};
  v2->set(0, make<aaa>(ctx, &one));
  v2->set(1, make<aaa>(ctx, &two));

  EXPECT_TRUE(one);
  EXPECT_TRUE(two);

  v2.reset();
  ctx.store.collect_garbage();

  EXPECT_FALSE(one);
  EXPECT_FALSE(two);
}

TEST_F(scheme, read_integer) {
  using namespace std::literals;
  EXPECT_EQ(expect<integer>(read("0"))->value(), 0);
  EXPECT_EQ(expect<integer>(read("2"))->value(), 2);
  EXPECT_EQ(expect<integer>(read("-2"))->value(), -2);
  EXPECT_EQ(expect<integer>(read("+215"))->value(), +215);
  EXPECT_EQ(expect<integer>(read("-3812"))->value(), -3812);
  EXPECT_EQ(expect<integer>(read("9223372036854775807"))->value(), 9223372036854775807);
  EXPECT_EQ(expect<integer>(read("9223372036854775806"))->value(), 9223372036854775806);
  EXPECT_EQ(expect<integer>(read("-9223372036854775808"))->value(),
            std::numeric_limits<integer::value_type>::min());
  EXPECT_THROW(read("9223372036854775808"), parse_error);
  EXPECT_THROW(read("-9223372036854775809"), parse_error);
  EXPECT_EQ(expect<integer>(read("9223372036854775806"))->value(), 9223372036854775806);
  EXPECT_EQ(expect<integer>(read("-9223372036854775806"))->value(), -9223372036854775806);
}

TEST_F(scheme, read_list) {
  generic_ptr empty_1 = read("()");
  EXPECT_EQ(empty_1, ctx.constants.null);

  generic_ptr empty_2 = read("(   )");
  EXPECT_EQ(empty_2, ctx.constants.null);

  generic_ptr single_element = read("(1)");
  EXPECT_TRUE(is_list(ctx, single_element));
  EXPECT_EQ(expect<integer>(car(expect<pair>(single_element)))->value(), 1);
  EXPECT_EQ(cdr(expect<pair>(single_element)), ctx.constants.null);

  generic_ptr two_elements = read("(1 2)");
  EXPECT_TRUE(is_list(ctx, two_elements));
  EXPECT_EQ(expect<integer>(car(expect<pair>(two_elements)))->value(), 1);
  EXPECT_EQ(expect<integer>(car(expect<pair>(cdr(expect<pair>(two_elements)))))->value(), 2);
  EXPECT_EQ(cdr(expect<pair>(cdr(expect<pair>(two_elements)))), ctx.constants.null);

  generic_ptr no_elements = read("()");
  EXPECT_EQ(no_elements, ctx.constants.null);

  generic_ptr nested = read("(1 (2 3))");
  EXPECT_TRUE(is_list(ctx, nested));
  EXPECT_EQ(expect<integer>(car(expect<pair>(nested)))->value(), 1);
  EXPECT_TRUE(is_list(ctx, car(expect<pair>(cdr(expect<pair>(nested))))));
  ptr<pair> sublist_1 = expect<pair>(car(expect<pair>(cdr(expect<pair>(nested)))));
  EXPECT_EQ(expect<integer>(car(sublist_1))->value(), 2);
  EXPECT_EQ(expect<integer>(car(expect<pair>(cdr(sublist_1))))->value(), 3);

  EXPECT_THROW(read("("), parse_error);
  EXPECT_THROW(read("(1 2"), parse_error);
  EXPECT_THROW(read("(()"), parse_error);
}

TEST_F(scheme, read_vector) {
  auto v1 = expect<vector>(read("#()"));
  EXPECT_EQ(expect<vector>(v1)->size(), 0);

  auto v2 = expect<vector>(read("#(1 2 3)"));
  EXPECT_EQ(v2->size(), 3);
  EXPECT_EQ(expect<integer>(vector_ref(v2, 0))->value(), 1);
  EXPECT_EQ(expect<integer>(vector_ref(v2, 1))->value(), 2);
  EXPECT_EQ(expect<integer>(vector_ref(v2, 2))->value(), 3);

  auto v3 = expect<vector>(read("#(#(a b) c #(d e) f)"));
  EXPECT_EQ(v3->size(), 4);
  EXPECT_TRUE(is<vector>(vector_ref(v3, 0)));
  EXPECT_TRUE(is<symbol>(vector_ref(v3, 1)));

  EXPECT_THROW(read("#("), parse_error);
  EXPECT_THROW(read("#(1 2"), parse_error);
}

TEST_F(scheme, read_symbol) {
  EXPECT_EQ(read("foo"), ctx.intern("foo"));
  EXPECT_EQ(read("multiple-words"), ctx.intern("multiple-words"));
  EXPECT_EQ(read("%special-symbol"), ctx.intern("%special-symbol"));
  EXPECT_EQ(read("+"), ctx.intern("+"));
  EXPECT_EQ(read("-"), ctx.intern("-"));
  EXPECT_EQ(read("+fun"), ctx.intern("+fun"));
  EXPECT_EQ(read("#$if"), ctx.intern("#$if"));

  generic_ptr l = read("(one two three)");
  ASSERT_TRUE(is_list(ctx, l));
  EXPECT_EQ(expect<symbol>(car(expect<pair>(l)))->value(), "one");
  EXPECT_EQ(expect<symbol>(car(expect<pair>(cdr(expect<pair>(l)))))->value(), "two");
  EXPECT_EQ(expect<symbol>(car(expect<pair>(cdr(expect<pair>(cdr(expect<pair>(l)))))))->value(), "three");
}

TEST_F(scheme, read_string) {
  EXPECT_EQ(expect<string>(read(R"("foo")"))->value(), "foo");
  EXPECT_EQ(expect<string>(read(R"("one\ntwo")"))->value(), "one\ntwo");
  EXPECT_EQ(expect<string>(read(R"("this \"is\" a quote")"))->value(), "this \"is\" a quote");

  EXPECT_THROW(read(R"("unterminated)"), parse_error);
  EXPECT_THROW(read(R"("\invalid escape")"), parse_error);
  EXPECT_THROW(read(R"("\)"), parse_error);
  EXPECT_THROW(read(R"("\")"), parse_error);
}

TEST_F(scheme, read_multiple) {
  std::vector<generic_ptr> result1 = read_multiple(ctx, "foo bar baz");
  ASSERT_EQ(result1.size(), 3);
  EXPECT_EQ(expect<symbol>(result1[0])->value(), "foo");
  EXPECT_EQ(expect<symbol>(result1[1])->value(), "bar");
  EXPECT_EQ(expect<symbol>(result1[2])->value(), "baz");

  std::vector<generic_ptr> result2 = read_multiple(ctx, "(foo) (bar 2)");
  ASSERT_EQ(result2.size(), 2);
  EXPECT_TRUE(is_list(ctx, result2[0]));
  EXPECT_EQ(list_length(ctx, result2[0]), 1);

  EXPECT_TRUE(is_list(ctx, result2[1]));
  EXPECT_EQ(list_length(ctx, result2[1]), 2);
}

TEST(bytecode, instruction_info_consistency) {
  EXPECT_EQ(opcode_to_info(opcode::add).opcode, opcode::add);
  EXPECT_EQ(opcode_to_info(opcode::add).mnemonic, "add");

  EXPECT_EQ(mnemonic_to_info("add").mnemonic, "add");
  EXPECT_EQ(mnemonic_to_info("add").opcode, opcode::add);
}

template <typename T, typename... Args>
operand
make_static(context& ctx, Args&&... args) {
  return operand::static_(ctx.intern_static(make<T>(ctx, std::forward<Args>(args)...)));
}

TEST_F(scheme, exec_arithmetic) {
  // 2 * (3 + 6). The input constants are stored in statics, result is stored in
  // local register 0.
  auto two = make_static<integer>(ctx, 2);
  auto three = make_static<integer>(ctx, 3);
  auto six = make_static<integer>(ctx, 6);

  auto proc = make<procedure>(
    ctx,
    bytecode{{opcode::add,      three, six,               operand::local(1)},
             {opcode::multiply, two,   operand::local(1), operand::local(0)}},
    2,
    0
  );
  auto state = make_state(ctx, proc);
  run(state);

  EXPECT_EQ(assume<integer>(call_frame_local(state.current_frame, 0))->value(), 18);
}

TEST_F(scheme, exec_calls) {
  // f(x, y) = 2 * x + y
  // Evaluate: 3 * f(5, 7) + f(2, f(3, 4))

  auto two = make_static<integer>(ctx, 2);

  auto f = make_static<procedure>(
    ctx,
    bytecode{{opcode::multiply, two,               operand::local(0), operand::local(2)},
             {opcode::add,      operand::local(2), operand::local(1), operand::local(2)},
             {opcode::ret,      operand::local(2), {},                {}}},
    3,
    2
  );

  auto three = make_static<integer>(ctx, 3);
  auto five = make_static<integer>(ctx, 5);
  auto seven = make_static<integer>(ctx, 7);
  auto four = make_static<integer>(ctx, 4);

  auto global = make<procedure>(
    ctx,
    bytecode{{opcode::call,     f,                 operand::immediate(2), operand::local(0)},
             {opcode::data,     five,              seven,                 {}},
             {opcode::multiply, three,             operand::local(0),     operand::local(0)},
             {opcode::call,     f,                 operand::immediate(2), operand::local(2)},
             {opcode::data,     three,             four,                  {}},
             {opcode::call,     f,                 operand::immediate(2), operand::local(1)},
             {opcode::data,     two,               operand::local(2),     {}},
             {opcode::add,      operand::local(0), operand::local(1),     operand::local(0)}},
    3,
    0
  );
  auto state = make_state(ctx, global);
  run(state);

  auto native_f = [] (int x, int y) { return 2 * x + y; };
  EXPECT_EQ(assume<integer>(call_frame_local(state.current_frame, 0))->value(),
            3 * native_f(5, 7) + native_f(2, native_f(3, 4)));
}

TEST_F(scheme, three_argument_calls) {
  // f(x, y, z) = 2 * x + 3 * y + 4 * z
  // Evaluate 2 * f(1, 2, 3)

  auto one = make_static<integer>(ctx, 1);
  auto two = make_static<integer>(ctx, 2);
  auto three = make_static<integer>(ctx, 3);
  auto four = make_static<integer>(ctx, 4);

  auto f = make_static<procedure>(
    ctx,
    bytecode{{opcode::multiply, two,               operand::local(0), operand::local(3)},
             {opcode::multiply, three,             operand::local(1), operand::local(4)},
             {opcode::multiply, four,              operand::local(2), operand::local(5)},
             {opcode::add,      operand::local(3), operand::local(4), operand::local(3)},
             {opcode::add,      operand::local(3), operand::local(5), operand::local(3)},
             {opcode::ret,      operand::local(3), {},                {}}},
    6,
    3
  );
  auto global = make<procedure>(
    ctx,
    bytecode{{opcode::call,     f,   operand::immediate(3), operand::local(0)},
             {opcode::data,     one, two,                   three},
             {opcode::multiply, two, operand::local(0),     operand::local(0)}},
    1,
    0
  );
  auto state = make_state(ctx, global);
  run(state);

  auto native_f = [] (int x, int y, int z) { return 2 * x + 3 * y + 4 * z; };
  EXPECT_EQ(expect<integer>(call_frame_local(state.current_frame, 0))->value(),
            2 * native_f(1, 2, 3));
}

TEST_F(scheme, exec_tail_calls) {
  // f(x) = g(x)
  // g(x) = 2 * x
  auto two = make_static<integer>(ctx, 2);
  auto g = make_static<procedure>(
    ctx,
    bytecode{{opcode::multiply, two,               operand::local(0), operand::local(1)},
             {opcode::ret,      operand::local(1), {},                {}}},
    2,
    1
  );
  auto f = make_static<procedure>(
    ctx,
    bytecode{{opcode::tail_call, g,                 operand::immediate(1), {}},
             {opcode::data,      operand::local(0), {},                    {}}},
    1,
    1
  );
  auto six = make_static<integer>(ctx, 6);
  auto global = make<procedure>(
    ctx,
    bytecode{{opcode::call, f,   operand::immediate(1), operand::local(0)},
             {opcode::data, six, {},                    {}}},
    1,
    0
  );
  auto state = make_state(ctx, global);
  run(state);

  EXPECT_EQ(assume<integer>(call_frame_local(state.current_frame, 0))->value(), 12);
}

TEST_F(scheme, exec_loop) {
  // sum = 0
  // i = 0
  // while i < 10
  //   sum += i
  //   i += 1

  auto zero = make_static<integer>(ctx, 0);
  auto ten = make_static<integer>(ctx, 10);
  auto one = make_static<integer>(ctx, 1);
  auto global = make<procedure>(
    ctx,
    bytecode{{opcode::set,          zero,                {},                 operand::local(0)},
             {opcode::set,          zero,                {},                 operand::local(1)},
             {opcode::less_than,    operand::local(1),   ten,                operand::local(2)},
             {opcode::jump_unless,  operand::local(2),   operand::offset(3), {}},
             {opcode::add,          operand::local(0),   operand::local(1),  operand::local(0)},
             {opcode::add,          operand::local(1),   one,                operand::local(1)},
             {opcode::jump,         operand::offset(-5), {},                 {}},
             {opcode::no_operation, {},                  {},                 {}}},
    3,
    0
  );
  auto state = make_state(ctx, global);
  run(state);

  EXPECT_EQ(assume<integer>(call_frame_local(state.current_frame, 0))->value(), 45);
}

TEST_F(scheme, exec_native_call) {
  auto native = [] (context& ctx, std::vector<generic_ptr> const& args) {
    return make<integer>(ctx,
                         2 * expect<integer>(args[0])->value()
                         + 3 * expect<integer>(args[1])->value()
                         + 5 * expect<integer>(args[2])->value());
  };
  auto native_static = make_static<native_procedure>(ctx, native);
  auto ten = make_static<integer>(ctx, 10);
  auto twenty = make_static<integer>(ctx, 20);
  auto thirty = make_static<integer>(ctx, 30);
  auto global = make<procedure>(
    ctx,
    bytecode{{opcode::call, native_static, operand::immediate(3), operand::local(0)},
             {opcode::data, ten,           twenty,                thirty}},
    1,
    0
  );
  auto state = make_state(ctx, global);
  run(state);

  EXPECT_EQ(assume<integer>(call_frame_local(state.current_frame, 0))->value(),
            2 * 10 + 3 * 20 + 5 * 30);
}

TEST_F(scheme, exec_closure_ref) {
  auto add = make_static<procedure>(
    ctx,
    bytecode{{opcode::add, operand::local(0), operand::closure(0), operand::local(0)},
             {opcode::ret, operand::local(0), {},                  {}}},
    1, 1
  );
  auto three = make_static<integer>(ctx, 3);
  auto five = make_static<integer>(ctx, 5);
  auto global = make<procedure>(
    ctx,
    bytecode{{opcode::make_closure, add,               operand::immediate(1), operand::local(1)},
             {opcode::data,         three,             {},                    {}},
             {opcode::call,         operand::local(1), operand::immediate(1), operand::local(0)},
             {opcode::data,         five,              {},                    {}}},
    2, 0
  );
  auto state = make_state(ctx, global);
  run(state);

  EXPECT_EQ(assume<integer>(call_frame_local(state.current_frame, 0))->value(), 5 + 3);
}

TEST_F(scheme, exec_cons) {
  auto one = make_static<integer>(ctx, 1);
  auto two = make_static<integer>(ctx, 2);
  auto three = make_static<integer>(ctx, 3);
  auto global = make<procedure>(
    ctx,
    bytecode{{opcode::cons, three, ctx.statics.null,  operand::local(0)},
             {opcode::cons, two,   operand::local(0), operand::local(0)},
             {opcode::cons, one,   operand::local(0), operand::local(0)}},
    1, 0
  );
  auto state = make_state(ctx, global);
  run(state);

  EXPECT_TRUE(equal(call_frame_local(state.current_frame, 0), read("(1 2 3)")));
}

TEST_F(scheme, exec_make_vector) {
  auto one = make_static<integer>(ctx, 1);
  auto two = make_static<integer>(ctx, 2);
  auto three = make_static<integer>(ctx, 3);
  auto global = make<procedure>(
    ctx,
    bytecode{{opcode::make_vector, operand::immediate(3), {},  operand::local(0)},
             {opcode::data,        one,                   two, three}},
    1, 0
  );

  auto state = make_state(ctx, global);
  run(state);

  EXPECT_TRUE(equal(call_frame_local(state.current_frame, 0), read("#(1 2 3)")));
}

TEST_F(scheme, compile_arithmetic) {
  generic_ptr result = eval(
    "(+ 2 3 (* 5 9) (- 9 8) (/ 8 2))"
  );
  EXPECT_EQ(expect<integer>(result)->value(),
            2 + 3 + (5 * 9) + (9 - 8) + (8 / 2));
}

TEST_F(scheme, compile_let) {
  generic_ptr result = eval(
    R"(
      (#$let ((a 2)
              (b 5))
        (#$let ((sum (+ a b))
                (product (* a b)))
          (- sum product)))
    )"
  );

  int a = 2;
  int b = 5;
  int sum = a + b;
  int product = a * b;
  EXPECT_EQ(expect<integer>(result)->value(), sum - product);

  EXPECT_THROW(compile_expression(ctx, read("(let ((a 2)))"), ctx.constants.internal),
               std::runtime_error);
  EXPECT_THROW(compile_expression(ctx, read("(let foo)"), ctx.constants.internal),
               std::runtime_error);
}

TEST_F(scheme, let_shadowing) {
  generic_ptr result = eval(
    R"(
      (#$let ((a 2))
        (#$let ((a 5))
          a))
    )"
  );
  EXPECT_EQ(expect<integer>(result)->value(), 5);
}

TEST_F(scheme, compile_lambda) {
  generic_ptr result1 = eval(
    R"(
      (#$let ((twice (#$lambda (x) (* 2 x))))
        (twice 4))
    )"
  );
  EXPECT_EQ(expect<integer>(result1)->value(), 8);

  generic_ptr result2 = eval(
    R"(
      (#$let ((sum (#$lambda (a b c d) (+ a b c d))))
        (sum 1 2 3 4))
    )"
  );
  EXPECT_EQ(expect<integer>(result2)->value(), 1 + 2 + 3 + 4);

  generic_ptr result3 = eval(
    R"(
      (#$let ((call-with-sum (#$lambda (f a b) (f (+ a b))))
              (f (#$lambda (x) (* 2 x))))
        (call-with-sum f 3 4))
    )"
  );
  EXPECT_EQ(expect<integer>(result3)->value(), 2 * (3 + 4));
}

TEST_F(scheme, compile_if) {
  generic_ptr result1 = eval("(#$if #t 2 3)");
  EXPECT_EQ(expect<integer>(result1)->value(), 2);

  generic_ptr result2 = eval("(#$if #f 2 3)");
  EXPECT_EQ(expect<integer>(result2)->value(), 3);

  generic_ptr result3 = eval("(#$if #t 2)");
  EXPECT_EQ(expect<integer>(result3)->value(), 2);

  generic_ptr result4 = eval("(#$if #f 2)");
  EXPECT_EQ(result4, ctx.constants.void_);

  generic_ptr result5 = eval(
    R"(
      (#$let ((f (#$lambda (x) (* 2 x)))
              (x 4))
        (#$if (< x 5)
              (f x)
              0))
    )"
  );
  EXPECT_EQ(expect<integer>(result5)->value(), 8);

  generic_ptr result6 = eval(
    R"(
      (#$let ((f (#$lambda (x) (* 2 x)))
              (x 6))
        (#$if (< x 5)
              (f x)
              0))
    )"
  );
  EXPECT_EQ(expect<integer>(result6)->value(), 0);;

  generic_ptr result7 = eval(
    R"(
      (#$let ((f (#$lambda (x) (* 2 x)))
              (x 4))
        (#$if (< x 5)
              0
              (f x)))
    )"
  );
  EXPECT_EQ(expect<integer>(result7)->value(), 0);

  generic_ptr result8 = eval(
    R"(
      (#$let ((f (#$lambda (x) (* 2 x)))
              (x 6))
        (#$if (< x 5)
              0
              (f x)))
    )"
  );
  EXPECT_EQ(expect<integer>(result8)->value(), 12);

  generic_ptr result9 = eval(
    R"(
      (#$let ((f (#$lambda (x) (* 2 x)))
              (g (#$lambda (x) (+ 2 x)))
            (x 4))
        (#$if (< x 5)
              (f x)
              (g x)))
    )"
  );
  EXPECT_EQ(expect<integer>(result9)->value(), 8);

  generic_ptr result10 = eval(
    R"(
      (#$let ((f (#$lambda (x) (* 2 x)))
              (g (#$lambda (x) (+ 10 x)))
            (x 6))
        (#$if (< x 5)
              (f x)
              (g x)))
    )"
  );
  EXPECT_EQ(expect<integer>(result10)->value(), 16);
}

TEST_F(scheme, compile_closure) {
  generic_ptr result1 = eval(
    R"(
      (#$let ((make-adder (#$lambda (x) (#$lambda (y) (+ x y)))))
        (#$let ((add-2 (make-adder 2)))
          (add-2 5)))
    )"
  );
  EXPECT_EQ(expect<integer>(result1)->value(), 7);

  generic_ptr result2 = eval(
    R"(
      (#$let ((x 7))
        (#$let ((f (#$lambda (y) (+ x y))))
          (f 3)))
    )"
  );
  EXPECT_EQ(expect<integer>(result2)->value(), 10);
}

TEST_F(scheme, compile_set) {
  generic_ptr result1 = eval(
    R"(
      (#$let ((x 2))
        (#$set! x 5)
        x)
    )"
  );
  EXPECT_EQ(expect<integer>(result1)->value(), 5);

  generic_ptr result2 = eval(
    R"(
      (#$let ((fact #void))
        (#$set! fact (#$lambda (n)
                       (#$if (= n 0)
                         1
                         (* n (fact (- n 1))))))
        (fact 5))
    )"
  );
  EXPECT_EQ(expect<integer>(result2)->value(), 120);

  generic_ptr result3 = eval(
    R"(
      (#$let ((f (#$lambda (x)
                   (#$set! x (* 2 x))
                   (#$lambda (y)
                     (+ x y)))))
        ((f 5) 3))
    )"
  );
  EXPECT_EQ(expect<integer>(result3)->value(), 13);
}

TEST_F(scheme, compile_box) {
  generic_ptr result = eval(
    R"(
      (#$let ((b1 (#$box 5))
              (b2 (#$box 7)))
        (#$box-set! b1 (+ (#$unbox b1) (#$unbox b2)))
        (#$unbox b1))
    )"
  );
  EXPECT_EQ(expect<integer>(result)->value(), 12);
}

TEST_F(scheme, compile_higher_order_arithmetic) {
  generic_ptr result = eval(
    R"(
      (#$let ((f (#$lambda (op x y) (op x y))))
        (f + 2 3))
    )"
  );
  EXPECT_EQ(expect<integer>(result)->value(), 5);
}

TEST_F(scheme, compile_module) {
  int sum = 0;
  define_top_level(
    ctx, ctx.constants.internal, "f",
    make<native_procedure>(ctx,
                           [&] (context& ctx, std::vector<generic_ptr> const& args) {
                             sum += expect<integer>(args[0])->value();
                             return ctx.constants.void_;
                           }),
    true
  );

  auto m = compile_module(ctx,
                          read_multiple(ctx,
                                        "(import (insider internal))"
                                        "(f 3)"
                                        "(#$let ((x 2))"
                                        "  (f x))"));
  auto state = make_state(ctx, module_top_level_procedure(m));
  run(state);

  EXPECT_EQ(sum, 5);
}

TEST_F(scheme, compile_top_level_define) {
  auto result1 = eval_module(
    R"(
      (import (insider internal))
      (#$define f
        (#$lambda (x)
          (+ x 2)))
      (#$define var 7)
      (f var)
    )"
  );
  EXPECT_EQ(expect<integer>(result1)->value(), 9);

  auto result2 = eval_module(
    R"(
      (import (insider internal))
      (#$define x 4)
      (#$define y 7)
      (#$set! x (+ y 2))
      x
    )"
  );
  EXPECT_EQ(expect<integer>(result2)->value(), 9);
}

TEST_F(scheme, compile_internal_define) {
  auto result1 = eval_module(
    R"(
      (import (insider internal))

      (#$define f
        (#$lambda (n)
          (#$define go
            (#$lambda (k accum)
              (#$if (= k 0)
                accum
                (go (- k 1) (+ accum k)))))
          (go n 0)))
      (f 5)
    )"
  );
  EXPECT_EQ(expect<integer>(result1)->value(), 5 + 4 + 3 + 2 + 1);
}

TEST_F(scheme, define_lambda) {
  define_lambda<int(int, int)>(
    ctx, ctx.constants.internal, "f", true,
    [] (int a, int b) { return 2 * a + b; }
  );

  int x = 0;
  define_lambda<void(int)>(
    ctx, ctx.constants.internal, "g", true,
    [&] (int a) { x += a; }
  );

  define_lambda<std::string(int)>(
    ctx, ctx.constants.internal, "to-string", true,
    [] (int i) { return std::to_string(i); }
  );

  auto result1 = eval("(f 5 7)");
  EXPECT_EQ(expect<integer>(result1)->value(), 2 * 5 + 7);

  auto result2 = eval("(g 9)");
  EXPECT_EQ(x, 9);

  auto result3 = eval("(to-string 3)");
  EXPECT_EQ(expect<string>(result3)->value(), "3");
}

static std::string
to_string(context& ctx, generic_ptr const& datum) {
  auto out = make<port>(ctx, std::string{}, false, true);
  write_simple(ctx, datum, out);
  return out->get_string();
}

TEST_F(scheme, test_write) {
  EXPECT_EQ(to_string(ctx, read("(1 2 3)")), "(1 2 3)");

  auto p1 = make<pair>(ctx, make<integer>(ctx, 1), make<integer>(ctx, 2));
  EXPECT_EQ(to_string(ctx, p1), "(1 . 2)");

  auto p2 = make<pair>(ctx, make<integer>(ctx, 0), p1);
  EXPECT_EQ(to_string(ctx, p2), "(0 1 . 2)");

  auto v = make<vector>(ctx, 3);
  v->set(0, make<character>(ctx, 'r'));
  v->set(1, p2);
  v->set(2, make_string(ctx, "foobar"));
  EXPECT_EQ(to_string(ctx, v), R"(#(#\r (0 1 . 2) "foobar"))");

  auto s = make_string(ctx, R"(one "two" three \ four)");
  EXPECT_EQ(to_string(ctx, s), R"("one \"two\" three \\ four")");

  auto l = make_list(
    ctx,
    ctx.constants.null,
    ctx.constants.void_,
    ctx.constants.t,
    ctx.constants.f,
    ctx.intern("symbol"),
    make_string(ctx, "string"),
    make<character>(ctx, 'c'),
    make<integer>(ctx, -13)
  );
  EXPECT_EQ(to_string(ctx, l), R"((() #void #t #f symbol "string" #\c -13))");
}

TEST_F(scheme, quote) {
  auto result1 = eval("(#$quote (a b c))");
  EXPECT_TRUE(is_list(ctx, result1));
  EXPECT_EQ(list_length(ctx, result1), 3);

  auto result2 = eval("(#$quote 2)");
  EXPECT_EQ(expect<integer>(result2)->value(), 2);

  auto result3 = eval("'3");
  EXPECT_EQ(expect<integer>(result3)->value(), 3);

  auto result4 = eval("'(a b)");
  EXPECT_TRUE(is_list(ctx, result4));
  EXPECT_EQ(list_length(ctx, result4), 2);
  EXPECT_EQ(expect<symbol>(car(expect<pair>(result4)))->value(), "a");
  EXPECT_EQ(expect<symbol>(cadr(expect<pair>(result4)))->value(), "b");

  auto result5 = eval("''a");
  EXPECT_TRUE(is_list(ctx, result5));
  EXPECT_EQ(list_length(ctx, result5), 2);
  EXPECT_EQ(expect<symbol>(car(expect<pair>(result5)))->value(), "#$quote");
  EXPECT_EQ(expect<symbol>(cadr(expect<pair>(result5)))->value(), "a");
}

TEST_F(scheme, equal) {
  EXPECT_TRUE(equal(read("1"), read("1")));
  EXPECT_FALSE(equal(read("1"), read("2")));
  EXPECT_FALSE(equal(read("1"), read("sym")));
  EXPECT_TRUE(equal(read("'(1 2)"), read("'(1 2)")));
  EXPECT_TRUE(equal(read("'(1 2)"), read("(#$quote (1 2))")));
  EXPECT_FALSE(equal(read("'(1 2)"), read("'(1 3)")));
  EXPECT_FALSE(equal(read("'(1 2)"), read("'(1 2 3)")));
  EXPECT_TRUE(equal(make_string(ctx, "foo"), make_string(ctx, "foo")));
  EXPECT_FALSE(equal(make_string(ctx, "foo"), make_string(ctx, "bar")));
}

TEST_F(scheme, quasiquote) {
  auto result1 = eval("`5");
  EXPECT_TRUE(equal(result1, read("5")));

  auto result2 = eval("`(1 2 5)");
  EXPECT_TRUE(equal(result2, read("(1 2 5)")));

  auto result3 = eval("(#$let ((a 7)) `(1 ,a 3))");
  EXPECT_TRUE(equal(result3, read("(1 7 3)")));

  auto result4 = eval("`(1 ,(+ 2 3) 3)");
  EXPECT_TRUE(equal(result4, read("(1 5 3)")));

  auto result5 = eval("(#$let ((name 'a)) `(list ,name ',name))");
  EXPECT_TRUE(equal(result5, read("(list a (#$quote a))")));

  auto result6 = eval("`#(1 2 5)");
  EXPECT_TRUE(equal(result6, read("#(1 2 5)")));

  auto result7 = eval("(#$let ((a 12)) `#(3 ,a 5 ,(* a 2) 9))");
  EXPECT_TRUE(equal(result7, read("#(3 12 5 24 9)")));

  auto result8 = eval("(#$let ((b '(b1 b2 b3))) `(a1 a2 ,@b c1 c2))");
  EXPECT_TRUE(equal(result8, read("(a1 a2 b1 b2 b3 c1 c2)")));

  auto result9 = eval("(#$let ((b '(b1 b2 b3))) `(a1 a2 ,b c1 c2))");
  EXPECT_TRUE(equal(result9, read("(a1 a2 (b1 b2 b3) c1 c2)")));

  auto result10 = eval("(#$let ((b '(b1 b2))) `(a1 a2 ,@b))");
  EXPECT_TRUE(equal(result10, read("(a1 a2 b1 b2)")));

  // auto result11 = eval("`(`(a b ,c))");
  // EXPECT_TRUE(equal(result11, read("(#$quasiquote (a b (#$unquote c)))")));

  auto result12 = eval("(#$let ((b '(b1 b2 b3))) `#(a1 a2 ,@b c1 c2))");
  EXPECT_TRUE(equal(result12, read("#(a1 a2 b1 b2 b3 c1 c2)")));

  auto result13 = eval("(#$let ((b '(b1 b2 b3))) `#(a1 a2 ,b c1 c2))");
  EXPECT_TRUE(equal(result13, read("#(a1 a2 (b1 b2 b3) c1 c2)")));

  auto result14 = eval("(#$let ((b '(a1 a2))) `#(,@b b1 b2 b3))");
  EXPECT_TRUE(equal(result14, read("#(a1 a2 b1 b2 b3)")));

  auto result15 = eval("(#$let ((b '(b1 b2))) `#(a1 a2 ,@b))");
  EXPECT_TRUE(equal(result15, read("#(a1 a2 b1 b2)")));
}

TEST_F(scheme, append) {
  auto r1 = eval("(append '(a1 a2 a3) '(b1 b2 b3) '(c1 c2) '(d1) '() '(f1 f2))");
  EXPECT_TRUE(equal(r1, read("(a1 a2 a3 b1 b2 b3 c1 c2 d1 f1 f2)")));

  auto r2 = eval("(append)");
  EXPECT_TRUE(equal(r2, read("()")));

  auto r3 = eval("(append '(a1 a2 a3))");
  EXPECT_TRUE(equal(r3, read("(a1 a2 a3)")));

  auto r4 = eval("(append '(a1 a2) 'tail)");
  EXPECT_EQ(cddr(expect<pair>(r4)), ctx.intern("tail"));

  auto r5 = eval("(append '() '() '() '())");
  EXPECT_TRUE(equal(r5, ctx.constants.null));

  auto r6 = eval("(append '() '(a1 a2))");
  EXPECT_TRUE(equal(r6, read("(a1 a2)")));

  auto r7 = eval("(append '() '(a1 a2) '() '() '(b1 b2 b3))");
  EXPECT_TRUE(equal(r7, read("(a1 a2 b1 b2 b3)")));

  auto r8 = eval("(append '() '(a1 a2) '() '() '(b1 b2 b3) '())");
  EXPECT_TRUE(equal(r8, read("(a1 a2 b1 b2 b3)")));
}
