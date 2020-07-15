#include <gtest/gtest.h>

#include "bytecode.hpp"
#include "compiler.hpp"
#include "converters.hpp"
#include "io.hpp"
#include "scheme.hpp"
#include "vm.hpp"

#include <cmath>

using namespace insider;

struct scheme : testing::Test {
  context ctx;

  generic_ptr
  read(std::string const& expr) {
    return insider::read(ctx, expr);
  }

  generic_ptr
  eval(std::string const& expr) {
    module m{ctx};
    import_all(ctx, m, ctx.internal_module);
    auto f = compile_expression(ctx, read(expr), m);
    auto state = make_state(ctx, f);
    return run(state);
  }

  generic_ptr
  eval_module(std::string const& expr) {
    module m = compile_main_module(ctx, read_multiple(ctx, expr));
    return execute(ctx, m);
  }

  void
  add_library(std::string const& body) {
    ctx.load_library_module(read_multiple(ctx, body));
  }

  bool
  num_equal(generic_ptr const& lhs, generic_ptr const& rhs) {
    return arith_equal(ctx, lhs, rhs) == ctx.constants->t;
  }

  ptr<fraction>
  make_fraction(int n, int d) {
    return make<fraction>(ctx, make<integer>(ctx, n), make<integer>(ctx, d));
  }

  ptr<floating_point>
  make_float(double value) {
    return make<floating_point>(ctx, value);
  }
};

struct aaa : leaf_object<aaa> {
  bool* alive;

  explicit
  aaa(bool* alive) : alive{alive} { *alive = true; }

  aaa(aaa&& other) : alive{other.alive} { other.alive = nullptr; }

  ~aaa() {
    if (alive)
      *alive = false;
  }
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

struct bbb : composite_object<bbb> {
  bool* alive;
  object* child = nullptr;

  explicit
  bbb(bool* alive) : alive{alive} { *alive = true; }

  bbb(bbb&& other)
    : alive{other.alive}
    , child{other.child}
  {
    other.alive = nullptr;
  }

  ~bbb() {
    if (alive)
      *alive = false;
  }

  void
  trace(tracing_context& tc) {
    tc.trace(child);
  }

  void
  update_references() {
    update_reference(child);
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
  ptr<pair> p = make<pair>(ctx, ctx.constants->null, ctx.constants->null);
  generic_ptr x = p;
  generic_ptr null = ctx.constants->null;

  EXPECT_TRUE(is<pair>(x));
  EXPECT_FALSE(is<pair>(null));
  EXPECT_TRUE(expect<pair>(x) == p);
  EXPECT_THROW(expect<pair>(null), error);

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
  EXPECT_FALSE(is_list(l1));

  // (1 2)
  ptr<pair> l2 = make<pair>(ctx,
                            make<integer>(ctx, 1),
                            make<pair>(ctx,
                                       make<integer>(ctx, 2),
                                       ctx.constants->null));
  EXPECT_TRUE(is_list(l2));

  // (0 1 2)
  ptr<pair> l3 = make<pair>(ctx, make<integer>(ctx, 0), l2);
  EXPECT_TRUE(is_list(l3));
}

TEST_F(scheme, make_list) {
  generic_ptr empty = make_list(ctx);
  EXPECT_TRUE(empty == ctx.constants->null);

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

  EXPECT_EQ(cdr(third), ctx.constants->null);
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
  vector_set(v1, 0, make<integer>(ctx, 1));
  vector_set(v1, 1, make<integer>(ctx, 2));
  vector_set(v1, 2, make<integer>(ctx, 3));

  EXPECT_EQ(v1->size(), 3u);

  EXPECT_EQ(expect<integer>(vector_ref(v1, 0))->value(), 1);
  EXPECT_EQ(expect<integer>(vector_ref(v1, 1))->value(), 2);
  EXPECT_EQ(expect<integer>(vector_ref(v1, 2))->value(), 3);

  EXPECT_THROW(vector_ref(v1, 3), std::runtime_error);
  EXPECT_THROW(vector_set(v1, 4, make<integer>(ctx, 4)), std::runtime_error);

  ptr<vector> v2 = make<vector>(ctx, 2);
  bool one{}, two{};
  vector_set(v2, 0, make<aaa>(ctx, &one));
  vector_set(v2, 1, make<aaa>(ctx, &two));

  EXPECT_TRUE(one);
  EXPECT_TRUE(two);

  v2.reset();
  ctx.store.collect_garbage();

  EXPECT_FALSE(one);
  EXPECT_FALSE(two);
}

TEST_F(scheme, read_small_integer) {
  EXPECT_EQ(expect<integer>(read("0"))->value(), 0);
  EXPECT_EQ(expect<integer>(read("2"))->value(), 2);
  EXPECT_EQ(expect<integer>(read("-2"))->value(), -2);
  EXPECT_EQ(expect<integer>(read("+215"))->value(), +215);
  EXPECT_EQ(expect<integer>(read("-3812"))->value(), -3812);
  EXPECT_EQ(expect<integer>(read("4611686018427387903"))->value(), 4611686018427387903);
  EXPECT_EQ(expect<integer>(read("4611686018427387902"))->value(), 4611686018427387902);
  EXPECT_EQ(expect<integer>(read("-4611686018427387903"))->value(), -4611686018427387903);
  EXPECT_EQ(expect<integer>(read("-4611686018427387902"))->value(), -4611686018427387902);
}

TEST_F(scheme, read_list) {
  generic_ptr empty_1 = read("()");
  EXPECT_EQ(empty_1, ctx.constants->null);

  generic_ptr empty_2 = read("(   )");
  EXPECT_EQ(empty_2, ctx.constants->null);

  generic_ptr single_element = read("(1)");
  EXPECT_TRUE(is_list(single_element));
  EXPECT_EQ(expect<integer>(car(expect<pair>(single_element)))->value(), 1);
  EXPECT_EQ(cdr(expect<pair>(single_element)), ctx.constants->null);

  generic_ptr two_elements = read("(1 2)");
  EXPECT_TRUE(is_list(two_elements));
  EXPECT_EQ(expect<integer>(car(expect<pair>(two_elements)))->value(), 1);
  EXPECT_EQ(expect<integer>(car(expect<pair>(cdr(expect<pair>(two_elements)))))->value(), 2);
  EXPECT_EQ(cdr(expect<pair>(cdr(expect<pair>(two_elements)))), ctx.constants->null);

  generic_ptr no_elements = read("()");
  EXPECT_EQ(no_elements, ctx.constants->null);

  generic_ptr nested = read("(1 (2 3))");
  EXPECT_TRUE(is_list(nested));
  EXPECT_EQ(expect<integer>(car(expect<pair>(nested)))->value(), 1);
  EXPECT_TRUE(is_list(car(expect<pair>(cdr(expect<pair>(nested))))));
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
  EXPECT_EQ(read("if"), ctx.intern("if"));
  EXPECT_EQ(read("..."), ctx.intern("..."));
  EXPECT_EQ(read(".!"), ctx.intern(".!"));
  EXPECT_EQ(read(".dot"), ctx.intern(".dot"));

  generic_ptr l = read("(one two three)");
  ASSERT_TRUE(is_list(l));
  EXPECT_EQ(expect<symbol>(car(expect<pair>(l)))->value(), "one");
  EXPECT_EQ(expect<symbol>(car(expect<pair>(cdr(expect<pair>(l)))))->value(), "two");
  EXPECT_EQ(expect<symbol>(car(expect<pair>(cdr(expect<pair>(cdr(expect<pair>(l)))))))->value(), "three");
}

TEST_F(scheme, read_string) {
  EXPECT_EQ(expect<string>(read(R"("foo")"))->value(), "foo");
  EXPECT_EQ(expect<string>(read(R"("one\ntwo")"))->value(), "one\ntwo");
  char const* msvc_workaround1 = R"("this \"is\" a quote")";
  EXPECT_EQ(expect<string>(read(msvc_workaround1))->value(), "this \"is\" a quote");
  char const* msvc_workaround2 = R"("foo\"bar\"baz")";
  EXPECT_EQ(expect<string>(read(msvc_workaround2))->value(), "foo\"bar\"baz");

  EXPECT_THROW(read(R"("unterminated)"), parse_error);
  char const* msvc_workaround3 = R"("\invalid escape")";
  EXPECT_THROW(read(msvc_workaround3), parse_error);
  char const* msvc_workaround4 = R"("\)";
  EXPECT_THROW(read(msvc_workaround4), parse_error);
  char const* msvc_workaround5 = R"("\")";
  EXPECT_THROW(read(msvc_workaround5), parse_error);
}

TEST_F(scheme, read_multiple) {
  std::vector<generic_ptr> result1 = read_multiple(ctx, "foo bar baz");
  ASSERT_EQ(result1.size(), 3);
  EXPECT_EQ(expect<symbol>(result1[0])->value(), "foo");
  EXPECT_EQ(expect<symbol>(result1[1])->value(), "bar");
  EXPECT_EQ(expect<symbol>(result1[2])->value(), "baz");

  std::vector<generic_ptr> result2 = read_multiple(ctx, "(foo) (bar 2)");
  ASSERT_EQ(result2.size(), 2);
  EXPECT_TRUE(is_list(result2[0]));
  EXPECT_EQ(list_length(result2[0]), 1);

  EXPECT_TRUE(is_list(result2[1]));
  EXPECT_EQ(list_length(result2[1]), 2);
}

TEST_F(scheme, read_comments) {
  EXPECT_EQ(expect<integer>(read(R"(;; Comment
                                    2)"))->value(),
            2);
  EXPECT_EQ(expect<integer>(read("7 ;; A prime number"))->value(), 7);
  EXPECT_EQ(expect<string>(read(R"("foo;bar;baz" ; string)"))->value(), "foo;bar;baz");
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
      (let ((a 2)
            (b 5))
        (let ((sum (+ a b))
              (product (* a b)))
          (- sum product)))
    )"
  );

  int a = 2;
  int b = 5;
  int sum = a + b;
  int product = a * b;
  EXPECT_EQ(expect<integer>(result)->value(), sum - product);

  EXPECT_THROW(compile_expression(ctx, read("(let ((a 2)))"), ctx.internal_module),
               std::runtime_error);
  EXPECT_THROW(compile_expression(ctx, read("(let foo)"), ctx.internal_module),
               std::runtime_error);
}

TEST_F(scheme, let_shadowing) {
  generic_ptr result = eval(
    R"(
      (let ((a 2))
        (let ((a 5))
          a))
    )"
  );
  EXPECT_EQ(expect<integer>(result)->value(), 5);
}

TEST_F(scheme, compile_lambda) {
  generic_ptr result1 = eval(
    R"(
      (let ((twice (lambda (x) (* 2 x))))
        (twice 4))
    )"
  );
  EXPECT_EQ(expect<integer>(result1)->value(), 8);

  generic_ptr result2 = eval(
    R"(
      (let ((sum (lambda (a b c d) (+ a b c d))))
        (sum 1 2 3 4))
    )"
  );
  EXPECT_EQ(expect<integer>(result2)->value(), 1 + 2 + 3 + 4);

  generic_ptr result3 = eval(
    R"(
      (let ((call-with-sum (lambda (f a b) (f (+ a b))))
            (f (lambda (x) (* 2 x))))
        (call-with-sum f 3 4))
    )"
  );
  EXPECT_EQ(expect<integer>(result3)->value(), 2 * (3 + 4));
}

TEST_F(scheme, compile_if) {
  generic_ptr result1 = eval("(if #t 2 3)");
  EXPECT_EQ(expect<integer>(result1)->value(), 2);

  generic_ptr result2 = eval("(if #f 2 3)");
  EXPECT_EQ(expect<integer>(result2)->value(), 3);

  generic_ptr result3 = eval("(if #t 2)");
  EXPECT_EQ(expect<integer>(result3)->value(), 2);

  generic_ptr result4 = eval("(if #f 2)");
  EXPECT_EQ(result4, ctx.constants->void_);

  generic_ptr result5 = eval(
    R"(
      (let ((f (lambda (x) (* 2 x)))
            (x 4))
        (if (< x 5)
              (f x)
              0))
    )"
  );
  EXPECT_EQ(expect<integer>(result5)->value(), 8);

  generic_ptr result6 = eval(
    R"(
      (let ((f (lambda (x) (* 2 x)))
            (x 6))
        (if (< x 5)
              (f x)
              0))
    )"
  );
  EXPECT_EQ(expect<integer>(result6)->value(), 0);;

  generic_ptr result7 = eval(
    R"(
      (let ((f (lambda (x) (* 2 x)))
            (x 4))
        (if (< x 5)
              0
              (f x)))
    )"
  );
  EXPECT_EQ(expect<integer>(result7)->value(), 0);

  generic_ptr result8 = eval(
    R"(
      (let ((f (lambda (x) (* 2 x)))
            (x 6))
        (if (< x 5)
              0
              (f x)))
    )"
  );
  EXPECT_EQ(expect<integer>(result8)->value(), 12);

  generic_ptr result9 = eval(
    R"(
      (let ((f (lambda (x) (* 2 x)))
            (g (lambda (x) (+ 2 x)))
            (x 4))
        (if (< x 5)
              (f x)
              (g x)))
    )"
  );
  EXPECT_EQ(expect<integer>(result9)->value(), 8);

  generic_ptr result10 = eval(
    R"(
      (let ((f (lambda (x) (* 2 x)))
            (g (lambda (x) (+ 10 x)))
            (x 6))
        (if (< x 5)
              (f x)
              (g x)))
    )"
  );
  EXPECT_EQ(expect<integer>(result10)->value(), 16);
}

TEST_F(scheme, compile_closure) {
  generic_ptr result1 = eval(
    R"(
      (let ((make-adder (lambda (x) (lambda (y) (+ x y)))))
        (let ((add-2 (make-adder 2)))
          (add-2 5)))
    )"
  );
  EXPECT_EQ(expect<integer>(result1)->value(), 7);

  generic_ptr result2 = eval(
    R"(
      (let ((x 7))
        (let ((f (lambda (y) (+ x y))))
          (f 3)))
    )"
  );
  EXPECT_EQ(expect<integer>(result2)->value(), 10);
}

TEST_F(scheme, compile_set) {
  generic_ptr result1 = eval(
    R"(
      (let ((x 2))
        (set! x 5)
        x)
    )"
  );
  EXPECT_EQ(expect<integer>(result1)->value(), 5);

  generic_ptr result2 = eval(
    R"(
      (let ((fact #void))
        (set! fact (lambda (n)
                     (if (= n 0)
                       1
                       (* n (fact (- n 1))))))
        (fact 5))
    )"
  );
  EXPECT_EQ(expect<integer>(result2)->value(), 120);

  generic_ptr result3 = eval(
    R"(
      (let ((f (lambda (x)
                 (set! x (* 2 x))
                 (lambda (y)
                   (+ x y)))))
        ((f 5) 3))
    )"
  );
  EXPECT_EQ(expect<integer>(result3)->value(), 13);
}

TEST_F(scheme, compile_box) {
  generic_ptr result = eval(
    R"(
      (let ((b1 (box 5))
            (b2 (box 7)))
        (box-set! b1 (+ (unbox b1) (unbox b2)))
        (unbox b1))
    )"
  );
  EXPECT_EQ(expect<integer>(result)->value(), 12);
}

TEST_F(scheme, compile_sequence) {
  generic_ptr result = eval(R"(
    (let ((a 0)
          (b 0))
      (if #t
          (begin
            (set! a 1)
            (set! b 2))
          'unpossible)
      (+ a b))
  )");
  EXPECT_EQ(expect<integer>(result)->value(), 3);
}

TEST_F(scheme, compile_higher_order_arithmetic) {
  generic_ptr result = eval(
    R"(
      (let ((f (lambda (op x y) (op x y))))
        (f + 2 3))
    )"
  );
  EXPECT_EQ(expect<integer>(result)->value(), 5);
}

TEST_F(scheme, compile_module) {
  int sum = 0;
  define_top_level(
    ctx, ctx.internal_module, "f",
    make<native_procedure>(ctx,
                           [&] (context& ctx, std::vector<generic_ptr> const& args) {
                             sum += expect<integer>(args[0])->value();
                             return ctx.constants->void_;
                           }),
    true
  );

  auto m = compile_main_module(ctx,
                               read_multiple(ctx,
                                             "(import (insider internal))"
                                             "(f 3)"
                                             "(let ((x 2))"
                                             "  (f x))"));
  auto state = make_state(ctx, m.top_level_procedure());
  run(state);

  EXPECT_EQ(sum, 5);
}

TEST_F(scheme, compile_top_level_define) {
  auto result1 = eval_module(
    R"(
      (import (insider internal))
      (define f
        (lambda (x)
          (+ x 2)))
      (define var 7)
      (f var)
    )"
  );
  EXPECT_EQ(expect<integer>(result1)->value(), 9);

  auto result2 = eval_module(
    R"(
      (import (insider internal))
      (define x 4)
      (define y 7)
      (set! x (+ y 2))
      x
    )"
  );
  EXPECT_EQ(expect<integer>(result2)->value(), 9);

  auto result3 = eval_module(R"(
    (import (insider internal))

    (define a 1)
    (begin
      (define b 2)
      (define c 3)
      (begin
        (define d 4)
        (define e 5))
      (define f 6))
    (define g 7)
    (+ a b c d e f g)
  )");
  EXPECT_EQ(expect<integer>(result3)->value(), 1 + 2 + 3 + 4 + 5 + 6 + 7);
}

TEST_F(scheme, compile_internal_define) {
  auto result1 = eval_module(
    R"(
      (import (insider internal))

      (define f
        (lambda (n)
          (define go
            (lambda (k accum)
              (if (= k 0)
                accum
                (go (- k 1) (+ accum k)))))
          (go n 0)))
      (f 5)
    )"
  );
  EXPECT_EQ(expect<integer>(result1)->value(), 5 + 4 + 3 + 2 + 1);

  auto result2 = eval_module(R"(
    (import (insider internal))

    (define f
      (lambda (x y)
        (define sum (+ x y))
        (begin
          (define product (* x y))
          (define sum-of-squares (+ (* x x) (* y y))))
        (+ sum product sum-of-squares)))
    (f 4 5)
  )");
  EXPECT_EQ(expect<integer>(result2)->value(), 4 + 5 + 4 * 5 + 4 * 4 + 5 * 5);
}

TEST_F(scheme, define_lambda) {
  define_lambda<int(int, int)>(
    ctx, ctx.internal_module, "f", true,
    [] (int a, int b) { return 2 * a + b; }
  );

  int x = 0;
  define_lambda<void(int)>(
    ctx, ctx.internal_module, "g", true,
    [&] (int a) { x += a; }
  );

  define_lambda<std::string(int)>(
    ctx, ctx.internal_module, "to-string", true,
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
  vector_set(v, 0, make<character>(ctx, 'r'));
  vector_set(v, 1, p2);
  vector_set(v, 2, make_string(ctx, "foobar"));
  EXPECT_EQ(to_string(ctx, v), R"(#(#\r (0 1 . 2) "foobar"))");

  auto s = make_string(ctx, R"(one "two" three \ four)");
  char const* msvc_workaround1 = R"("one \"two\" three \\ four")";
  EXPECT_EQ(to_string(ctx, s), msvc_workaround1);

  auto l = make_list(
    ctx,
    ctx.constants->null,
    ctx.constants->void_,
    ctx.constants->t,
    ctx.constants->f,
    ctx.intern("symbol"),
    make_string(ctx, "string"),
    make<character>(ctx, 'c'),
    make<integer>(ctx, -13)
  );
  EXPECT_EQ(to_string(ctx, l), R"((() #void #t #f symbol "string" #\c -13))");
}

TEST_F(scheme, quote) {
  auto result1 = eval("(quote (a b c))");
  EXPECT_TRUE(is_list(result1));
  EXPECT_EQ(list_length(result1), 3);

  auto result2 = eval("(quote 2)");
  EXPECT_EQ(expect<integer>(result2)->value(), 2);

  auto result3 = eval("'3");
  EXPECT_EQ(expect<integer>(result3)->value(), 3);

  auto result4 = eval("'(a b)");
  EXPECT_TRUE(is_list(result4));
  EXPECT_EQ(list_length(result4), 2);
  EXPECT_EQ(expect<symbol>(car(expect<pair>(result4)))->value(), "a");
  EXPECT_EQ(expect<symbol>(cadr(expect<pair>(result4)))->value(), "b");

  auto result5 = eval("''a");
  EXPECT_TRUE(is_list(result5));
  EXPECT_EQ(list_length(result5), 2);
  EXPECT_EQ(expect<symbol>(car(expect<pair>(result5)))->value(), "quote");
  EXPECT_EQ(expect<symbol>(cadr(expect<pair>(result5)))->value(), "a");
}

TEST_F(scheme, equal) {
  EXPECT_TRUE(equal(read("1"), read("1")));
  EXPECT_FALSE(equal(read("1"), read("2")));
  EXPECT_FALSE(equal(read("1"), read("sym")));
  EXPECT_TRUE(equal(read("'(1 2)"), read("'(1 2)")));
  EXPECT_TRUE(equal(read("'(1 2)"), read("(quote (1 2))")));
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

  auto result3 = eval("(let ((a 7)) `(1 ,a 3))");
  EXPECT_TRUE(equal(result3, read("(1 7 3)")));

  auto result4 = eval("`(1 ,(+ 2 3) 3)");
  EXPECT_TRUE(equal(result4, read("(1 5 3)")));

  auto result5 = eval("(let ((name 'a)) `(list ,name ',name))");
  EXPECT_TRUE(equal(result5, read("(list a (quote a))")));

  auto result6 = eval("`#(1 2 5)");
  EXPECT_TRUE(equal(result6, read("#(1 2 5)")));

  auto result7 = eval("(let ((a 12)) `#(3 ,a 5 ,(* a 2) 9))");
  EXPECT_TRUE(equal(result7, read("#(3 12 5 24 9)")));

  auto result8 = eval("(let ((b '(b1 b2 b3))) `(a1 a2 ,@b c1 c2))");
  EXPECT_TRUE(equal(result8, read("(a1 a2 b1 b2 b3 c1 c2)")));

  auto result9 = eval("(let ((b '(b1 b2 b3))) `(a1 a2 ,b c1 c2))");
  EXPECT_TRUE(equal(result9, read("(a1 a2 (b1 b2 b3) c1 c2)")));

  auto result10 = eval("(let ((b '(b1 b2))) `(a1 a2 ,@b))");
  EXPECT_TRUE(equal(result10, read("(a1 a2 b1 b2)")));

  auto result11 = eval("``(a b ,c)");
  EXPECT_TRUE(equal(result11, read("(quasiquote (a b (unquote c)))")));

  auto result12 = eval("(let ((b '(b1 b2 b3))) `#(a1 a2 ,@b c1 c2))");
  EXPECT_TRUE(equal(result12, read("#(a1 a2 b1 b2 b3 c1 c2)")));

  auto result13 = eval("(let ((b '(b1 b2 b3))) `#(a1 a2 ,b c1 c2))");
  EXPECT_TRUE(equal(result13, read("#(a1 a2 (b1 b2 b3) c1 c2)")));

  auto result14 = eval("(let ((b '(a1 a2))) `#(,@b b1 b2 b3))");
  EXPECT_TRUE(equal(result14, read("#(a1 a2 b1 b2 b3)")));

  auto result15 = eval("(let ((b '(b1 b2))) `#(a1 a2 ,@b))");
  EXPECT_TRUE(equal(result15, read("#(a1 a2 b1 b2)")));

  auto result16 = eval("(let ((b '(b1 b2))) ``(a1 a2 ,b c1 c2 ,(d1 d2 ,b e1 e2)))");
  EXPECT_TRUE(equal(result16, read("(quasiquote (a1 a2 (unquote b) c1 c2 (unquote (d1 d2 (b1 b2) e1 e2))))")));

  auto result17 = eval("(let ((x '(x1 x2))) `(,@x . y))");
  EXPECT_TRUE(equal(result17, read("(x1 x2 . y)")));

  auto result18 = eval("(let ((x '(x1 x2))) `(a1 a2 ,@x . y))");
  EXPECT_TRUE(equal(result18, read("(a1 a2 x1 x2 . y)")));

  auto result19 = eval("(let ((x '(x1 x2))) `(,@x))");
  EXPECT_TRUE(equal(result19, read("(x1 x2)")));
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
  EXPECT_TRUE(equal(r5, ctx.constants->null));

  auto r6 = eval("(append '() '(a1 a2))");
  EXPECT_TRUE(equal(r6, read("(a1 a2)")));

  auto r7 = eval("(append '() '(a1 a2) '() '() '(b1 b2 b3))");
  EXPECT_TRUE(equal(r7, read("(a1 a2 b1 b2 b3)")));

  auto r8 = eval("(append '() '(a1 a2) '() '() '(b1 b2 b3) '())");
  EXPECT_TRUE(equal(r8, read("(a1 a2 b1 b2 b3)")));
}

TEST_F(scheme, call_from_native) {
  auto f = expect<procedure>(eval("(lambda (x y) (+ (* 2 x) (* 3 y)))"));
  generic_ptr result = call(ctx, f, {make<integer>(ctx, 5), make<integer>(ctx, 4)});
  EXPECT_EQ(expect<integer>(result)->value(), 2 * 5 + 3 * 4);

  scheme_procedure<int(int, int)> g{eval("(lambda (x y) (+ (* 2 x) (* 3 y)))")};
  EXPECT_EQ(g(ctx, 5, 4), 2 * 5 + 3 * 4);
}

TEST_F(scheme, top_level_transformers) {
  auto result1 = eval_module(R"(
    (import (insider internal))
    (define-syntax num (lambda (x transformer-env usage-env) 4))
    (* (num) 2)
  )");
  EXPECT_EQ(expect<integer>(result1)->value(), 8);

  auto result2 = eval_module(R"(
    (import (insider internal))
    (define-syntax when
      (lambda (datum transformer-env usage-env)
        (let ((test (car (cdr datum)))
              (body (cdr (cdr datum))))
          `(if ,test ((lambda () ,@body)) #f))))

    (define value 4)
    (cons (when (< value 5) (* value 10))
          (when (> value 5) (* value 20)))
  )");
  auto result2p = expect<pair>(result2);
  EXPECT_EQ(expect<integer>(car(result2p))->value(), 40);
  EXPECT_EQ(cdr(result2p), ctx.constants->f);
}

TEST_F(scheme, internal_transformers) {
  auto result1 = eval_module(R"(
    (import (insider internal))
    (define foo
      (lambda (x)
        (define-syntax double
          (lambda (datum transformer-env usage-env)
            (let ((var (car (cdr datum))))
              `(* 2 ,var))))
        (+ x (double x))))
    (foo 5)
  )");
  EXPECT_EQ(expect<integer>(result1)->value(), 5 + 2 * 5);
}

TEST_F(scheme, core_shadowing) {
  auto result1 = eval("(let ((let 'let)) let)");
  EXPECT_EQ(expect<symbol>(result1)->value(), "let");

  auto result2 = eval("(let ((unquote 'x)) `(1 ,2 3))");
  EXPECT_TRUE(equal(result2, read("(1 (unquote 2) 3)")));
}

TEST_F(scheme, opaque_value) {
  define_lambda<opaque_value<int>(context&)>(
    ctx, ctx.internal_module, "make-value", true,
    [] (context& ctx) { return make<opaque_value<int>>(ctx, 7); }
  );
  auto result = eval("(make-value)");
  EXPECT_EQ(expect<opaque_value<int>>(result)->value, 7);
}

TEST_F(scheme, sc_transformer) {
  auto result1 = eval_module(R"(
    (import (insider internal))

    (define-syntax run
      (lambda (form transformer-env usage-env)
        (let ((expr (make-syntactic-closure usage-env '() (car (cdr form)))))
          (make-syntactic-closure transformer-env '()
                                  `(let ((value 4))
                                     ,expr)))))

    (let ((value 10))
      (run
        value))
  )");
  EXPECT_EQ(expect<integer>(result1)->value(), 10);

  auto result2 = eval_module(R"(
    (import (insider internal))

    (define-syntax run
      (lambda (form transformer-env usage-env)
        (let ((expr (make-syntactic-closure usage-env '(value) (car (cdr form)))))
          (make-syntactic-closure transformer-env '()
                                  `(let ((value 4))
                                     ,expr)))))

    (let ((value 10))
      (run
        value))
  )");
  EXPECT_EQ(expect<integer>(result2)->value(), 4);
}

TEST_F(scheme, transformers_producing_definitions) {
  auto result1 = eval_module(R"(
    (import (insider internal))

    (define-syntax define-double
      (lambda (expr transformer-env usage-env)
        (let ((name (make-syntactic-closure usage-env '() (car (cdr expr))))
              (value (make-syntactic-closure usage-env '() (car (cdr (cdr expr))))))
          (make-syntactic-closure transformer-env '()
                                  `(define ,name (* 2 ,value))))))

    (define f
      (lambda (x y)
        (define sum (+ x y))
        (define-double result (* 3 sum))
        result))
    (f 4 5)
  )");
  EXPECT_EQ(expect<integer>(result1)->value(), 2 * 3 * (4 + 5));

  auto result2 = eval_module(R"(
    (import (insider internal))

    (define-syntax define-two
      (lambda (expr transformer-env usage-env)
        (let ((name-1 (make-syntactic-closure usage-env '() (car (cdr expr))))
              (init-1 (make-syntactic-closure usage-env '() (car (cdr (cdr expr)))))
              (name-2 (make-syntactic-closure usage-env '() (car (cdr (cdr (cdr expr))))))
              (init-2 (make-syntactic-closure usage-env '() (car (cdr (cdr (cdr (cdr expr))))))))
          (make-syntactic-closure transformer-env '()
            `(begin
               (define ,name-1 ,init-1)
               (define ,name-2 ,init-2))))))

    (define f
      (lambda (x y)
        (define-two twice-x (* 2 x)
                    twice-y (* 2 y))
        (+ twice-x twice-y)))

    (f 4 5)
  )");
  EXPECT_EQ(expect<integer>(result2)->value(), 2 * 4 + 2 * 5);

  auto result3 = eval_module(R"(
    (import (insider internal))


    (define-syntax define-two
      (lambda (expr transformer-env usage-env)
        (let ((name-1 (make-syntactic-closure usage-env '() (car (cdr expr))))
              (init-1 (make-syntactic-closure usage-env '() (car (cdr (cdr expr)))))
              (name-2 (make-syntactic-closure usage-env '() (car (cdr (cdr (cdr expr))))))
              (init-2 (make-syntactic-closure usage-env '() (car (cdr (cdr (cdr (cdr expr))))))))
          (make-syntactic-closure transformer-env '()
            `(begin
               (define ,name-1 ,init-1)
               (define ,name-2 ,init-2))))))

    (define-two a 7
                b 12)
    (+ a b)
  )");
  EXPECT_EQ(expect<integer>(result3)->value(), 7 + 12);
}

TEST_F(scheme, module_activation) {
  std::vector<int> trace;
  define_lambda<void(int)>(
    ctx, ctx.internal_module, "leave-mark", true,
    [&] (int value) { trace.push_back(value); }
  );

  add_library(R"(
    (library (foo))
    (import (insider internal))

    (leave-mark 1)
  )");
  EXPECT_TRUE(trace.empty());

  add_library(R"(
    (library (bar))
    (import (insider internal))
    (import (foo))

    (leave-mark 2)
  )");
  EXPECT_TRUE(trace.empty());

  eval_module(R"(
    (import (insider internal))
    (import (bar))

    (leave-mark 3)
  )");
  EXPECT_EQ(trace, (std::vector{1, 2, 3}));
}

TEST_F(scheme, module_variable_export) {
  add_library(R"(
    (library (foo))
    (import (insider internal))
    (export foo)
    (export exported)

    (define foo
      (lambda (x)
        (* 2 x)))

    (define exported 2)
    (define not-exported 3)
  )");

  auto result = eval_module(R"(
    (import (foo))
    (foo 3)
  )");
  EXPECT_EQ(expect<integer>(result)->value(), 6);

  EXPECT_EQ(expect<integer>(eval_module("(import (foo)) exported"))->value(), 2);
  EXPECT_THROW(eval_module("(import (foo)) not-exported"), std::runtime_error);
}

TEST_F(scheme, module_syntax_export) {
  add_library(R"(
    (library (foo))
    (import (insider internal))
    (export double)

    (define-syntax double
      (lambda (form transformer-env usage-env)
        (let ((value (car (cdr form))))
          `(* 2 ,value))))
  )");

  auto result1 = eval_module(R"(
    (import (foo))
    (import (insider internal))

    (double 3)
  )");
  EXPECT_EQ(expect<integer>(result1)->value(), 6);

  add_library(R"(
    (library (bar))
    (import (insider internal))
    (export get-var)

    (define var 7)
    (define-syntax get-var
      (lambda (form transformer-env usage-env)
        (make-syntactic-closure transformer-env '() 'var)))
  )");
  auto result2 = eval_module(R"(
    (import (bar))
    (import (insider internal))

    (define var 3)
    (get-var)
  )");
  EXPECT_EQ(expect<integer>(result2)->value(), 7);

  add_library(R"(
    (library (baz))
    (import (insider internal))
    (export get-value)

    (define value 14)
    (define-syntax get-value
      (lambda (form transformer-env usage-env)
        value))
  )");
  auto result3 = eval_module(R"(
    (import (baz))
    (get-value)
  )");
  EXPECT_EQ(expect<integer>(result3)->value(), 14);
}

TEST_F(scheme, import_specifiers) {
  add_library(R"(
    (library (foo))
    (import (insider internal))
    (export a b c d e)
    (define a 1)
    (define b 2)
    (define c 3)
    (define d 4)
    (define e 5)
  )");

  auto result1 = eval_module(R"(
    (import (insider internal)
            (only (foo) a b))
    (+ a b)
  )");
  EXPECT_EQ(expect<integer>(result1)->value(), 1 + 2);

  EXPECT_THROW(eval_module("(import (insider internal) (only (foo) a b)) (+ a b c)"),
               std::runtime_error);

  auto result2 = eval_module(R"(
    (import (insider internal)
            (except (foo) a b))
    (+ c d e)
  )");
  EXPECT_EQ(expect<integer>(result2)->value(), 3 + 4 + 5);

  EXPECT_THROW(eval_module("(import (insider internal) (except (foo) a b)) (+ a b c d e)"),
               std::runtime_error);

  auto result3 = eval_module(R"(
    (import (insider internal)
            (prefix (foo) foo:))
    (+ foo:a foo:b foo:c foo:d foo:e)
  )");
  EXPECT_EQ(expect<integer>(result3)->value(), 1 + 2 + 3 + 4 + 5);

  auto result4 = eval_module(R"(
    (import (insider internal)
            (rename (foo)
                    (a first)
                    (b second)
                    (c third)
                    (d fourth)
                    (e fifth)))
    (+ first second third fourth fifth)
  )");
  EXPECT_EQ(expect<integer>(result4)->value(), 1 + 2 + 3 + 4 + 5);

  auto result5 = eval_module(R"(
    (import (insider internal)
            (prefix (only (foo) a b) foo:))
    (+ foo:a foo:b)
  )");
  EXPECT_EQ(expect<integer>(result5)->value(), 1 + 2);
}

using limb_type = big_integer::limb_type;
using limb_vector = std::vector<limb_type>;
constexpr std::uintmax_t limb_max = std::numeric_limits<limb_type>::max();

void
convert_limbs(limb_vector&) { }

template <typename Limb, typename... Limbs>
void
convert_limbs(limb_vector& limbs, Limb first, Limbs... rest) {
  if constexpr (sizeof(Limb) <= sizeof(limb_type)) {
    limbs.push_back(first);
    convert_limbs(limbs, rest...);
  } else if constexpr (sizeof(Limb) == 2 * sizeof(limb_type)) {
    Limb mask = (Limb{1} << std::numeric_limits<limb_type>::digits) - 1;
    limbs.push_back(first & mask);
    limb_type hi = first >> std::numeric_limits<limb_type>::digits;
    if (hi > 0 || sizeof...(rest) > 0)
      limbs.push_back(hi);
    convert_limbs(limbs, rest...);
  } else
    static_assert(sizeof(Limb) == 0, "Unimplemented");
}

template <typename... Limbs>
ptr<big_integer>
make_big(context& ctx, Limbs... limbs) {
  limb_vector ls;
  convert_limbs(ls, static_cast<std::uint64_t>(limbs)...);
  return make<big_integer>(ctx, ls, true);
}

ptr<big_integer>
make_big_literal(context& ctx, limb_vector limbs) {
  return make<big_integer>(ctx, std::move(limbs), true);
}

template <typename... Limbs>
ptr<big_integer>
make_big_negative(context& ctx, Limbs... limbs) {
  limb_vector ls;
  convert_limbs(ls, static_cast<std::uint64_t>(limbs)...);
  return make<big_integer>(ctx, ls, false);
}

ptr<big_integer>
make_big_negative_literal(context& ctx, limb_vector limbs) {
  return make<big_integer>(ctx, std::move(limbs), false);
}

TEST_F(scheme, bignum_add_subtract) {
  auto make_small = [&] (integer::value_type v) {
    return make<integer>(ctx, v);
  };

#define TEST_ADD1(lhs, rhs, result) EXPECT_TRUE(num_equal(add(ctx, lhs, rhs), result))

  TEST_ADD1(make_big_literal(ctx, {limb_max}), make_big_literal(ctx, {1}), make_big_literal(ctx, {0, 1}));
  TEST_ADD1(make_big_literal(ctx, {limb_max}), make_big_literal(ctx, {5}), make_big_literal(ctx, {4, 1}));
  TEST_ADD1(make_big(ctx, 3, 2, 1), make_big(ctx, 6, 5, 4), make_big(ctx, 9, 7, 5));
  TEST_ADD1(make_big(ctx, 0), make_big(ctx, 17), make_big(ctx, 17));
  TEST_ADD1(make_big_literal(ctx, {limb_max, limb_max, limb_max}), make_small(1), make_big_literal(ctx, {0, 0, 0, 1}));
  TEST_ADD1(make_big_literal(ctx, {limb_max - 1}), make_small(1), make_big_literal(ctx, {limb_max}));
  TEST_ADD1(make_big_literal(ctx, {limb_max - 1}), make_small(2), make_big_literal(ctx, {0, 1}));
  TEST_ADD1(make_big_literal(ctx, {limb_max, 1}), make_small(1), make_big_literal(ctx, {0, 2}));
  TEST_ADD1(make_big(ctx, 1, 1, 1), make_big(ctx, 1), make_big(ctx, 2, 1, 1));
  TEST_ADD1(make_big_literal(ctx, {1, 1, 1}), make_big_literal(ctx, {limb_max}), make_big_literal(ctx, {0, 2, 1}));

#undef TEST_ADD1

#define TEST_ADD2(lhs, rhs, result) EXPECT_TRUE(num_equal(add(ctx, lhs, rhs), result))

  TEST_ADD2(make_small(integer::max), make_small(1), make_big(ctx, integer::max + 1));
  TEST_ADD2(make_small(integer::max / 2 + 1), make_small(integer::max / 2 + 1),
            make_big(ctx, 2 * (integer::max / 2 + 1)));
  TEST_ADD2(make_small(integer::min), make_small(-1), make_big_negative(ctx, -integer::min + 1));
  TEST_ADD2(make_big_negative_literal(ctx, {0, 1}), make_big_literal(ctx, {0, 2}), make_big_literal(ctx, {0, 1}));
  TEST_ADD2(make_big_literal(ctx, {0, 2}), make_big_negative_literal(ctx, {0, 1}), make_big_literal(ctx, {0, 1}));
  TEST_ADD2(make_big_negative(ctx, 0, 1), make_big_negative(ctx, 0, 2), make_big_negative(ctx, 0, 3));

#undef TEST_ADD2

#define TEST_SUB(lhs, rhs, result) EXPECT_TRUE(num_equal(subtract(ctx, lhs, rhs), result))

  TEST_SUB(make_big(ctx, 7), make_big(ctx, 5), make_big(ctx, 2));
  TEST_SUB(make_big(ctx, 5), make_big(ctx, 7), make_big_negative(ctx, 2));
  TEST_SUB(make_big(ctx, 0), make_big(ctx, 2), make_big_negative(ctx, 2));
  TEST_SUB(make_big_literal(ctx, {0, 1}), make_small(1), make_big_literal(ctx, {limb_max}));
  TEST_SUB(make_small(1), make_big_literal(ctx, {0, 1}), make_big_negative_literal(ctx, {limb_max}));
  TEST_SUB(make_small(integer::min), make_small(1), make_big_negative(ctx, -integer::min + 1));
  TEST_SUB(make_small(integer::max), make_small(-1), make_big(ctx, integer::max + 1));

#undef TEST_SUB
}

TEST_F(scheme, bignum_multiply) {
  auto make_small = [&] (integer::value_type v) {
    return make<integer>(ctx, v);
  };

#define TEST_MUL(lhs, rhs, result) EXPECT_TRUE(num_equal(multiply(ctx, lhs, rhs), result))

  TEST_MUL(make_big(ctx, 6), make_big(ctx, 4), make_big(ctx, 24));
  TEST_MUL(make_big(ctx, 3, 7), make_big(ctx, 2), make_big(ctx, 6, 14));
  TEST_MUL(make_big_literal(ctx, {limb_max / 2 + 2}), make_small(2), make_big_literal(ctx, {2, 1}));
  TEST_MUL(make_big(ctx, 0xAAAAAAAAAAAAAAAA), make_big(ctx, 0x5555555555555555),
           make_big(ctx, 2049638230412172402, 4099276460824344803));
  TEST_MUL(make_big_literal(ctx, {limb_max, limb_max, limb_max}),
           make_big_literal(ctx, {limb_max, limb_max, limb_max}),
           make_big_literal(ctx, {1ull, 0ull, 0ull, limb_max - 1, limb_max, limb_max}));
  TEST_MUL(make_big(ctx,
                    7553641000729792380ull,
                    5922650218298786245ull,
                    16162713787851717198ull,
                    10217089460051462907ull,
                    8909038635035976174ull,
                    6264544477583426584ull),
           make_big(ctx,
                    135006098906616526ull,
                    18228197287099656848ull,
                    16295224771980191197ull,
                    12041080681835578308ull,
                    12088442273849314669ull,
                    16369766287213198900ull),
           make_big(ctx,
                    17587943455618018760ull,
                    3661188350774644697ull,
                    18134422418394596149ull,
                    2811448761515084749ull,
                    9308728024445826184ull,
                    13579246687949292473ull,
                    14227902833484535106ull,
                    9980069117625531926ull,
                    17630642390782412258ull,
                    10715135489352511738ull,
                    2172624792098790866ull,
                    5559199422083740143ull));
  TEST_MUL(make_big(ctx,
                    4973457347152855529ull,
                    3974748182163407329ull,
                    12985577770049413009ull,
                    9076302685846177862ull,
                    738070451437927480ull,
                    15264537084396607285ull),
           make_big(ctx,
                    18446486188639752782ull,
                    3232568627881589338ull,
                    6067942679178015607ull,
                    5102215575270724457ull,
                    9073736952742515913ull,
                    1132841502366999848ull),
           make_big(ctx,
                    10907754461151885054ull,
                    17739271069039180747ull,
                    43421228727999234ull,
                    15577091673733669039ull,
                    16779690354161498684ull,
                    8434430878092964694ull,
                    5379906370098889887ull,
                    3835003850008530511ull,
                    10897188578878994922ull,
                    9109306920291096961ull,
                    4303885782369079147ull,
                    937417522275367995ull));
  TEST_MUL(make_big(ctx,
                    7198000039145371562ull,
                    1123912303584447440ull,
                    3980891719142245558ull,
                    8577746048298875858ull,
                    8311727073236976024ull,
                    17884054197143250996ull),
           make_big(ctx,
                    1188782814227785944ull,
                    2325270131132468069ull,
                    16644864265523681630ull,
                    1350302678026269136ull,
                    2920890536911698555ull,
                    14188063578955447128ull),
           make_big(ctx,
                    2755251443097077616ull,
                    3689811377661473058ull,
                    8877032773261469093ull,
                    3501002863683630417ull,
                    3353221143724972381ull,
                    12641577925382422259ull,
                    4772326402676056ull,
                    13270911720695667193ull,
                    1977996037122781734ull,
                    9541927886011288034ull,
                    10643155227105218129ull,
                    13755278274835822806ull));
  TEST_MUL(make_big(ctx,
                    14406138149647546023ull,
                    10014334381816322851ull,
                    11337790629485301035ull,
                    4430716325805898191ull,
                    194131313579415733ull,
                    15917838048752608414ull),
           make_big(ctx,
                    14581525398179601359ull,
                    16070399142870265493ull,
                    2186843138437186843ull,
                    4427149851635696604ull,
                    16869355141075854150ull,
                    7241713424351240708ull),
           make_big(ctx,
                    14269956649823456777ull,
                    12265344471916286582ull,
                    11559624670299181550ull,
                    17455721097159657918ull,
                    12556885120134179191ull,
                    10585635572377579879ull,
                    16816908444647058668ull,
                    15251982720436126217ull,
                    7735126288172378743ull,
                    2550247449105320530ull,
                    17493571027862026555ull,
                    6248930490047179004ull));
  TEST_MUL(make_small(limb_max / 8), make_small(16), make_big_literal(ctx, {limb_max - 15, 1}));
  TEST_MUL(make_small(limb_max / 8), make_small(-16), make_big_negative_literal(ctx, {limb_max - 15, 1}));
  TEST_MUL(make_small(-(limb_max / 8)), make_small(16), make_big_negative_literal(ctx, {limb_max - 15, 1}));
  TEST_MUL(make_small(-(limb_max / 8)), make_small(-16), make_big_literal(ctx, {limb_max - 15, 1}));

#undef TEST_MUL
}

TEST_F(scheme, bignum_divide) {
  auto make_small = [&] (integer::value_type v) {
    return make<integer>(ctx, v);
  };

  auto test_div = [&] (generic_ptr const& x, generic_ptr const& y,
                       generic_ptr const& quotient, generic_ptr const& remainder) {
    auto [q, r] = quotient_remainder(ctx, x, y);
    EXPECT_TRUE(num_equal(q, quotient));
    EXPECT_TRUE(num_equal(r, remainder));
  };

  test_div(make_big(ctx, 0, 0, 8), make_small(2), make_big(ctx, 0, 0, 4), make_small(0));
  test_div(make_big(ctx, 1, 0, 8), make_small(2), make_big(ctx, 0, 0, 4), make_small(1));
  test_div(make_big(ctx, 0, 0, 9), make_small(2), make_big(ctx, 0, 9223372036854775808ull, 4), make_small(0));
  test_div(make<big_integer>(ctx, limb_vector{1, 0, 0, limb_max - 1, limb_max, limb_max}),
           make<big_integer>(ctx, limb_vector{limb_max, limb_max, limb_max}),
           make<big_integer>(ctx, limb_vector{limb_max, limb_max, limb_max}),
           make_small(0));
  test_div(make<big_integer>(ctx, limb_vector{limb_max, limb_max}),
           make<big_integer>(ctx, limb_vector{2, limb_max}),
           make_small(1),
           make<big_integer>(ctx, limb_vector{limb_max - 2}));
  test_div(make_small(-5), make_small(2), make_small(-2), make_small(-1));
  test_div(make_small(5), make_small(-2), make_small(-2), make_small(1));
  test_div(make_small(-5), make_small(-2), make_small(2), make_small(-1));
  test_div(make_big(ctx,
                    14874543083359811318ull,
                    1935678593982463049ull,
                    13199980569319760333ull,
                    15410505428270988819ull,
                    10213726445258162469ull,
                    12395278556799578418ull,
                    7456403789386227136ull,
                    1076335716858975346ull,
                    10881479039968020671ull),
           make_big(ctx,
                    11194330546299954062ull,
                    8087252728553791077ull,
                    7865184323385124607ull,
                    2510237557858698741ull,
                    6408985391941973001ull,
                    11839946699381125055ull,
                    10961764771554828402ull,
                    6611077199486813791ull,
                    2901902384916741495ull),
           make_small(3),
           make_big(ctx,
                    18185039591879052364ull,
                    14567408555740193048ull,
                    8051171672873938126ull,
                    7879792754694892595ull,
                    9433514343141795082ull,
                    13768926606075306484ull,
                    11464597622140845160ull,
                    18136592265817637203ull,
                    2175771885217796184ull));

  test_div(make_big(ctx,
                    7692261422231040055ull,
                    15384495960622693763ull,
                    8772732661969891694ull,
                    2535977428667197115ull,
                    16174420715838619803ull,
                    3874780564069069863ull,
                    10705952965053044654ull,
                    8482589844543901272ull,
                    8164205199812095894ull),
           make_big(ctx,
                    7282122855066198906ull,
                    8675704914817105958ull,
                    8550653847546750756ull,
                    15565807313993354545ull,
                    8244894239515059345ull,
                    12359625180438533263ull,
                    6526454882856490956ull,
                    3550628387050239179ull,
                    4768174037124105332ull),
           make_big(ctx, 1ull),
           make_big(ctx,
                    410138567164841149ull,
                    6708791045805587805ull,
                    222078814423140938ull,
                    5416914188383394186ull,
                    7929526476323560457ull,
                    9961899457340088216ull,
                    4179498082196553697ull,
                    4931961457493662093ull,
                    3396031162687990562ull));

  test_div(make_big(ctx,
                    2446330396973426494ull,
                    14461425362529891049ull,
                    4366362775501768695ull,
                    4235920909984245928ull,
                    16769734018257183108ull,
                    15925757854589919735ull,
                    7341355787832885332ull,
                    10348492034933946425ull,
                    13731135294911915075ull),
           make_big(ctx,
                    13880604877705042050ull,
                    2611974763742920252ull,
                    12134750319276141604ull,
                    4688483100727472282ull,
                    18258228925628760636ull,
                    4887347349206545042ull,
                    4787504742638920873ull,
                    14685252780079427324ull,
                    9435378219307584956ull),
           make_small(1),
           make_big(ctx,
                    7012469592977936060ull,
                    11849450598786970796ull,
                    10678356529935178707ull,
                    17994181882966325261ull,
                    16958249166337974087ull,
                    11038410505383374692ull,
                    2553851045193964459ull,
                    14109983328564070717ull,
                    4295757075604330118ull));

  test_div(make_big(ctx,
                    12506852871317207676ull,
                    10700938666703083024ull,
                    12626603003370626477ull,
                    14261716924607286084ull,
                    2161370295798928489ull,
                    10122529076047113631ull,
                    16373459737971821544ull,
                    10311005268424454975ull,
                    1100495285425557724ull),
           make_big(ctx,
                    12465178309255449209ull,
                    1427445383266802948ull,
                    5756920205309277496ull,
                    7696573369463630995ull,
                    7566067571733492416ull,
                    13073773422555002889ull,
                    6551708513246522343ull,
                    17329835614592961126ull,
                    14847207217269943192ull),
           make_small(0),
           make_big(ctx,
                    12506852871317207676ull,
                    10700938666703083024ull,
                    12626603003370626477ull,
                    14261716924607286084ull,
                    2161370295798928489ull,
                    10122529076047113631ull,
                    16373459737971821544ull,
                    10311005268424454975ull,
                    1100495285425557724ull));

  test_div(make_big(ctx,
                    13768515950283912958ull,
                    5039599389847194458ull,
                    12107044372400000832ull,
                    134123983447138649ull,
                    2153789414069619898ull,
                    7910490151013994715ull,
                    16554460799871884867ull,
                    13004393950989746279ull,
                    17606722284703736159ull),
           make_big(ctx,
                    13528201411004224237ull,
                    9721197764555830598ull,
                    4888196661345244575ull,
                    2313115633983103010ull,
                    13500977888171292075ull,
                    5759984481056258966ull,
                    12748738408593844646ull,
                    16600874190120711725ull,
                    11498829053734232490ull),
           make_small(1),
           make_big(ctx,
                    240314539279688721ull,
                    13765145699000915476ull,
                    7218847711054756256ull,
                    16267752423173587255ull,
                    7099555599607879438ull,
                    2150505669957735748ull,
                    3805722391278040221ull,
                    14850263834578586170ull,
                    6107893230969503668ull));

  test_div(make_big(ctx,
                    6972646283730170953ull,
                    7519107045762773052ull,
                    4872916222398852026ull,
                    2189920864793289825ull,
                    1602617798560111769ull,
                    10146934531826883440ull,
                    9150028946074351450ull,
                    2209844256419692506ull,
                    2371063704493274143ull),
           make_big(ctx,
                    10108171535437529585ull,
                    1183060610282380979ull,
                    17553143743327730375ull,
                    2571022672643327199ull),
           make_big(ctx,
                    8518718272992650627ull,
                    15809441883104914544ull,
                    13990766892202295253ull,
                    4433784788950460276ull,
                    17012065200607766589ull),
           make_big(ctx,
                    5328558974349566198ull,
                    8640356290420700764ull,
                    7580956359732994985ull,
                    1045752718567588000ull));

  test_div(make_big(ctx,
                    10170782082354554675ull,
                    7960934355353996918ull,
                    5724955629544235019ull,
                    2930662393834309931ull,
                    8140058148974841898ull,
                    3338167932104888346ull,
                    8168977224709667766ull,
                    13173537970649653350ull,
                    5448979766364583280ull),
           make_big(ctx,
                    16552174762367217981ull,
                    17366052293534881471ull,
                    2156616536430823413ull,
                    13732808403092740360ull),
           make_big(ctx,
                    1368660787543327668ull,
                    1158922548137579624ull,
                    1206006948639191786ull,
                    16985513432125315283ull,
                    7319401266117724767ull),
           make_big(ctx,
                    5555724399352713551ull,
                    2855470964291304281ull,
                    1391062671460447334ull,
                    6816471574359062907ull));

  test_div(make_big(ctx,
                    4705319185493022819ull,
                    10937512369564114100ull,
                    4225858901270008466ull,
                    16934837836058328188ull,
                    11345351083103042896ull,
                    12502877840311511405ull,
                    15414710077957670839ull,
                    14073218093612395962ull,
                    10141262211251290976ull),
           make_big(ctx,
                    16365587332934573297ull,
                    14110451132936902545ull,
                    14227516877567820616ull,
                    773047176899413847ull),
           make_big(ctx,
                    4027645462635810675ull,
                    15784796192978711547ull,
                    5351585159487159110ull,
                    13295712082196645282ull,
                    2186960985928611038ull,
                    13ull),
           make_big(ctx,
                    11528601757446224160ull,
                    13150424515817646992ull,
                    12367838195135482048ull,
                    731834574828767176ull));
}

TEST_F(scheme, read_bignum) {
  EXPECT_TRUE(num_equal(read("18446744073709551616"), make_big(ctx, 0ull, 1ull)));
  EXPECT_TRUE(num_equal(read("-18446744073709551616"), make_big_negative(ctx, 0ull, 1ull)));
  EXPECT_TRUE(num_equal(read("4611686018427387903"), make_big(ctx, 4611686018427387903ull)));
  EXPECT_TRUE(num_equal(read("38616195397574606111029898159411003755739963811995564291018845157317291934032285276296721365296300445450322552142080"),
                        make_big(ctx,
                                 262276201643358464ull,
                                 43373824340229465ull,
                                 7844025956150470852ull,
                                 470401255560051253ull,
                                 11431680516999648673ull,
                                 18078852890099872823ull)));
}

TEST_F(scheme, write_bignum) {
  EXPECT_EQ(to_string(ctx, make_big(ctx, 0, 1)), "18446744073709551616");
  EXPECT_EQ(to_string(ctx, make_big_negative(ctx, 0, 1)), "-18446744073709551616");
  EXPECT_EQ(to_string(ctx, make_big(ctx,
                                    17938764184775092447ull,
                                    4633044886490317294ull,
                                    11636559762171942713ull,
                                    8137458716480145127ull,
                                    1756181151806355891ull,
                                    13177594331470775302ull)),
            "28147170656646448008236484114643053198784882683455037102776641378964740414176277099771727921396609852836168744626399");
}

TEST_F(scheme, gcd) {
  EXPECT_EQ(expect<integer>(gcd(ctx, make<integer>(ctx, 32), make<integer>(ctx, 36)))->value(), 4);
  EXPECT_EQ(expect<integer>(gcd(ctx, make<integer>(ctx, 32), make<integer>(ctx, -36)))->value(), 4);
  EXPECT_EQ(expect<integer>(gcd(ctx,
                                read("15637276472805114870656615051104685548619384683912883036250789338440357844169168108881159530367194070722099976990439"),
                                read("28310636747421372819581491222259960704337320575997318939278431382334585462782549354990068622867945599916809865521754")))->value(),
            3);
  EXPECT_EQ(expect<integer>(gcd(ctx,
                                read("32900744989775849384444111067279827681464964267754061354276182079922022805047611540836777891564097053008952910818635"),
                                read("16871863743058363314072331379289859763122718194749875170705775524655845186234765954680571628747994726760230873387371")))->value(),
            1);
  EXPECT_TRUE(num_equal(gcd(ctx,
                            read("326842357047048580094685541896229290526226710742342560706866927058691036387550824695609726546021742995306480127227041503526172382597364126586477735162986"),
                            read("7143737363507851466671560831127318663187019217037069553424396890442458422936353440819622600793958827154371382539989874611891068389366589075003540538150224")),
                        read("3482687899064411289424507725617653109781215164227824305838")));
}

TEST_F(scheme, read_write_fraction) {
  EXPECT_TRUE(num_equal(read("1/2"), make_fraction(1, 2)));
  EXPECT_TRUE(num_equal(read("2/4"), make_fraction(1, 2)));
  EXPECT_TRUE(num_equal(read("-1/2"), make_fraction(-1, 2)));
  EXPECT_TRUE(num_equal(read("0/5"), make<integer>(ctx, 0)));
  EXPECT_TRUE(num_equal(read("6/3"), make<integer>(ctx, 2)));

  EXPECT_EQ(to_string(ctx, make_fraction(1, 2)), "1/2");
  EXPECT_EQ(to_string(ctx, make_fraction(-1, 2)), "-1/2");
}

TEST_F(scheme, fraction_arithmetic) {
  EXPECT_TRUE(num_equal(add(ctx, make_fraction(1, 2), make_fraction(1, 3)), make_fraction(5, 6)));
  EXPECT_TRUE(num_equal(add(ctx, make_fraction(7, 12), make_fraction(5, 12)), make<integer>(ctx, 1)));
  EXPECT_TRUE(num_equal(add(ctx, make_fraction(1, 6), make_fraction(2, 3)), make_fraction(5, 6)));
  EXPECT_TRUE(num_equal(add(ctx, make_fraction(3, 4), make_fraction(1, 6)), make_fraction(11, 12)));
  EXPECT_TRUE(num_equal(subtract(ctx, make<integer>(ctx, 1), make_fraction(1, 3)), make_fraction(2, 3)));
  EXPECT_TRUE(num_equal(subtract(ctx, make_fraction(7, 2), make<integer>(ctx, 4)), make_fraction(-1, 2)));
  EXPECT_TRUE(num_equal(multiply(ctx, make_fraction(1, 3), make_fraction(2, 3)), make_fraction(2, 9)));
  EXPECT_TRUE(num_equal(multiply(ctx, make_fraction(7, 2), make_fraction(2, 7)), make<integer>(ctx, 1)));
  EXPECT_TRUE(num_equal(divide(ctx, make<integer>(ctx, 1), make<integer>(ctx, 2)), make_fraction(1, 2)));
  EXPECT_TRUE(num_equal(divide(ctx, make<integer>(ctx, 8), make<integer>(ctx, 2)), make<integer>(ctx, 4)));
  EXPECT_TRUE(num_equal(divide(ctx, make_fraction(3, 4), make_fraction(2, 3)), make_fraction(9, 8)));
  EXPECT_TRUE(num_equal(divide(ctx, make_fraction(1, 3), make_fraction(2, 3)), make_fraction(1, 2)));
}

TEST_F(scheme, read_write_float) {
  EXPECT_TRUE(num_equal(read("0.0"), make_float(0.0)));
  EXPECT_TRUE(num_equal(read("0.1"), make_float(0.1)));
  EXPECT_TRUE(num_equal(read("-0.1"), make_float(-0.1)));
  EXPECT_TRUE(num_equal(read("1.0"), make_float(1.0)));
  EXPECT_TRUE(num_equal(read("3.14"), make_float(3.14)));
  EXPECT_TRUE(num_equal(read(".5"), make_float(0.5)));
  EXPECT_TRUE(num_equal(read("-.5"), make_float(-0.5)));
  EXPECT_TRUE(num_equal(read("5."), make_float(5.0)));
  EXPECT_TRUE(num_equal(read("-5."), make_float(-5.0)));
  EXPECT_TRUE(num_equal(read("+inf.0"), make_float(floating_point::positive_infinity)));
  EXPECT_TRUE(num_equal(read("+INF.0"), make_float(floating_point::positive_infinity)));
  EXPECT_TRUE(num_equal(read("-inf.0"), make_float(floating_point::negative_infinity)));
  EXPECT_TRUE(std::isnan(expect<floating_point>(read("+nan.0"))->value));
  EXPECT_TRUE(std::isnan(expect<floating_point>(read("+NaN.0"))->value));
  EXPECT_TRUE(std::isnan(expect<floating_point>(read("-nan.0"))->value));

  EXPECT_EQ(to_string(ctx, make_float(0.0)), "0.0");
  EXPECT_EQ(to_string(ctx, make_float(0.1)), "0.1");
  EXPECT_EQ(to_string(ctx, make_float(-0.1)), "-0.1");
  EXPECT_EQ(to_string(ctx, make_float(1.0)), "1.0");
  EXPECT_EQ(to_string(ctx, make_float(3.14)), "3.14");
  EXPECT_EQ(to_string(ctx, make_float(123456789.0)), "123456789.0");
  EXPECT_EQ(to_string(ctx, make_float(floating_point::positive_infinity)), "+inf.0");
  EXPECT_EQ(to_string(ctx, make_float(floating_point::negative_infinity)), "-inf.0");
  EXPECT_EQ(to_string(ctx, make_float(floating_point::positive_nan)), "+nan.0");
  EXPECT_EQ(to_string(ctx, make_float(floating_point::negative_nan)), "-nan.0");
}

TEST_F(scheme, float_arithmetic) {
#define ASSERT_FP_EQ(lhs, rhs) ASSERT_DOUBLE_EQ(expect<floating_point>(lhs)->value, rhs)
  ASSERT_FP_EQ(add(ctx, make<floating_point>(ctx, 0.5), make<floating_point>(ctx, 0.4)), 0.9);
  ASSERT_FP_EQ(add(ctx, make<floating_point>(ctx, 0.7), make<integer>(ctx, 2)), 2.7);
  ASSERT_FP_EQ(add(ctx, make<floating_point>(ctx, 1.0), make_fraction(1, 2)), 1.5);
  ASSERT_FP_EQ(multiply(ctx, make<floating_point>(ctx, 3.0), make<floating_point>(ctx, 0.5)), 1.5);
  ASSERT_FP_EQ(divide(ctx, make<floating_point>(ctx, 3.0), make<floating_point>(ctx, 2.0)), 1.5);
  ASSERT_FP_EQ(divide(ctx, make<floating_point>(ctx, 1.0), make<floating_point>(ctx, 0.0)),
               floating_point::positive_infinity);
  ASSERT_TRUE(std::isnan(expect<floating_point>(divide(ctx, make<floating_point>(ctx, 0.0),
                                                       make<floating_point>(ctx, 0.0)))->value));
#undef ASSERT_FP_EQ
}
