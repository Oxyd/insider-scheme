#include <gtest/gtest.h>

#include "bytecode.hpp"
#include "compiler.hpp"
#include "io.hpp"
#include "scheme.hpp"
#include "vm.hpp"

using namespace game::scm;

struct scheme : testing::Test {
  unique_context ctx;
};

struct aaa : object {
  bool* alive;

  explicit
  aaa(bool* alive) : alive{alive} { *alive = true; }

  ~aaa() override { *alive = false; }
};

TEST_F(scheme, collect_direct_garbage) {
  bool one{}, two{}, three{};
  ptr<aaa> a = thread_context().store.make<aaa>(&one);
  ptr<aaa> b = thread_context().store.make<aaa>(&two);
  ptr<aaa> c = thread_context().store.make<aaa>(&three);

  thread_context().store.collect_garbage();

  EXPECT_TRUE(one);
  EXPECT_TRUE(two);
  EXPECT_TRUE(three);

  a.reset();
  thread_context().store.collect_garbage();

  EXPECT_FALSE(one);
  EXPECT_TRUE(two);
  EXPECT_TRUE(three);

  b.reset();
  c.reset();
  thread_context().store.collect_garbage();

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
  ptr<bbb> root = thread_context().store.make<bbb>(&one);
  root->child = thread_context().store.make<bbb>(&two).get();
  static_cast<bbb*>(root->child)->child = thread_context().store.make<bbb>(&three).get();
  static_cast<bbb*>(static_cast<bbb*>(root->child)->child)->child = thread_context().store.make<bbb>(&four).get();

  thread_context().store.collect_garbage();
  EXPECT_TRUE(one);
  EXPECT_TRUE(two);
  EXPECT_TRUE(three);
  EXPECT_TRUE(four);

  static_cast<bbb*>(root->child)->child = nullptr;
  thread_context().store.collect_garbage();
  EXPECT_TRUE(one);
  EXPECT_TRUE(two);
  EXPECT_FALSE(three);
  EXPECT_FALSE(four);

  root.reset();
  thread_context().store.collect_garbage();
  EXPECT_FALSE(one);
  EXPECT_FALSE(two);
  EXPECT_FALSE(three);
  EXPECT_FALSE(four);
}

TEST_F(scheme, collect_circles) {
  bool one{}, two{};
  ptr<bbb> a = thread_context().store.make<bbb>(&one);
  a->child = thread_context().store.make<bbb>(&two).get();
  static_cast<bbb*>(a->child)->child = a.get();

  thread_context().store.collect_garbage();
  EXPECT_TRUE(one);
  EXPECT_TRUE(two);

  a.reset();
  thread_context().store.collect_garbage();
  EXPECT_FALSE(one);
  EXPECT_FALSE(two);
}

TEST_F(scheme, weak_ptr) {
  bool one{};
  ptr<aaa> a = make<aaa>(&one);
  weak_ptr<aaa> w = a;

  thread_context().store.collect_garbage();
  EXPECT_TRUE(one);
  EXPECT_TRUE(w);

  a.reset();
  thread_context().store.collect_garbage();
  EXPECT_FALSE(one);
  EXPECT_FALSE(w);

  ptr<aaa> b = w.lock();
  EXPECT_FALSE(b);
}

TEST_F(scheme, type_predicates) {
  ptr<pair> p = make<pair>(null(), null());
  generic_ptr x = p;
  generic_ptr null = game::scm::null();

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
  ptr<pair> l1 = make<pair>(make<integer>(1), make<integer>(2));
  EXPECT_FALSE(is_list(l1));

  // (1 2)
  ptr<pair> l2 = make<pair>(make<integer>(1),
                            make<pair>(make<integer>(2),
                                       null()));
  EXPECT_TRUE(is_list(l2));

  // (0 1 2)
  ptr<pair> l3 = make<pair>(make<integer>(0), l2);
  EXPECT_TRUE(is_list(l3));
}

TEST_F(scheme, make_list) {
  generic_ptr empty = make_list();
  EXPECT_TRUE(empty == null());

  generic_ptr l = make_list(make<integer>(1),
                            make<integer>(2),
                            make<integer>(3));
  auto first = expect<pair>(l);
  EXPECT_EQ(expect<integer>(first->car())->value(), 1);

  auto second = expect<pair>(first->cdr());
  EXPECT_EQ(expect<integer>(second->car())->value(), 2);

  auto third = expect<pair>(second->cdr());
  EXPECT_EQ(expect<integer>(third->car())->value(), 3);

  EXPECT_EQ(third->cdr(), null());
}

TEST_F(scheme, intern) {
  ptr<symbol> a_1 = intern("a");
  ptr<symbol> b_1 = intern("b");
  ptr<symbol> a_2 = intern("a");

  EXPECT_TRUE(a_1);
  EXPECT_TRUE(b_1);
  EXPECT_TRUE(a_2);

  EXPECT_EQ(a_1, a_2);
  EXPECT_NE(a_1, b_1);

  b_1.reset();
  thread_context().store.collect_garbage();

  ptr<symbol> b_2 = intern("b");
  ptr<symbol> b_3 = intern("b");
  EXPECT_TRUE(b_2);
  EXPECT_TRUE(b_3);
  EXPECT_EQ(b_2, b_3);
}

TEST_F(scheme, vector) {
  ptr<vector> v1 = make<vector>(3);
  v1->set(0, make<integer>(1));
  v1->set(1, make<integer>(2));
  v1->set(2, make<integer>(3));

  EXPECT_EQ(v1->size(), 3u);

  EXPECT_EQ(expect<integer>(v1->ref(0))->value(), 1);
  EXPECT_EQ(expect<integer>(v1->ref(1))->value(), 2);
  EXPECT_EQ(expect<integer>(v1->ref(2))->value(), 3);

  EXPECT_THROW(v1->ref(3), std::runtime_error);
  EXPECT_THROW(v1->set(4, make<integer>(4)), std::runtime_error);

  ptr<vector> v2 = make<vector>(2);
  bool one{}, two{};
  v2->set(0, make<aaa>(&one));
  v2->set(1, make<aaa>(&two));

  EXPECT_TRUE(one);
  EXPECT_TRUE(two);

  v2.reset();
  thread_context().store.collect_garbage();

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
  EXPECT_EQ(empty_1, null());

  generic_ptr empty_2 = read("(   )");
  EXPECT_EQ(empty_2, null());

  generic_ptr single_element = read("(1)");
  EXPECT_TRUE(is_list(single_element));
  EXPECT_EQ(expect<integer>(expect<pair>(single_element)->car())->value(), 1);
  EXPECT_EQ(expect<pair>(single_element)->cdr(), null());

  generic_ptr two_elements = read("(1 2)");
  EXPECT_TRUE(is_list(two_elements));
  EXPECT_EQ(expect<integer>(expect<pair>(two_elements)->car())->value(), 1);
  EXPECT_EQ(expect<integer>(expect<pair>(expect<pair>(two_elements)->cdr())->car())->value(), 2);
  EXPECT_EQ(expect<pair>(expect<pair>(two_elements)->cdr())->cdr(), null());

  generic_ptr no_elements = read("()");
  EXPECT_EQ(no_elements, null());

  generic_ptr nested = read("(1 (2 3))");
  EXPECT_TRUE(is_list(nested));
  EXPECT_EQ(expect<integer>(expect<pair>(nested)->car())->value(), 1);
  EXPECT_TRUE(is_list(expect<pair>(expect<pair>(nested)->cdr())->car()));
  ptr<pair> sublist = expect<pair>(expect<pair>(expect<pair>(nested)->cdr())->car());
  EXPECT_EQ(expect<integer>(sublist->car())->value(), 2);
  EXPECT_EQ(expect<integer>(expect<pair>(sublist->cdr())->car())->value(), 3);

  EXPECT_THROW(read("("), parse_error);
  EXPECT_THROW(read("(1 2"), parse_error);
  EXPECT_THROW(read("(()"), parse_error);
}

TEST_F(scheme, read_symbol) {
  EXPECT_EQ(read("foo"), intern("foo"));
  EXPECT_EQ(read("multiple-words"), intern("multiple-words"));
  EXPECT_EQ(read("%special-symbol"), intern("%special-symbol"));
  EXPECT_EQ(read("+"), intern("+"));
  EXPECT_EQ(read("-"), intern("-"));
  EXPECT_EQ(read("+fun"), intern("+fun"));
  EXPECT_EQ(read("#$if"), intern("#$if"));

  generic_ptr l = read("(one two three)");
  ASSERT_TRUE(is_list(l));
  EXPECT_EQ(expect<symbol>(expect<pair>(l)->car())->value(), "one");
  EXPECT_EQ(expect<symbol>(expect<pair>(expect<pair>(l)->cdr())->car())->value(), "two");
  EXPECT_EQ(expect<symbol>(expect<pair>(expect<pair>(expect<pair>(l)->cdr())->cdr())->car())->value(), "three");
}

TEST(bytecode, instruction_info_consistency) {
  EXPECT_EQ(opcode_to_info(opcode::add).opcode, opcode::add);
  EXPECT_EQ(opcode_to_info(opcode::add).mnemonic, "add");

  EXPECT_EQ(mnemonic_to_info("add").mnemonic, "add");
  EXPECT_EQ(mnemonic_to_info("add").opcode, opcode::add);
}

template <typename T, typename... Args>
operand
make_static(Args&&... args) {
  return operand::static_(thread_context().intern_static(make<T>(std::forward<Args>(args)...)));
}

TEST_F(scheme, exec_arithmetic) {
  // 2 * (3 + 6). The input constants are stored in statics, result is stored in
  // local register 0.
  auto two = make_static<integer>(2);
  auto three = make_static<integer>(3);
  auto six = make_static<integer>(6);

  auto proc = make<procedure>(
    bytecode{{opcode::add,      three, six,               operand::local(1)},
             {opcode::multiply, two,   operand::local(1), operand::local(0)}},
    2,
    0
  );
  auto state = make_state(thread_context(), proc, make<module>());
  run(state);

  EXPECT_EQ(assume<integer>(state.current_frame->local(0))->value(), 18);
}

TEST_F(scheme, exec_calls) {
  // f(x, y) = 2 * x + y
  // Evaluate: 3 * f(5, 7) + f(2, f(3, 4))

  auto two = make_static<integer>(2);

  auto f = make_static<procedure>(
    bytecode{{opcode::multiply, two,               operand::local(0), operand::local(2)},
             {opcode::add,      operand::local(2), operand::local(1), operand::local(2)},
             {opcode::ret,      operand::local(2), {},                {}}},
    3,
    2
  );

  auto three = make_static<integer>(3);
  auto five = make_static<integer>(5);
  auto seven = make_static<integer>(7);
  auto four = make_static<integer>(4);

  auto global = make<procedure>(
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
  auto state = make_state(thread_context(), global, make<module>());
  run(state);

  auto native_f = [] (int x, int y) { return 2 * x + y; };
  EXPECT_EQ(assume<integer>(state.current_frame->local(0))->value(),
            3 * native_f(5, 7) + native_f(2, native_f(3, 4)));
}

TEST_F(scheme, three_argument_calls) {
  // f(x, y, z) = 2 * x + 3 * y + 4 * z
  // Evaluate 2 * f(1, 2, 3)

  auto one = make_static<integer>(1);
  auto two = make_static<integer>(2);
  auto three = make_static<integer>(3);
  auto four = make_static<integer>(4);

  auto f = make_static<procedure>(
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
    bytecode{{opcode::call,     f,   operand::immediate(3), operand::local(0)},
             {opcode::data,     one, two,                   three},
             {opcode::multiply, two, operand::local(0),     operand::local(0)}},
    1,
    0
  );
  auto state = make_state(thread_context(), global, make<module>());
  run(state);

  auto native_f = [] (int x, int y, int z) { return 2 * x + 3 * y + 4 * z; };
  EXPECT_EQ(expect<integer>(state.current_frame->local(0))->value(),
            2 * native_f(1, 2, 3));
}

TEST_F(scheme, exec_tail_calls) {
  // f(x) = g(x)
  // g(x) = 2 * x
  auto two = make_static<integer>(2);
  auto g = make_static<procedure>(
    bytecode{{opcode::multiply, two,               operand::local(0), operand::local(1)},
             {opcode::ret,      operand::local(1), {},                {}}},
    2,
    1
  );
  auto f = make_static<procedure>(
    bytecode{{opcode::tail_call, g,                 operand::immediate(1), {}},
             {opcode::data,      operand::local(0), {},                    {}}},
    1,
    1
  );
  auto six = make_static<integer>(6);
  auto global = make<procedure>(
    bytecode{{opcode::call, f,   operand::immediate(1), operand::local(0)},
             {opcode::data, six, {},                    {}}},
    1,
    0
  );
  auto state = make_state(thread_context(), global, make<module>());
  run(state);

  EXPECT_EQ(assume<integer>(state.current_frame->local(0))->value(), 12);
}

TEST_F(scheme, exec_loop) {
  // sum = 0
  // i = 0
  // while i < 10
  //   sum += i
  //   i += 1

  auto zero = make_static<integer>(0);
  auto ten = make_static<integer>(10);
  auto one = make_static<integer>(1);
  auto global = make<procedure>(
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
  auto state = make_state(thread_context(), global, make<module>());
  run(state);

  EXPECT_EQ(assume<integer>(state.current_frame->local(0))->value(), 45);
}

TEST_F(scheme, exec_native_call) {
  auto native = [] (std::vector<generic_ptr> const& args) {
    return make<integer>(2 * expect<integer>(args[0])->value()
                         + 3 * expect<integer>(args[1])->value()
                         + 5 * expect<integer>(args[2])->value());
  };
  auto native_static = make_static<native_procedure>(native);
  auto ten = make_static<integer>(10);
  auto twenty = make_static<integer>(20);
  auto thirty = make_static<integer>(30);
  auto global = make<procedure>(
    bytecode{{opcode::call, native_static, operand::immediate(3), operand::local(0)},
             {opcode::data, ten,           twenty,                thirty}},
    1,
    0
  );
  auto state = make_state(thread_context(), global, make<module>());
  run(state);

  EXPECT_EQ(assume<integer>(state.current_frame->local(0))->value(),
            2 * 10 + 3 * 20 + 5 * 30);
}

TEST_F(scheme, exec_closure_ref) {
  auto add = make_static<procedure>(
    bytecode{{opcode::add, operand::local(0), operand::closure(0), operand::local(0)},
             {opcode::ret, operand::local(0), {},                  {}}},
    1, 1
  );
  auto three = make_static<integer>(3);
  auto five = make_static<integer>(5);
  auto global = make<procedure>(
    bytecode{{opcode::make_closure, add,               operand::immediate(1), operand::local(1)},
             {opcode::data,         three,             {},                    {}},
             {opcode::call,         operand::local(1), operand::immediate(1), operand::local(0)},
             {opcode::data,         five,              {},                    {}}},
    2, 0
  );
  auto state = make_state(thread_context(), global, make<module>());
  run(state);

  EXPECT_EQ(assume<integer>(state.current_frame->local(0))->value(), 5 + 3);
}

static generic_ptr
eval(std::string const& expr) {
  auto m = make<module>();
  import_all(m, thread_context().constants->internal);
  auto f = compile_expression(thread_context(), read(expr), m);
  auto state = make_state(thread_context(), f, m);
  return run(state);
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

  EXPECT_THROW(compile_expression(thread_context(), read("(let ((a 2)))"), thread_context().constants->internal),
               std::runtime_error);
  EXPECT_THROW(compile_expression(thread_context(), read("(let foo)"), thread_context().constants->internal),
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
  EXPECT_EQ(result4, thread_context().constants->void_);

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

TEST_F(scheme, compile_letrec) {
  generic_ptr result1 = eval(
    R"(
      (#$letrec ((a 3)
                 (b (+ a 2)))
        (+ a b))
    )"
  );
  EXPECT_EQ(expect<integer>(result1)->value(), 8);
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
      (#$letrec ((fact (#$lambda (n)
                         (#$if (= n 0)
                           1
                           (* n (fact (- n 1)))))))
        (fact 5))
    )"
  );
  EXPECT_EQ(expect<integer>(result2)->value(), 120);
}
