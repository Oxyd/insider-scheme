#include "bignum.hpp"
#include "scheme_fixture.hpp"

#include "basic_types.hpp"
#include "character.hpp"
#include "numeric.hpp"
#include "port.hpp"
#include "read.hpp"
#include "string.hpp"
#include "write.hpp"

using namespace insider;

struct io : scheme_fixture { };

TEST_F(io, read_small_integer) {
  EXPECT_EQ(expect<integer>(read("0")).value(), 0);
  EXPECT_EQ(expect<integer>(read("2")).value(), 2);
  EXPECT_EQ(expect<integer>(read("-2")).value(), -2);
  EXPECT_EQ(expect<integer>(read("+215")).value(), +215);
  EXPECT_EQ(expect<integer>(read("-3812")).value(), -3812);
  EXPECT_EQ(expect<integer>(read("4611686018427387903")).value(), 4611686018427387903);
  EXPECT_EQ(expect<integer>(read("4611686018427387902")).value(), 4611686018427387902);
  EXPECT_EQ(expect<integer>(read("-4611686018427387903")).value(), -4611686018427387903);
  EXPECT_EQ(expect<integer>(read("-4611686018427387902")).value(), -4611686018427387902);
}

TEST_F(io, read_list) {
  ptr<> empty_1 = read("()");
  EXPECT_EQ(empty_1, ctx.constants->null.get());

  ptr<> empty_2 = read("(   )");
  EXPECT_EQ(empty_2, ctx.constants->null.get());

  ptr<> single_element = read("(1)");
  EXPECT_TRUE(is_list(single_element));
  EXPECT_EQ(expect<integer>(car(expect<pair>(single_element))).value(), 1);
  EXPECT_EQ(cdr(expect<pair>(single_element)), ctx.constants->null.get());

  ptr<> two_elements = read("(1 2)");
  EXPECT_TRUE(is_list(two_elements));
  EXPECT_EQ(expect<integer>(car(expect<pair>(two_elements))).value(), 1);
  EXPECT_EQ(expect<integer>(car(expect<pair>(cdr(expect<pair>(two_elements))))).value(), 2);
  EXPECT_EQ(cdr(expect<pair>(cdr(expect<pair>(two_elements)))), ctx.constants->null.get());

  ptr<> no_elements = read("()");
  EXPECT_EQ(no_elements, ctx.constants->null.get());

  ptr<> nested = read("(1 (2 3))");
  EXPECT_TRUE(is_list(nested));
  EXPECT_EQ(expect<integer>(car(expect<pair>(nested))).value(), 1);
  EXPECT_TRUE(is_list(car(expect<pair>(cdr(expect<pair>(nested))))));
  ptr<pair> sublist_1 = expect<pair>(car(expect<pair>(cdr(expect<pair>(nested)))));
  EXPECT_EQ(expect<integer>(car(sublist_1)).value(), 2);
  EXPECT_EQ(expect<integer>(car(expect<pair>(cdr(sublist_1)))).value(), 3);

  EXPECT_THROW(read("("), read_error);
  EXPECT_THROW(read("(1 2"), read_error);
  EXPECT_THROW(read("(()"), read_error);
}

TEST_F(io, read_vector) {
  auto v1 = expect<vector>(read("#()"));
  EXPECT_EQ(expect<vector>(v1)->size(), 0);

  auto v2 = expect<vector>(read("#(1 2 3)"));
  EXPECT_EQ(v2->size(), 3);
  EXPECT_EQ(expect<integer>(v2->ref(0)).value(), 1);
  EXPECT_EQ(expect<integer>(v2->ref(1)).value(), 2);
  EXPECT_EQ(expect<integer>(v2->ref(2)).value(), 3);

  auto v3 = expect<vector>(read("#(#(a b) c #(d e) f)"));
  EXPECT_EQ(v3->size(), 4);
  EXPECT_TRUE(is<vector>(v3->ref(0)));
  EXPECT_TRUE(is<symbol>(v3->ref(1)));

  EXPECT_THROW(read("#("), read_error);
  EXPECT_THROW(read("#(1 2"), read_error);
}

TEST_F(io, read_symbol) {
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
  EXPECT_EQ(read(u8"bác"), ctx.intern(u8"bác"));
  EXPECT_EQ(read(u8"žbluňk"), ctx.intern(u8"žbluňk"));

  ptr<> l = read("(one two three)");
  ASSERT_TRUE(is_list(l));
  EXPECT_EQ(expect<symbol>(car(expect<pair>(l)))->value(), "one");
  EXPECT_EQ(expect<symbol>(car(expect<pair>(cdr(expect<pair>(l)))))->value(), "two");
  EXPECT_EQ(expect<symbol>(car(expect<pair>(cdr(expect<pair>(cdr(expect<pair>(l)))))))->value(), "three");
}

TEST_F(io, read_char) {
  EXPECT_EQ(expect<char32_t>(read(R"(#\a)")), 'a');
  EXPECT_EQ(expect<char32_t>(read(R"(#\A)")), 'A');
  EXPECT_EQ(expect<char32_t>(read(R"(#\4)")), '4');
  EXPECT_EQ(expect<char32_t>(read(R"(#\\)")), '\\');
  EXPECT_EQ(expect<char32_t>(read(R"(#\()")), '(');
  EXPECT_EQ(expect<char32_t>(read(R"(#\ )")), ' ');
  EXPECT_EQ(expect<char32_t>(read(R"(#\space)")), ' ');
  EXPECT_EQ(expect<char32_t>(read(R"(#\x63)")), 'c');
  EXPECT_EQ(expect<char32_t>(read(R"(#\x6d)")), 'm');
  EXPECT_EQ(expect<char32_t>(read(R"(#\x6D)")), 'm');
  EXPECT_EQ(expect<char32_t>(read(R"(#\x4d)")), 'M');
  EXPECT_EQ(expect<char32_t>(read(R"(#\x)")), 'x');
  EXPECT_EQ(expect<char32_t>(read(u8R"(#\ž)")), U'ž');
  EXPECT_EQ(expect<char32_t>(read(R"(#\x17e)")), U'ž');
}

TEST_F(io, read_string) {
  EXPECT_EQ(expect<string>(read(R"("foo")"))->value(), "foo");
  EXPECT_EQ(expect<string>(read(R"("one\ntwo")"))->value(), "one\ntwo");
  char const* msvc_workaround1 = R"("this \"is\" a quote")";
  EXPECT_EQ(expect<string>(read(msvc_workaround1))->value(), "this \"is\" a quote");
  char const* msvc_workaround2 = R"("foo\"bar\"baz")";
  EXPECT_EQ(expect<string>(read(msvc_workaround2))->value(), "foo\"bar\"baz");

  EXPECT_THROW(read(R"("unterminated)"), read_error);
  char const* msvc_workaround3 = R"("\invalid escape")";
  EXPECT_THROW(read(msvc_workaround3), read_error);
  char const* msvc_workaround4 = R"("\)";
  EXPECT_THROW(read(msvc_workaround4), read_error);
  char const* msvc_workaround5 = R"("\")";
  EXPECT_THROW(read(msvc_workaround5), read_error);

  char const* msvc_workaround6 = u8R"("příšerně žluťoučký kůň úpěl ďábelské ódy")";
  EXPECT_EQ(expect<string>(read(msvc_workaround6))->value(), u8"příšerně žluťoučký kůň úpěl ďábelské ódy");

  char const* msvc_workaround7 = R"("foo\x20;bar")";
  EXPECT_EQ(expect<string>(read(msvc_workaround7))->value(), "foo bar");

  char const* msvc_workaround8 = R"("foo\x20bar")";
  EXPECT_THROW(read(msvc_workaround8), read_error);

  char const* msvc_workaround9 = R"("foo\)" "\n"
                                 R"(bar")";
  EXPECT_EQ(expect<string>(read(msvc_workaround9))->value(), "foobar");

  char const* msvc_workaround10 = R"("foo\    )" "\n"
                                  R"(bar")";
  EXPECT_EQ(expect<string>(read(msvc_workaround10))->value(), "foobar");

  char const* msvc_workaround11 = R"("foo\)" "\n"
                                  R"(    bar")";
  EXPECT_EQ(expect<string>(read(msvc_workaround11))->value(), "foobar");

  char const* msvc_workaround12 = R"("foo\   )" "\n"
                                  R"(    bar")";
  EXPECT_EQ(expect<string>(read(msvc_workaround12))->value(), "foobar");
}

TEST_F(io, read_multiple) {
  std::vector<tracked_ptr<>> result1 = read_multiple(ctx, "foo bar baz");
  ASSERT_EQ(result1.size(), 3);
  EXPECT_EQ(expect<symbol>(result1[0])->value(), "foo");
  EXPECT_EQ(expect<symbol>(result1[1])->value(), "bar");
  EXPECT_EQ(expect<symbol>(result1[2])->value(), "baz");

  std::vector<tracked_ptr<>> result2 = read_multiple(ctx, "(foo) (bar 2)");
  ASSERT_EQ(result2.size(), 2);
  EXPECT_TRUE(is_list(result2[0].get()));
  EXPECT_EQ(list_length(result2[0].get()), 1);

  EXPECT_TRUE(is_list(result2[1].get()));
  EXPECT_EQ(list_length(result2[1].get()), 2);
}

TEST_F(io, read_comments) {
  EXPECT_EQ(expect<integer>(read(R"(;; Comment
                                    2)")).value(),
            2);
  EXPECT_EQ(expect<integer>(read("7 ;; A prime number")).value(), 7);
  EXPECT_EQ(expect<string>(read(R"("foo;bar;baz" ; string)"))->value(), "foo;bar;baz");
}

TEST_F(io, read_datum_comment) {
  EXPECT_TRUE(equal(ctx, read("(1 #;(+ 2 3) 4)"), read("(1 4)")));
  EXPECT_TRUE(equal(ctx, read("(1 #;   (+ 2 3) 4)"), read("(1 4)")));
  EXPECT_TRUE(equal(ctx, read("(1 #;(+ #;(+ 4 5) 3) 4)"), read("(1 4)")));
  EXPECT_TRUE(equal(ctx, read("(1 #;2 4)"), read("(1 4)")));
}

TEST_F(io, read_block_comment) {
  EXPECT_TRUE(equal(ctx, read("(1 #| rest of the list |# 2 3)"), read("(1 2 3)")));
  EXPECT_TRUE(equal(ctx, read("(1 #| outer #| nested |# |# 2 3)"), read("(1 2 3)")));
}

TEST_F(io, case_folding) {
  EXPECT_TRUE(equal(ctx, read("#!fold-case fOO"), read("foo")));
  EXPECT_TRUE(equal(ctx, read("(#!fold-case FOO #!no-fold-case BAR)"), read("(foo BAR)")));
  EXPECT_EQ(expect<char32_t>(read(R"(#!fold-case #\NEWLINE)")), '\n');
}

static std::string
to_string_simple(context& ctx, ptr<> datum) {
  auto out = make<textual_output_port>(ctx, std::make_unique<string_port_sink>());
  write_simple(ctx, datum, out);
  return out->get_string();
}

TEST_F(io, write) {
  EXPECT_EQ(to_string_simple(ctx, read("(1 2 3)")), "(1 2 3)");

  auto p1 = make<pair>(ctx, integer_to_ptr(integer{1}), integer_to_ptr(integer{2}));
  EXPECT_EQ(to_string_simple(ctx, p1), "(1 . 2)");

  auto p2 = make<pair>(ctx, integer_to_ptr(integer{0}), p1);
  EXPECT_EQ(to_string_simple(ctx, p2), "(0 1 . 2)");

  auto v = make<vector>(ctx, ctx, 3);
  v->set(ctx.store, 0, character_to_ptr('r'));
  v->set(ctx.store, 1, p2);
  v->set(ctx.store, 2, make<string>(ctx, "foobar"));
  EXPECT_EQ(to_string_simple(ctx, v), R"(#(#\r (0 1 . 2) "foobar"))");

  auto s = make<string>(ctx, R"(one "two" three \ four)");
  char const* msvc_workaround1 = R"("one \"two\" three \\ four")";
  EXPECT_EQ(to_string_simple(ctx, s), msvc_workaround1);

  auto l = make_list(
    ctx,
    ctx.constants->null.get(),
    ctx.constants->void_.get(),
    ctx.constants->t.get(),
    ctx.constants->f.get(),
    ctx.intern("symbol"),
    make<string>(ctx, "string"),
    character_to_ptr('c'),
    integer_to_ptr(integer{-13})
  );
  EXPECT_EQ(to_string_simple(ctx, l), R"((() #void #t #f symbol "string" #\c -13))");
}

TEST_F(io, read_bignum) {
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

TEST_F(io, write_bignum) {
  EXPECT_EQ(to_string_simple(ctx, make_big(ctx, 0, 1)), "18446744073709551616");
  EXPECT_EQ(to_string_simple(ctx, make_big_negative(ctx, 0, 1)), "-18446744073709551616");
  EXPECT_EQ(to_string_simple(ctx, make_big(ctx,
                                    17938764184775092447ull,
                                    4633044886490317294ull,
                                    11636559762171942713ull,
                                    8137458716480145127ull,
                                    1756181151806355891ull,
                                    13177594331470775302ull)),
            "28147170656646448008236484114643053198784882683455037102776641378964740414176277099771727921396609852836168744626399");
}

TEST_F(io, read_write_fraction) {
  EXPECT_TRUE(num_equal(read("1/2"), make_fraction(1, 2)));
  EXPECT_TRUE(num_equal(read("2/4"), make_fraction(1, 2)));
  EXPECT_TRUE(num_equal(read("-1/2"), make_fraction(-1, 2)));
  EXPECT_TRUE(num_equal(read("0/5"), integer_to_ptr(integer{0})));
  EXPECT_TRUE(num_equal(read("6/3"), integer_to_ptr(integer{2})));

  EXPECT_EQ(to_string_simple(ctx, make_fraction(1, 2)), "1/2");
  EXPECT_EQ(to_string_simple(ctx, make_fraction(-1, 2)), "-1/2");
}

TEST_F(io, read_write_float) {
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

  EXPECT_EQ(to_string_simple(ctx, make_float(0.0)), "0.0");
  EXPECT_EQ(to_string_simple(ctx, make_float(0.1)), "0.1");
  EXPECT_EQ(to_string_simple(ctx, make_float(-0.1)), "-0.1");
  EXPECT_EQ(to_string_simple(ctx, make_float(1.0)), "1.0");
  EXPECT_EQ(to_string_simple(ctx, make_float(123456789.0)), "123456789.0");
  EXPECT_EQ(to_string_simple(ctx, make_float(floating_point::positive_infinity)), "+inf.0");
  EXPECT_EQ(to_string_simple(ctx, make_float(floating_point::negative_infinity)), "-inf.0");
  EXPECT_EQ(to_string_simple(ctx, make_float(floating_point::positive_nan)), "+nan.0");
  EXPECT_EQ(to_string_simple(ctx, make_float(floating_point::negative_nan)), "-nan.0");
}

TEST_F(io, read_datum_label) {
  EXPECT_EQ(expect<integer>(read("#0=2")).value(), 2);
}

TEST_F(io, read_datum_reference_to_atomic_value) {
  auto result = expect<pair>(read("(1 #0=2 3 #0#)"));
  EXPECT_TRUE(equal(ctx, result, read("(1 2 3 2)")));
}

TEST_F(io, read_datum_reference_in_pair) {
  auto result = expect<pair>(read("#0=(head . #0#)"));
  EXPECT_EQ(expect<symbol>(result->car())->value(), "head");
  EXPECT_EQ(result->cdr(), result);
}

TEST_F(io, read_datum_reference_in_vector) {
  auto result = expect<vector>(read("#0=#(1 2 #0# 3)"));
  EXPECT_EQ(expect<integer>(result->ref(0)).value(), 1);
  EXPECT_EQ(expect<integer>(result->ref(1)).value(), 2);
  EXPECT_EQ(expect<vector>(result->ref(2)), result);
  EXPECT_EQ(expect<integer>(result->ref(3)).value(), 3);
}

TEST_F(io, read_datum_reference_to_vector) {
  auto result = expect<pair>(read("(#0=#(1 2 3) #0#)"));
  EXPECT_TRUE(equal(ctx, expect<vector>(car(result)), read("#(1 2 3)")));
  EXPECT_TRUE(equal(ctx, expect<vector>(cadr(result)), read("#(1 2 3)")));
  EXPECT_EQ(expect<vector>(cadr(result)), car(result));
}

TEST_F(io, multiple_datum_references) {
  auto result = expect<pair>(read("#0=(0 . #1=(1 #0# . #1#))"));
  EXPECT_EQ(expect<integer>(result->car()).value(), 0);
  EXPECT_EQ(expect<integer>(cadr(result)).value(), 1);
  EXPECT_EQ(caddr(result), result);
  EXPECT_EQ(cadddr(result), cadr(result));
}

TEST_F(io, datum_label_to_atomic_shortcut) {
  auto result = expect<pair>(read("(#0='foo #0#)"));
  EXPECT_TRUE(equal(ctx, car(result), read("(quote foo)")));
  EXPECT_EQ(cadr(result), car(result));
}

TEST_F(io, datum_label_to_nonatomic_shortcut) {
  auto result = expect<pair>(read("#0='(#0#)"));
  EXPECT_EQ(expect<symbol>(car(result))->value(), "quote");
  EXPECT_EQ(car(expect<pair>(cadr(result))), result);
}

static std::string
to_string_shared(context& ctx, ptr<> datum) {
  auto out = make<textual_output_port>(ctx, std::make_unique<string_port_sink>());
  write_shared(ctx, datum, out);
  return out->get_string();
}

#define EXPECT_READ_WRITE_EQ(s) EXPECT_EQ(to_string_shared(ctx, read(s)), s)

TEST_F(io, write_shared_simple_list) {
  EXPECT_READ_WRITE_EQ("(#0=(1 2) #0#)");
}

TEST_F(io, write_shared_infinite_list) {
  EXPECT_READ_WRITE_EQ("#0=(1 2 . #0#)");
}

TEST_F(io, write_shared_simple_vector) {
  EXPECT_READ_WRITE_EQ("(#0=#(1 2 3) #0#)");
}

TEST_F(io, write_shared_infinite_vector) {
  EXPECT_READ_WRITE_EQ("#0=#(1 2 #0#)");
}

#undef EXPECT_READ_WRITE_EQ

static std::string
to_string(context& ctx, ptr<> datum) {
  auto out = make<textual_output_port>(ctx, std::make_unique<string_port_sink>());
  write(ctx, datum, out);
  return out->get_string();
}

static std::string
read_write(context& ctx, std::string const& expr) {
  return to_string(ctx, read(ctx, expr));
}

TEST_F(io, write_doesnt_use_labels_when_no_cycles) {
  EXPECT_EQ(read_write(ctx, "(#0=1 #0#)"), "(1 1)");
}

TEST_F(io, write_uses_labels_when_cycles_are_present) {
  EXPECT_EQ(read_write(ctx, "#0=(1 . #0#)"), "#0=(1 . #0#)");
}
