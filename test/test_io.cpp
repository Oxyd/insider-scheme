#include "bignum.hpp"
#include "scheme_fixture.hpp"

#include "io/port.hpp"
#include "io/read.hpp"
#include "io/write.hpp"
#include "runtime/basic_types.hpp"
#include "runtime/character.hpp"
#include "runtime/numeric.hpp"
#include "runtime/string.hpp"
#include "runtime/symbol.hpp"
#include "runtime/syntax.hpp"

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
  EXPECT_EQ(empty_1, ctx.constants->null);

  ptr<> empty_2 = read("(   )");
  EXPECT_EQ(empty_2, ctx.constants->null);

  ptr<> single_element = read("(1)");
  EXPECT_TRUE(is_list(single_element));
  EXPECT_EQ(expect<integer>(car(expect<pair>(single_element))).value(), 1);
  EXPECT_EQ(cdr(expect<pair>(single_element)), ctx.constants->null);

  ptr<> two_elements = read("(1 2)");
  EXPECT_TRUE(is_list(two_elements));
  EXPECT_EQ(expect<integer>(car(expect<pair>(two_elements))).value(), 1);
  EXPECT_EQ(expect<integer>(car(expect<pair>(cdr(expect<pair>(two_elements))))).value(), 2);
  EXPECT_EQ(cdr(expect<pair>(cdr(expect<pair>(two_elements)))), ctx.constants->null);

  ptr<> no_elements = read("()");
  EXPECT_EQ(no_elements, ctx.constants->null);

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

TEST_F(io, read_bytevector) {
  auto v1 = expect<bytevector>(read("#u8()"));
  EXPECT_EQ(v1->size(), 0);

  auto v2 = expect<bytevector>(read("#u8(1 2 3)"));
  EXPECT_EQ(v2->size(), 3);
  EXPECT_EQ(v2->ref(0), 1);
  EXPECT_EQ(v2->ref(1), 2);
  EXPECT_EQ(v2->ref(2), 3);

  EXPECT_THROW(read("#u8(712)"), read_error);
  EXPECT_THROW(read("#u8(-2)"), read_error);
  EXPECT_THROW(read("#u8(symbol)"), read_error);
  EXPECT_THROW(read("#u8(())"), read_error);
  EXPECT_THROW(read("#u8(12"), read_error);
  EXPECT_THROW(read("#u8("), read_error);
}

TEST_F(io, eval_bytevector_literal) {
  auto result = expect<bytevector>(eval("#u8(1 2 3)"));
  EXPECT_TRUE(equal(result, read("#u8(1 2 3)")));
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
  EXPECT_EQ(read("bác"), ctx.intern("bác"));
  EXPECT_EQ(read("žbluňk"), ctx.intern("žbluňk"));

  ptr<> l = read("(one two three)");
  ASSERT_TRUE(is_list(l));
  EXPECT_EQ(expect<symbol>(car(expect<pair>(l)))->value(), "one");
  EXPECT_EQ(expect<symbol>(car(expect<pair>(cdr(expect<pair>(l)))))->value(), "two");
  EXPECT_EQ(expect<symbol>(car(expect<pair>(cdr(expect<pair>(cdr(expect<pair>(l)))))))->value(), "three");
}

TEST_F(io, read_verbatim_symbol) {
  EXPECT_EQ(read("|foo|"), ctx.intern("foo"));
  EXPECT_THROW(read("|foo"), read_error);
  EXPECT_EQ(read("|foo bar|"), ctx.intern("foo bar"));
  EXPECT_EQ(read(R"(|foo\x20;bar|)"), ctx.intern("foo bar"));
  EXPECT_EQ(read(R"(|H\x65;llo|)"), ctx.intern("Hello"));
  EXPECT_EQ(read(R"(|\x3BB;|)"), ctx.intern("λ"));
  EXPECT_EQ(read(R"(|\t\t|)"), read(R"(|\x9;\x9;|)"));
  EXPECT_EQ(read(R"(||)"), ctx.intern(""));
}

TEST_F(io, read_char) {
  EXPECT_EQ(expect<char32_t>(read(R"(#\a)")), U'a');
  EXPECT_EQ(expect<char32_t>(read(R"(#\A)")), U'A');
  EXPECT_EQ(expect<char32_t>(read(R"(#\4)")), U'4');
  EXPECT_EQ(expect<char32_t>(read(R"(#\\)")), U'\\');
  EXPECT_EQ(expect<char32_t>(read(R"(#\()")), U'(');
  EXPECT_EQ(expect<char32_t>(read(R"(#\ )")), U' ');
  EXPECT_EQ(expect<char32_t>(read(R"(#\space)")), U' ');
  EXPECT_EQ(expect<char32_t>(read(R"(#\x63)")), U'c');
  EXPECT_EQ(expect<char32_t>(read(R"(#\x6d)")), U'm');
  EXPECT_EQ(expect<char32_t>(read(R"(#\x6D)")), U'm');
  EXPECT_EQ(expect<char32_t>(read(R"(#\x4d)")), U'M');
  EXPECT_EQ(expect<char32_t>(read(R"(#\x)")), U'x');
  EXPECT_EQ(expect<char32_t>(read(R"(#\ž)")), U'ž');
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

  char const* msvc_workaround6 = R"("příšerně žluťoučký kůň úpěl ďábelské ódy")";
  EXPECT_EQ(expect<string>(read(msvc_workaround6))->value(), "příšerně žluťoučký kůň úpěl ďábelské ódy");

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

TEST_F(io, read_simple_keyword) {
  EXPECT_EQ(expect<keyword>(read("#:foo"))->value(), "foo");
}

TEST_F(io, keywords_are_interned) {
  auto k1 = read("#:foo");
  auto k2 = read("#:foo");
  auto k3 = read("#:bar");
  EXPECT_EQ(k1, k2);
  EXPECT_NE(k1, k3);
}

TEST_F(io, keywords_can_be_case_folded) {
  EXPECT_EQ(expect<keyword>(read("#!fold-case #:FOO"))->value(), "foo");
}

TEST_F(io, read_verbatim_keyword) {
  EXPECT_EQ(expect<keyword>(read("#:|foo bar baz|"))->value(), "foo bar baz");
}

TEST_F(io, read_multiple) {
  std::vector<ptr<>> result1 = read_multiple(ctx, "foo bar baz");
  ASSERT_EQ(result1.size(), 3);
  EXPECT_EQ(expect<symbol>(result1[0])->value(), "foo");
  EXPECT_EQ(expect<symbol>(result1[1])->value(), "bar");
  EXPECT_EQ(expect<symbol>(result1[2])->value(), "baz");

  std::vector<ptr<>> result2 = read_multiple(ctx, "(foo) (bar 2)");
  ASSERT_EQ(result2.size(), 2);
  EXPECT_TRUE(is_list(result2[0]));
  EXPECT_EQ(list_length(result2[0]), 1);

  EXPECT_TRUE(is_list(result2[1]));
  EXPECT_EQ(list_length(result2[1]), 2);
}

TEST_F(io, read_comments) {
  EXPECT_EQ(expect<integer>(read(R"(;; Comment
                                    2)")).value(),
            2);
  EXPECT_EQ(expect<integer>(read("7 ;; A prime number")).value(), 7);
  EXPECT_EQ(expect<string>(read(R"("foo;bar;baz" ; string)"))->value(), "foo;bar;baz");
  EXPECT_EQ(expect<symbol>(read("foo;comment"))->value(), "foo");
}

TEST_F(io, read_datum_comment) {
  EXPECT_TRUE(equal(read("(1 #;(+ 2 3) 4)"), read("(1 4)")));
  EXPECT_TRUE(equal(read("(1 #;   (+ 2 3) 4)"), read("(1 4)")));
  EXPECT_TRUE(equal(read("(1 #;(+ #;(+ 4 5) 3) 4)"), read("(1 4)")));
  EXPECT_TRUE(equal(read("(1 #;2 4)"), read("(1 4)")));
}

TEST_F(io, read_block_comment) {
  EXPECT_TRUE(equal(read("(1 #| rest of the list |# 2 3)"), read("(1 2 3)")));
  EXPECT_TRUE(equal(read("(1 #| outer #| nested |# |# 2 3)"), read("(1 2 3)")));
}

TEST_F(io, case_folding) {
  EXPECT_TRUE(equal(read("#!fold-case fOO"), read("foo")));
  EXPECT_TRUE(equal(read("(#!fold-case FOO #!no-fold-case BAR)"), read("(foo BAR)")));
  EXPECT_EQ(expect<char32_t>(read(R"(#!fold-case #\NEWLINE)")), U'\n');
}

TEST_F(io, read_syntax_with_shared_data) {
  auto stx = assume<syntax>(read_syntax(ctx, "(#0=(1 2) #0#)"));
  auto first = expect<syntax>(
    car(expect<pair>(stx->update_and_get_expression(ctx)))
  )->update_and_get_expression(ctx);
  auto second = expect<syntax>(
    cadr(expect<pair>(stx->update_and_get_expression(ctx)))
  )->update_and_get_expression(ctx);
  EXPECT_EQ(first, second);
}

TEST_F(io, read_default_value_literal) {
  EXPECT_EQ(read("#default-value"), ctx.constants->default_value);
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

  auto v = make<vector>(ctx, 3, ctx.constants->void_);
  v->set(ctx.store, 0, character_to_ptr('r'));
  v->set(ctx.store, 1, p2);
  v->set(ctx.store, 2, make<string>(ctx, "foobar"));
  EXPECT_EQ(to_string_simple(ctx, v), R"(#(#\r (0 1 . 2) "foobar"))");

  auto s = make<string>(ctx, R"(one "two" three \ four)");
  char const* msvc_workaround1 = R"("one \"two\" three \\ four")";
  EXPECT_EQ(to_string_simple(ctx, s), msvc_workaround1);

  auto l = make_list(
    ctx,
    ctx.constants->null,
    ctx.constants->void_,
    ctx.constants->t,
    ctx.constants->f,
    ctx.intern("symbol"),
    make<string>(ctx, "string"),
    character_to_ptr('c'),
    integer_to_ptr(integer{-13})
  );
  EXPECT_EQ(to_string_simple(ctx, l), R"((() #void #t #f symbol "string" #\c -13))");
}

TEST_F(io, write_bytevector) {
  auto bv1 = make<bytevector>(ctx, 0);
  EXPECT_EQ(to_string_simple(ctx, bv1), "#u8()");

  auto bv2 = make<bytevector>(ctx, 3);
  bv2->set(0, 1);
  bv2->set(1, 2);
  bv2->set(2, 3);
  EXPECT_EQ(to_string_simple(ctx, bv2), "#u8(1 2 3)");
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

TEST_F(io, write_default_value) {
  EXPECT_EQ(to_string_simple(ctx, ctx.constants->default_value),
            "#default-value");
}

TEST_F(io, write_special_characters) {
  EXPECT_EQ(to_string_simple(ctx, character_to_ptr('\a')), R"(#\alarm)");
  EXPECT_EQ(to_string_simple(ctx, character_to_ptr('\x08')), R"(#\backspace)");
  EXPECT_EQ(to_string_simple(ctx, character_to_ptr('\x7F')), R"(#\delete)");
  EXPECT_EQ(to_string_simple(ctx, character_to_ptr('\x1B')), R"(#\escape)");
  EXPECT_EQ(to_string_simple(ctx, character_to_ptr('\n')), R"(#\newline)");
  EXPECT_EQ(to_string_simple(ctx, character_to_ptr('\0')), R"(#\null)");
  EXPECT_EQ(to_string_simple(ctx, character_to_ptr('\r')), R"(#\return)");
  EXPECT_EQ(to_string_simple(ctx, character_to_ptr(' ')), R"(#\space)");
  EXPECT_EQ(to_string_simple(ctx, character_to_ptr('\t')), R"(#\tab)");
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
  EXPECT_TRUE(equal(read("0.0"), make_float(0.0)));
  EXPECT_TRUE(equal(read("0.1"), make_float(0.1)));
  EXPECT_TRUE(equal(read("-0.1"), make_float(-0.1)));
  EXPECT_TRUE(equal(read("1.0"), make_float(1.0)));
  EXPECT_TRUE(equal(read("3.14"), make_float(3.14)));
  EXPECT_TRUE(equal(read(".5"), make_float(0.5)));
  EXPECT_TRUE(equal(read("-.5"), make_float(-0.5)));
  EXPECT_TRUE(equal(read("5."), make_float(5.0)));
  EXPECT_TRUE(equal(read("-5."), make_float(-5.0)));
  EXPECT_TRUE(equal(read("5e3"), make_float(5000.0)));
  EXPECT_TRUE(equal(read("5e-2"), make_float(0.05)));
  EXPECT_TRUE(equal(read("+inf.0"), make_float(floating_point::positive_infinity)));
  EXPECT_TRUE(equal(read("+INF.0"), make_float(floating_point::positive_infinity)));
  EXPECT_TRUE(equal(read("-inf.0"), make_float(floating_point::negative_infinity)));
  EXPECT_TRUE(std::isnan(expect<floating_point>(read("+nan.0"))->value));
  EXPECT_TRUE(std::isnan(expect<floating_point>(read("+NaN.0"))->value));
  EXPECT_TRUE(std::isnan(expect<floating_point>(read("-nan.0"))->value));

  EXPECT_EQ(to_string_simple(ctx, make_float(0.0)), "0.0");
  EXPECT_EQ(to_string_simple(ctx, make_float(-0.0)), "-0.0");
  EXPECT_EQ(to_string_simple(ctx, make_float(0.1)), "0.1");
  EXPECT_EQ(to_string_simple(ctx, make_float(-0.1)), "-0.1");
  EXPECT_EQ(to_string_simple(ctx, make_float(1.0)), "1.0");
  EXPECT_EQ(to_string_simple(ctx, make_float(123456789.0)), "123456789.0");
  EXPECT_EQ(to_string_simple(ctx, make_float(floating_point::positive_infinity)), "+inf.0");
  EXPECT_EQ(to_string_simple(ctx, make_float(floating_point::negative_infinity)), "-inf.0");
  EXPECT_EQ(to_string_simple(ctx, make_float(floating_point::positive_nan)), "+nan.0");
  EXPECT_EQ(to_string_simple(ctx, make_float(floating_point::negative_nan)), "-nan.0");

  EXPECT_EQ(to_string_simple(ctx, make_float(203.523)), "203.523");
}

TEST_F(io, read_negative_zero) {
  auto neg = expect<floating_point>(read("-0.0"));
  EXPECT_TRUE(std::signbit(neg->value));

  auto pos = expect<floating_point>(read("+0.0"));
  EXPECT_FALSE(std::signbit(pos->value));
}

TEST_F(io, read_datum_label) {
  EXPECT_EQ(expect<integer>(read("#0=2")).value(), 2);
}

TEST_F(io, read_datum_reference_to_atomic_value) {
  auto result = expect<pair>(read("(1 #0=2 3 #0#)"));
  EXPECT_TRUE(equal(result, read("(1 2 3 2)")));
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

TEST_F(io, read_datum_reference_in_bytevector) {
  auto result = expect<pair>(read("(#0=2 . #u8(1 #0# 3))"));
  EXPECT_EQ(expect<integer>(car(result)).value(), 2);
  EXPECT_TRUE(equal(cdr(result), read("#u8(1 2 3)")));
}

TEST_F(io, read_datum_reference_to_vector) {
  auto result = expect<pair>(read("(#0=#(1 2 3) #0#)"));
  EXPECT_TRUE(equal(expect<vector>(car(result)), read("#(1 2 3)")));
  EXPECT_TRUE(equal(expect<vector>(cadr(result)), read("#(1 2 3)")));
  EXPECT_EQ(expect<vector>(cadr(result)), car(result));
}

TEST_F(io, read_datum_reference_to_bytevector) {
  auto result = expect<pair>(read("(#0=#u8(1 2 3) #0#)"));
  EXPECT_TRUE(equal(expect<bytevector>(car(result)), read("#u8(1 2 3)")));
  EXPECT_TRUE(equal(expect<bytevector>(cadr(result)), read("#u8(1 2 3)")));
  EXPECT_EQ(expect<bytevector>(cadr(result)), car(result));
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
  EXPECT_TRUE(equal(car(result), read("(quote foo)")));
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

TEST_F(io, read_decimal_prefix) {
  EXPECT_EQ(expect<integer>(read("#d12")).value(), 12);
  EXPECT_EQ(expect<integer>(read("#d-4")).value(), -4);
  EXPECT_TRUE(num_equal(read("#d1/3"), make_fraction(1, 3)));
  EXPECT_TRUE(num_equal(read("#d+inf.0"), make<floating_point>(ctx, floating_point::positive_infinity)));
}

TEST_F(io, read_binary_prefix) {
  EXPECT_EQ(expect<integer>(read("#b11")).value(), 0b11);
  EXPECT_EQ(expect<integer>(read("#b-11")).value(), -0b11);
  EXPECT_TRUE(num_equal(read("#b1/10"), make_fraction(1, 2)));
  EXPECT_TRUE(num_equal(read("#b+inf.0"), make<floating_point>(ctx, floating_point::positive_infinity)));
}

TEST_F(io, read_octal_prefix) {
  EXPECT_EQ(expect<integer>(read("#o10")).value(), 8);
  EXPECT_TRUE(num_equal(read("#o+inf.0"), make<floating_point>(ctx, floating_point::positive_infinity)));
}

TEST_F(io, read_hex_prefix) {
  EXPECT_EQ(expect<integer>(read("#x10")).value(), 16);
  EXPECT_EQ(expect<integer>(read("#xa")).value(), 10);
  EXPECT_EQ(expect<integer>(read("#xaB")).value(), 0xab);
  EXPECT_EQ(expect<integer>(read("#x1a2B3c4")).value(), 0x1a2b3c4);
  EXPECT_TRUE(num_equal(read("#x+inf.0"), make<floating_point>(ctx, floating_point::positive_infinity)));
}

TEST_F(io, read_inexact) {
  EXPECT_TRUE(equal(read("#i5"), read("5.0")));
  EXPECT_TRUE(equal(read("#i-5"), read("-5.0")));
  EXPECT_TRUE(equal(read("#i1/2"), read("0.5")));
  EXPECT_TRUE(equal(read("#i0.25"), read("0.25")));
  EXPECT_TRUE(equal(read("#i#xa"), read("10.0")));
  EXPECT_TRUE(equal(read("#o#i10"), read("8.0")));
  EXPECT_TRUE(equal(read("#i+i"), read("0.0+1.0i")));
  EXPECT_TRUE(equal(read("#i1+i"), read("1.0+1.0i")));
}

TEST_F(io, read_exact) {
  EXPECT_TRUE(equal(read("#e1.0"), read("1")));
  EXPECT_TRUE(equal(read("#e0.1"), read("1/10")));
  EXPECT_TRUE(equal(read("#e3.14"), read("314/100")));
  EXPECT_TRUE(equal(read("#e1e2"), read("100")));
  EXPECT_TRUE(equal(read("#e123.456"), read("123456/1000")));
  EXPECT_TRUE(equal(read("#e123.456e2"), read("123456/10")));
  EXPECT_TRUE(equal(read("#e123.456e-2"), read("123456/100000")));
  EXPECT_THROW(read("#e+inf.0"), std::runtime_error);
  EXPECT_THROW(read("#e+nan.0"), std::runtime_error);
}

TEST_F(io, read_complex) {
  EXPECT_TRUE(equal(read("1+1i"), make_rectangular(ctx, integer_to_ptr(1), integer_to_ptr(1))));
  EXPECT_TRUE(equal(read("1-1i"), make_rectangular(ctx, integer_to_ptr(1), integer_to_ptr(-1))));
  EXPECT_TRUE(equal(read("1/2+2/3i"), make_rectangular(ctx, make_fraction(1, 2), make_fraction(2, 3))));
  EXPECT_TRUE(equal(read("1.5+1.5i"), make_rectangular(ctx, make<floating_point>(ctx, 1.5), make<floating_point>(ctx, 1.5))));

  EXPECT_TRUE(equal(read("1+i"), make_rectangular(ctx, integer_to_ptr(1), integer_to_ptr(1))));
  EXPECT_TRUE(equal(read("1-i"), make_rectangular(ctx, integer_to_ptr(1), integer_to_ptr(-1))));

  EXPECT_TRUE(equal(read("1+inf.0i"), make_rectangular(ctx, integer_to_ptr(1), make<floating_point>(ctx, floating_point::positive_infinity))));

  auto one_plus_imaginary_nan = expect<complex>(read("1+nan.0i"));
  EXPECT_TRUE(equal(one_plus_imaginary_nan->real(), integer_to_ptr(1)));
  EXPECT_TRUE(std::isnan(expect<floating_point>(one_plus_imaginary_nan->imaginary())->value));

  EXPECT_TRUE(equal(read("+2i"), make_rectangular(ctx, integer_to_ptr(0), integer_to_ptr(2))));
  EXPECT_TRUE(equal(read("-2i"), make_rectangular(ctx, integer_to_ptr(0), integer_to_ptr(-2))));

  EXPECT_TRUE(equal(read("+inf.0i"), make_rectangular(ctx, integer_to_ptr(0), make<floating_point>(ctx, floating_point::positive_infinity))));

  auto zero_plus_imaginary_nan = expect<complex>(read("+nan.0i"));
  EXPECT_TRUE(equal(zero_plus_imaginary_nan->real(), integer_to_ptr(0)));
  EXPECT_TRUE(std::isnan(expect<floating_point>(zero_plus_imaginary_nan->imaginary())->value));

  EXPECT_TRUE(equal(read("+i"), make_rectangular(ctx, integer_to_ptr(0), integer_to_ptr(1))));
  EXPECT_TRUE(equal(read("-i"), make_rectangular(ctx, integer_to_ptr(0), integer_to_ptr(-1))));
}

TEST_F(io, read_polar_complex) {
  auto z1 = read("1@0");
  EXPECT_EQ(expect<integer>(z1).value(), 1);

  auto z2 = expect<complex>(read("1@1.5707963267948966"));
  EXPECT_NEAR(expect<floating_point>(z2->real())->value, 0.0, 1e-6);
  EXPECT_DOUBLE_EQ(expect<floating_point>(z2->imaginary())->value, 1.0);

  auto z3 = expect<complex>(read("2@2"));
  EXPECT_DOUBLE_EQ(expect<floating_point>(z3->real())->value, -0.8322936730942848);
  EXPECT_DOUBLE_EQ(expect<floating_point>(z3->imaginary())->value, 1.8185948536513634);
}

TEST_F(io, write_complex) {
  EXPECT_EQ(to_string_simple(ctx, make_rectangular(ctx, integer_to_ptr(1), integer_to_ptr(1))), "1+i");
  EXPECT_EQ(to_string_simple(ctx, make_rectangular(ctx, integer_to_ptr(0), integer_to_ptr(1))), "+i");
  EXPECT_EQ(to_string_simple(ctx, make_rectangular(ctx, integer_to_ptr(0), integer_to_ptr(2))), "2i");
  EXPECT_EQ(to_string_simple(ctx, make_rectangular(ctx, integer_to_ptr(1), integer_to_ptr(2))), "1+2i");

  auto flt_zero = make<floating_point>(ctx, 0.0);
  auto flt_one = make<floating_point>(ctx, 1.0);
  auto flt_two = make<floating_point>(ctx, 2.0);
  EXPECT_EQ(to_string_simple(ctx, make_rectangular(ctx, integer_to_ptr(1), flt_one)), "1+1.0i");
  EXPECT_EQ(to_string_simple(ctx, make_rectangular(ctx, integer_to_ptr(0), flt_one)), "1.0i");
  EXPECT_EQ(to_string_simple(ctx, make_rectangular(ctx, flt_zero, flt_one)), "0.0+1.0i");
  EXPECT_EQ(to_string_simple(ctx, make_rectangular(ctx, integer_to_ptr(0), flt_two)), "2.0i");
  EXPECT_EQ(to_string_simple(ctx, make_rectangular(ctx, flt_zero, flt_two)), "0.0+2.0i");
  EXPECT_EQ(to_string_simple(ctx, make_rectangular(ctx, integer_to_ptr(1), flt_two)), "1+2.0i");

  EXPECT_EQ(to_string_simple(ctx, make_rectangular(ctx, integer_to_ptr(1), integer_to_ptr(-1))), "1-i");
  EXPECT_EQ(to_string_simple(ctx, make_rectangular(ctx, integer_to_ptr(0), integer_to_ptr(-1))), "-i");
  EXPECT_EQ(to_string_simple(ctx, make_rectangular(ctx, integer_to_ptr(0), integer_to_ptr(-2))), "-2i");
  EXPECT_EQ(to_string_simple(ctx, make_rectangular(ctx, integer_to_ptr(1), integer_to_ptr(-2))), "1-2i");

  auto pos_inf = make<floating_point>(ctx, floating_point::positive_infinity);
  auto pos_nan = make<floating_point>(ctx, floating_point::positive_nan);
  EXPECT_EQ(to_string_simple(ctx, make_rectangular(ctx, integer_to_ptr(0), pos_inf)), "+inf.0i");
  EXPECT_EQ(to_string_simple(ctx, make_rectangular(ctx, integer_to_ptr(0), pos_nan)), "+nan.0i");
  EXPECT_EQ(to_string_simple(ctx, make_rectangular(ctx, integer_to_ptr(1), pos_inf)), "1+inf.0i");
  EXPECT_EQ(to_string_simple(ctx, make_rectangular(ctx, integer_to_ptr(1), pos_nan)), "1+nan.0i");
  EXPECT_EQ(to_string_simple(ctx, make_rectangular(ctx, pos_inf, integer_to_ptr(1))), "+inf.0+i");
  EXPECT_EQ(to_string_simple(ctx, make_rectangular(ctx, pos_nan, integer_to_ptr(1))), "+nan.0+i");
  EXPECT_EQ(to_string_simple(ctx, make_rectangular(ctx, pos_inf, integer_to_ptr(2))), "+inf.0+2i");
  EXPECT_EQ(to_string_simple(ctx, make_rectangular(ctx, pos_nan, integer_to_ptr(2))), "+nan.0+2i");

  auto neg_inf = make<floating_point>(ctx, floating_point::negative_infinity);
  auto neg_nan = make<floating_point>(ctx, floating_point::negative_nan);
  EXPECT_EQ(to_string_simple(ctx, make_rectangular(ctx, integer_to_ptr(0), neg_inf)), "-inf.0i");
  EXPECT_EQ(to_string_simple(ctx, make_rectangular(ctx, integer_to_ptr(0), neg_nan)), "-nan.0i");
  EXPECT_EQ(to_string_simple(ctx, make_rectangular(ctx, integer_to_ptr(1), neg_inf)), "1-inf.0i");
  EXPECT_EQ(to_string_simple(ctx, make_rectangular(ctx, integer_to_ptr(1), neg_nan)), "1-nan.0i");
  EXPECT_EQ(to_string_simple(ctx, make_rectangular(ctx, neg_inf, integer_to_ptr(1))), "-inf.0+i");
  EXPECT_EQ(to_string_simple(ctx, make_rectangular(ctx, neg_nan, integer_to_ptr(1))), "-nan.0+i");
  EXPECT_EQ(to_string_simple(ctx, make_rectangular(ctx, neg_inf, integer_to_ptr(2))), "-inf.0+2i");
  EXPECT_EQ(to_string_simple(ctx, make_rectangular(ctx, neg_nan, integer_to_ptr(2))), "-nan.0+2i");
}

TEST_F(io, number_to_string) {
  EXPECT_EQ(number_to_string(ctx, integer_to_ptr(0)), "0");
  EXPECT_EQ(number_to_string(ctx, integer_to_ptr(2)), "2");
  EXPECT_EQ(number_to_string(ctx, integer_to_ptr(-23)), "-23");
  EXPECT_EQ(number_to_string(ctx, make<fraction>(ctx, integer_to_ptr(2), integer_to_ptr(3))), "2/3");
  EXPECT_EQ(number_to_string(ctx, make<floating_point>(ctx, 2.5)), "2.5");
  EXPECT_EQ(number_to_string(ctx, make_rectangular(ctx, integer_to_ptr(2), integer_to_ptr(3))), "2+3i");
  EXPECT_EQ(number_to_string(ctx, make<floating_point>(ctx, floating_point::positive_infinity)), "+inf.0");
  EXPECT_EQ(number_to_string(ctx, make<floating_point>(ctx, floating_point::positive_nan)), "+nan.0");

  EXPECT_EQ(number_to_string(ctx, integer_to_ptr(-23), 8), "-27");
  EXPECT_EQ(number_to_string(ctx, make<fraction>(ctx, integer_to_ptr(10), integer_to_ptr(11)), 8), "12/13");
  EXPECT_EQ(number_to_string(ctx, make<floating_point>(ctx, 12.5), 8), "#i31/2");
  EXPECT_EQ(number_to_string(ctx, make<floating_point>(ctx, floating_point::positive_infinity), 8), "+inf.0");

  EXPECT_EQ(number_to_string(ctx, make<floating_point>(ctx, 2.6875), 16), "#i2b/10");
}

TEST_F(io, string_to_number) {
  EXPECT_TRUE(equal(string_to_number(ctx, "100"), integer_to_ptr(100)));
  EXPECT_TRUE(equal(string_to_number(ctx, "100", 16), integer_to_ptr(256)));
  EXPECT_TRUE(equal(string_to_number(ctx, "1e2"), make<floating_point>(ctx, 100.0)));
  EXPECT_TRUE(equal(string_to_number(ctx, "2/3"), make<fraction>(ctx, integer_to_ptr(2), integer_to_ptr(3))));
  EXPECT_TRUE(equal(string_to_number(ctx, "#o10", 16), integer_to_ptr(8)));

  EXPECT_EQ(string_to_number(ctx, "invalid"), ctx.constants->f);
  EXPECT_EQ(string_to_number(ctx, "12+"), ctx.constants->f);
  EXPECT_EQ(string_to_number(ctx, "#xabgh"), ctx.constants->f);
}

TEST_F(io, string_with_embedded_newlines) {
  EXPECT_EQ(datum_to_string(ctx, read(R"("foo\nbar")")), R"("foo\nbar")");
  EXPECT_EQ(datum_to_string(ctx, read(R"("foo\rbar")")), R"("foo\rbar")");
  EXPECT_EQ(datum_to_string(ctx, read(R"("foo\r\nbar")")), R"("foo\r\nbar")");
}

TEST_F(io, read_returns_eof_on_end) {
  EXPECT_EQ(insider::read(ctx, ""), ctx.constants->eof);
}

TEST_F(io, read_syntax_returns_eof_on_end) {
  EXPECT_EQ(read_syntax(ctx, ""), ctx.constants->eof);
}

#define EXPECT_SYMBOL_EQ(x, y) \
  do EXPECT_EQ(datum_to_string(ctx, ctx.intern(x)), y); while (false)

TEST_F(io, write_simple_symbol) {
  EXPECT_SYMBOL_EQ("foo", "foo");
}

TEST_F(io, write_symbols_that_require_pipes) {
  EXPECT_SYMBOL_EQ("", "||");
  EXPECT_SYMBOL_EQ(".", "|.|");
  EXPECT_SYMBOL_EQ("123", "|123|");
  EXPECT_SYMBOL_EQ(".123", "|.123|");
  EXPECT_SYMBOL_EQ("+123", "|+123|");
  EXPECT_SYMBOL_EQ("-123", "|-123|");
  EXPECT_SYMBOL_EQ("+i", "|+i|");
  EXPECT_SYMBOL_EQ("-i", "|-i|");
  EXPECT_SYMBOL_EQ("+nan.0", "|+nan.0|");
  EXPECT_SYMBOL_EQ("+NaN.0", "|+NaN.0|");
  EXPECT_SYMBOL_EQ("+inf.0", "|+inf.0|");
  EXPECT_SYMBOL_EQ("-inf.0", "|-inf.0|");
  EXPECT_SYMBOL_EQ("foo bar", "|foo bar|");
  EXPECT_SYMBOL_EQ(R"(foo\bar)", R"(|foo\\bar|)");
  EXPECT_SYMBOL_EQ("foo|bar", R"(|foo\|bar|)");
  EXPECT_SYMBOL_EQ("foo)(bar", "|foo)(bar|");
}

#undef EXPECT_SYMBOL_EQ

#define EXPECT_KEYWORD_EQ(x, y) \
  do EXPECT_EQ(datum_to_string(ctx, ctx.intern_keyword(x)), y); while (false)

TEST_F(io, write_simple_keyword) {
  EXPECT_KEYWORD_EQ("foo", "#:foo");
  EXPECT_KEYWORD_EQ("123", "#:123");
}

TEST_F(io, write_keyword_that_requires_pipes) {
  EXPECT_KEYWORD_EQ("", "#:||");
  EXPECT_KEYWORD_EQ(".", "#:|.|");
  EXPECT_KEYWORD_EQ("foo bar", "#:|foo bar|");
  EXPECT_KEYWORD_EQ("foo|bar", "#:|foo\\|bar|");
}

#undef EXPECT_KEYWORD_EQ
