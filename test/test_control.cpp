#include "scheme_fixture.hpp"

#include "define_procedure.hpp"
#include "error.hpp"
#include "from_scheme.hpp"
#include "to_scheme.hpp"

using namespace insider;

struct control : scheme_fixture { };

TEST_F(control, simple_escape) {
  std::string product_module = R"(
    (import (insider internal))

    (define product
      (lambda (list)
        (define go
          (lambda (l return-cont)
            (if (eq? l '())
                1
                (if (= (car l) 0)
                    (replace-stack! return-cont 0)
                    (* (car l) (go (cdr l) return-cont))))))
        (capture-stack
          (lambda (return-cont)
            (go list return-cont)))))
  )";

  auto result1 = eval_module(product_module + "(product '(1 2 0 3))");
  EXPECT_EQ(expect<integer>(result1).value(), 0);

  auto result2 = eval_module(product_module + "(product '(1 2 3 4))");
  EXPECT_EQ(expect<integer>(result2).value(), 24);
}

TEST_F(control, return_to_previous_frame) {
  auto result = eval_module(R"(
    (import (insider internal))

    (define val 5)
    (define cont #f)
    (define result '())

    (define f
      (lambda ()
        (capture-stack
          (lambda (c)
            (set! cont c)))
        (> val 0)))

    (define go
      (lambda ()
        (let ((r (f)))
          (set! result (cons r result))
          (if (> val 0)
              (begin
                (set! val 0)
                (replace-stack! cont #f))
              #t))))

    (go)
    result
  )");
  ASSERT_TRUE(is_list(result));
  EXPECT_EQ(expect<insider::boolean>(car(expect<pair>(result))), ctx.constants->f.get());
  EXPECT_EQ(expect<insider::boolean>(cadr(expect<pair>(result))), ctx.constants->t.get());
}

TEST_F(control, jump_to_inner_continuation) {
  auto result = eval(R"(
    (+ 1 (capture-stack
           (lambda (outer)
             (+ 10
                (capture-stack
                  (lambda (inner)
                    (replace-stack! inner 5)))))))
  )");
  EXPECT_EQ(expect<integer>(result).value(), 16);
}

TEST_F(control, jump_to_outer_continuation) {
  auto result = eval(R"(
    (+ 1 (capture-stack
           (lambda (outer)
             (+ 10
                (capture-stack
                  (lambda (inner)
                    (replace-stack! outer 5)))))))
  )");
  EXPECT_EQ(expect<integer>(result).value(), 6);
}

TEST_F(control, top_level_parameter_values) {
  auto result = eval_module(R"(
    (import (insider internal))

    (define p (create-parameter-tag 0))
    (set-parameter-value! p 1)
    (find-parameter-value p)
  )");
  EXPECT_EQ(expect<integer>(result).value(), 1);
}

TEST_F(control, parameterize_overrides_value) {
  auto result = eval_module(R"(
    (import (insider internal))

    (define p (create-parameter-tag 0))

    (define f
      (lambda ()
        (call-parameterized p 2
          (lambda ()
            (find-parameter-value p)))))

    (set-parameter-value! p 1)
    (f)
  )");
  EXPECT_EQ(expect<integer>(result).value(), 2);
}

TEST_F(control, parameterization_has_no_effect_outside_frame) {
  auto result = eval_module(R"(
    (import (insider internal))

    (define p (create-parameter-tag 0))

    (define f
      (lambda ()
        (call-parameterized p 2
          (lambda ()
            (find-parameter-value p)))))

    (set-parameter-value! p 1)
    (f)
    (find-parameter-value p)
  )");
  EXPECT_EQ(expect<integer>(result).value(), 1);
}

TEST_F(control, call_parameterized_can_nest) {
  auto result = eval(R"(
    (let ((p (create-parameter-tag 0)))
      (call-parameterized p 1
        (lambda ()
          (call-parameterized p 2
            (lambda ()
              (find-parameter-value p))))))
  )");
  EXPECT_EQ(expect<integer>(result).value(), 2);
}

TEST_F(control, call_continuable_works_like_call) {
  define_procedure(ctx, "f", ctx.internal_module, true,
                   [] (context& ctx, ptr<> f, ptr<> arg) {
                     return call_continuable(ctx, f, {arg},
                                             [] (context& ctx, ptr<> result) {
                                               return to_scheme(ctx, expect<integer>(result).value() * 2);
                                             });
                   });

  auto result = eval("(f (lambda (x) (+ x 1)) 3)");
  EXPECT_EQ(expect<integer>(result).value(), 8);
}

TEST_F(control, call_continuable_allows_jump_up) {
  bool continuation_called = false;
  define_procedure<ptr<>(context&, ptr<>, ptr<>)>(
    ctx, "f", ctx.internal_module, true,
    [&] (context& ctx, ptr<> f, ptr<> arg) {
      return call_continuable(ctx, f, {arg},
                              [&] (context& ctx, ptr<> result) {
                                continuation_called = true;
                                return to_scheme(ctx, expect<integer>(result).value() * 2);
                              });
    }
  );
  auto result = eval(R"(
    (capture-stack
      (lambda (stack)
        (f (lambda (x) (replace-stack! stack 0)) 3)))
  )");
  EXPECT_FALSE(continuation_called);
  EXPECT_EQ(expect<integer>(result).value(), 0);
}

TEST_F(control, call_continuable_allows_jump_back_in) {
  unsigned continuation_counter = 0;
  define_procedure<ptr<>(context&, ptr<>, ptr<>)>(
    ctx, "f", ctx.internal_module, true,
    [&] (context& ctx, ptr<> f, ptr<> arg) {
      return call_continuable(ctx, f, {arg},
                              [&] (context& ctx, ptr<> result) {
                                ++continuation_counter;
                                return to_scheme(ctx, expect<integer>(result).value() * 2);
                              });
    }
  );
  auto result = eval_module(R"(
    (import (insider internal))

    (define cont #f)
    (define result '())
    (define cont-called? #f)

    (define call-and-append-to-result
      (lambda ()
        (let ((r (f (lambda (x)
                      (if (eq? cont #f)
                          (capture-stack
                            (lambda (stack)
                              (set! cont stack)
                              (+ x 1)))))
                    3)))
          (set! result (cons r result)))))

    (call-and-append-to-result)
    (if (eq? cont-called? #f)
        (begin
          (set! cont-called? #t)
          (replace-stack! cont 10))
        result)
  )");
  ASSERT_TRUE(is_list(result));
  auto p = assume<pair>(result);
  EXPECT_EQ(expect<integer>(car(p)).value(), 20);
  EXPECT_EQ(expect<integer>(cadr(p)).value(), 8);
  EXPECT_EQ(continuation_counter, 2);
}

TEST_F(control, call_continuable_can_be_used_twice) {
  define_procedure(
    ctx, "f", ctx.internal_module, true,
    [] (context& ctx, ptr<> f, ptr<> g) {
      return call_continuable(
        ctx, f, {to_scheme(ctx, 2)},
        [g = track(ctx, g)] (context& ctx, ptr<> result) {
          return call_continuable(
            ctx, g.get(), {result},
            [] (context& ctx, ptr<> result) {
              return to_scheme(ctx, 2 * from_scheme<int>(ctx, result));
            }
          ).get();
        }
      );
    }
  );
  auto result = eval("(f (lambda (x) (+ x 1)) (lambda (x) (+ x 2)))");
  EXPECT_EQ(expect<integer>(result).value(), 2 * ((2 + 1) + 2));
}

TEST_F(control, continuation_jump_goes_to_the_correct_call_continuable_call) {
  // (define (f g h)
  //   (h (g)))

  define_procedure(
    ctx, "f", ctx.internal_module, true,
    [] (context& ctx, ptr<> g, ptr<> h) {
      return call_continuable(
        ctx, g, {},
        [h = track(ctx, h)] (context& ctx, ptr<>) {
          return call_continuable(
            ctx, h.get(), {},
            [] (context&, ptr<> r) { return r; }
          ).get();
        }
      ).get();
    }
  );

  auto result = eval(R"(
    (let ((in-g #f) (g-count 0) (h-count 0) (jumped? #f))
      (f
        (lambda ()
          (capture-stack
            (lambda (k)
              (set! in-g k)))
          (set! g-count (+ g-count 1)))
        (lambda ()
          (set! h-count (+ h-count 1))))
      (if (eq? jumped? #f)
          (begin
            (set! jumped? #t)
            (replace-stack! in-g 0))
          (cons g-count (cons h-count '()))))
  )");
  auto result_v = list_to_std_vector(result);
  ASSERT_EQ(result_v.size(), 2);
  EXPECT_EQ(expect<integer>(result_v[0]).value(), 2);
  EXPECT_EQ(expect<integer>(result_v[1]).value(), 2);
}

TEST_F(control, barrier_prevents_jump_out) {
  EXPECT_THROW(
    eval(R"(
      (capture-stack
        (lambda (out)
          (call-with-continuation-barrier #f #t
            (lambda ()
              (replace-stack! out #f)))))
    )"),
    std::runtime_error
  );
}

TEST_F(control, barrier_does_not_prevent_jumps_within_it) {
  auto result = eval(R"(
    (capture-stack
      (lambda (outer)
        (call-with-continuation-barrier #f #t
          (lambda ()
            (capture-stack
              (lambda (inner)
                (replace-stack! inner #t)
                #f))))))
  )");
  EXPECT_EQ(result, ctx.constants->t.get());
}

TEST_F(control, barrier_prevents_jump_in) {
  EXPECT_THROW(
    eval(R"(
      (let ((inner #f))
        (call-with-continuation-barrier #t #f
          (lambda ()
            (capture-stack
              (lambda (k)
                (set! inner k)
                #f))))
        (inner #t))
      )"),
    std::runtime_error
  );
}

TEST_F(control, call_with_continuation_barrier_erects_a_barrier) {
  define_procedure(
    ctx, "f", ctx.internal_module, true,
    [] (context& ctx, ptr<> g) {
      return call_with_continuation_barrier(ctx, g, {});
    }
  );

  EXPECT_THROW(
    eval(R"(
      (capture-stack
        (lambda (exit)
          (f (lambda () (replace-stack! exit 0)))))
    )"),
    std::runtime_error
  );
}

TEST_F(control, dynamic_wind_calls_all_three_thunks_in_order) {
  auto r = eval_module(R"(
    (import (insider internal))

    (define result '())
    (define push!
      (lambda (x)
        (set! result (cons x result))))

    (dynamic-wind
      (lambda () (push! 1))
      (lambda () (push! 2))
      (lambda () (push! 3)))

    result
  )");

  auto result = list_to_std_vector(r);
  ASSERT_EQ(result.size(), 3);
  EXPECT_EQ(expect<integer>(result[2]).value(), 1);
  EXPECT_EQ(expect<integer>(result[1]).value(), 2);
  EXPECT_EQ(expect<integer>(result[0]).value(), 3);
}

TEST_F(control, dynamic_wind_calls_post_when_jumping_out) {
  auto result = eval(R"(
    (let ((result #f))
      (capture-stack
        (lambda (exit)
          (dynamic-wind
            (lambda () #void)
            (lambda ()
              (replace-stack! exit 0))
            (lambda ()
              (set! result #t)))))
      result)
  )");
  EXPECT_EQ(result, ctx.constants->t.get());
}

TEST_F(control, dynamic_wind_calls_pre_when_jumping_in) {
  auto result = eval(R"(
    (let ((in-count 0) (inside #f) (jumped? #f))
      (dynamic-wind
        (lambda ()
          (set! in-count (+ in-count 1)))
        (lambda ()
          (capture-stack
            (lambda (k)
              (set! inside k)
              #void)))
        (lambda () #void))
      (if (eq? jumped? #f)
          (begin
            (set! jumped? #t)
            (replace-stack! inside 0))
          in-count))
  )");
  EXPECT_EQ(expect<integer>(result).value(), 2);
}

TEST_F(control, dynamic_wind_calls_post_after_second_return) {
  auto result = eval(R"(
    (let ((out-count 0) (inside #f) (jumped? #f))
      (dynamic-wind
        (lambda () #void)
        (lambda ()
          (capture-stack
            (lambda (k)
              (set! inside k)
              #void)))
        (lambda ()
          (set! out-count (+ out-count 1))))
      (if (eq? jumped? #f)
          (begin
            (set! jumped? #t)
            (replace-stack! inside 0))
          out-count))
  )");
  EXPECT_EQ(expect<integer>(result).value(), 2);
}

TEST_F(control, dynamic_winds_can_nest) {
  auto result = eval(R"(
    (let ((pre-outer-counter 0) (pre-inner-counter 0) (thunk-outer-counter 0)
          (post-outer-counter 0) (post-inner-counter 0) (thunk-inner-counter 0)
          (inner-cont #f) (jumped? #f)
          (list (lambda l l)))
      (dynamic-wind
        (lambda () (set! pre-outer-counter (+ pre-outer-counter 1)))
        (lambda ()
          (set! thunk-outer-counter (+ thunk-outer-counter 1))
          (dynamic-wind
            (lambda () (set! pre-inner-counter (+ pre-inner-counter 1)))
            (lambda ()
              (set! thunk-inner-counter (+ thunk-inner-counter 1))
              (capture-stack
                (lambda (k)
                  (set! inner-cont k))))
            (lambda () (set! post-inner-counter (+ post-inner-counter 1)))))
        (lambda () (set! post-outer-counter (+ post-outer-counter 1))))
      (if (eq? jumped? #f)
          (begin
            (set! jumped? #t)
            (replace-stack! inner-cont 0))
          (list pre-outer-counter pre-inner-counter
                thunk-outer-counter thunk-inner-counter
                post-outer-counter post-inner-counter)))
  )");
  auto result_v = list_to_std_vector(result);
  ASSERT_EQ(result_v.size(), 6);
  EXPECT_EQ(expect<integer>(result_v[0]).value(), 2);
  EXPECT_EQ(expect<integer>(result_v[1]).value(), 2);
  EXPECT_EQ(expect<integer>(result_v[2]).value(), 1);
  EXPECT_EQ(expect<integer>(result_v[3]).value(), 1);
  EXPECT_EQ(expect<integer>(result_v[4]).value(), 2);
  EXPECT_EQ(expect<integer>(result_v[5]).value(), 2);
}

TEST_F(control, dynamic_wind_uses_correct_dynamic_environment) {
  EXPECT_NO_THROW(
    eval(R"(
      (let ((p (create-parameter-tag 0))
            (inner #f)
            (jumped-out? #f) (jumped-in? #f))
        (let ((expect (lambda (value)
                        (if (eq? (find-parameter-value p) value)
                            #t
                            (raise (make-error "Wrong value" (cons value (cons (find-parameter-value p) '()))))))))
          (capture-stack
            (lambda (out)
              (call-parameterized p 1
                (lambda ()
                  (dynamic-wind
                    (lambda () (expect 1))
                    (lambda ()
                      (expect 1)
                      (call-parameterized p 2
                        (lambda ()
                          (dynamic-wind
                            (lambda () (expect 2))
                            (lambda ()
                              (expect 2)
                              (capture-stack
                                (lambda (in)
                                  (set! inner in)
                                  (if (eq? jumped-out? #f)
                                      (begin
                                        (set! jumped-out? #t)
                                        (replace-stack! out #void))))))
                            (lambda () (expect 2))))))
                    (lambda () (expect 1)))))))
          (if (eq? jumped-in? #f)
              (begin
                (set! jumped-in? #t)
                (replace-stack! inner #void)))))
    )")
  );
}

TEST_F(control, raise_continuable_jumps_to_handler) {
  auto result = eval(R"(
    (with-exception-handler
      (lambda (e)
        'result-from-handler)
      (lambda ()
        (raise-continuable 'ignored)))
  )");
  EXPECT_EQ(expect<symbol>(result)->value(), "result-from-handler");
}

TEST_F(control, with_exception_handler_returns_result_of_thunk) {
  auto result = eval(R"(
    (with-exception-handler
      (lambda (e)
        'result-from-handler)
      (lambda ()
        'result-from-thunk))
  )");
  EXPECT_EQ(expect<symbol>(result)->value(), "result-from-thunk");
}

TEST_F(control, calling_a_handler_restores_the_previous_handler) {
  auto result = eval(R"(
    (with-exception-handler
      (lambda (e)
        'outer-handler)
      (lambda ()
        (with-exception-handler
          (lambda (e)
            (raise-continuable e))
          (lambda ()
            (raise-continuable 'error)))))
  )");
  EXPECT_EQ(expect<symbol>(result)->value(), "outer-handler");
}

TEST_F(control, with_exception_handler_can_nest_several_times) {
  auto result = eval(R"(
    (with-exception-handler
      (lambda (e) (cons 'outermost-handler e))
      (lambda ()
        (with-exception-handler
          (lambda (e) (raise-continuable (cons 'middle-handler e)))
          (lambda ()
            (with-exception-handler
              (lambda (e) (raise-continuable (cons 'inner-handler e)))
              (lambda ()
                (raise-continuable '(raise))))))))
  )");
  EXPECT_TRUE(equal(ctx, result, read("(outermost-handler middle-handler inner-handler raise)")));
}

TEST_F(control, raise_continuable_goes_directly_to_builtin_handler_if_no_with_exception_handler) {
  try {
    eval("(raise-continuable 'exception)");
  } catch (scheme_exception& e) {
    EXPECT_EQ(expect<symbol>(e.object)->value(), "exception");
    SUCCEED();
    return;
  }

  FAIL();
}

TEST_F(control, exception_from_handler_goes_to_builtin_handler_if_no_other_handler) {
  try {
    eval(R"(
      (with-exception-handler
        (lambda (e)
          (raise-continuable e))
        (lambda ()
          (raise-continuable 'exception)))
    )");
  } catch (scheme_exception& e) {
    EXPECT_EQ(expect<symbol>(e.object)->value(), "exception");
    SUCCEED();
    return;
  }

  FAIL();
}

TEST_F(control, raise_goes_to_exception_handler) {
  auto result = eval(R"(
    (capture-stack
      (lambda (exit)
        (with-exception-handler
          (lambda (e)
            (replace-stack! exit e))
          (lambda ()
            (raise 'error)))))
  )");
  EXPECT_EQ(expect<symbol>(result)->value(), "error");
}

TEST_F(control, raise_raises_another_error_when_handler_returns) {
  auto result = eval(R"(
    (capture-stack
      (lambda (exit)
        (with-exception-handler
          (lambda (e)
            (replace-stack! exit e))
          (lambda ()
            (with-exception-handler
              (lambda (e)
                'unhandled)
              (lambda ()
                (raise 'error)))))))
  )");
  EXPECT_EQ(expect<symbol>(expect<uncaught_exception>(result)->inner_exception)->value(), "error");
}

TEST_F(control, raise_goes_to_builtin_error_handler_when_handler_returns) {
  try {
    eval(R"(
      (with-exception-handler
        (lambda (e)
          'unhandled)
        (lambda ()
          (raise 'error)))
    )");
  } catch (scheme_exception& e) {
    auto ue = expect<uncaught_exception>(e.object);
    EXPECT_EQ(expect<symbol>(ue->inner_exception)->value(), "error");
    SUCCEED();
    return;
  }

  FAIL();
}

TEST_F(control, cxx_exception_becomes_scheme_exception) {
  define_procedure(ctx, "f", ctx.internal_module, true,
                   [] { throw std::runtime_error{"foo"}; });
  auto result = eval(R"(
    (capture-stack
      (lambda (return)
        (with-exception-handler
          (lambda (e)
            (replace-stack! return e))
          (lambda ()
            (f)))))
  )");

  ASSERT_TRUE(is<cxx_exception>(result));
  try {
    assume<cxx_exception>(result)->rethrow();
  } catch (std::runtime_error& e) {
    EXPECT_EQ(e.what(), std::string{"foo"});
    SUCCEED();
    return;
  }

  FAIL();
}

TEST_F(control, throwing_scheme_exception_raises_it_in_vm) {
  define_procedure(ctx, "f", ctx.internal_module, true,
                   [] (context& ctx) {
                     throw make<error>(ctx, make<string>(ctx, "hi"), ctx.constants->null.get());
                   });
  auto result = eval(R"(
    (capture-stack
      (lambda (return)
        (with-exception-handler
          (lambda (e)
            (replace-stack! return e))
          (lambda ()
            (f)))))
  )");
  EXPECT_TRUE(is<error>(result));
}

TEST_F(control, cxx_exception_passes_through_if_not_handled) {
  define_procedure(ctx, "f", ctx.internal_module, true,
                   [] { throw std::runtime_error{"foo"}; });
  try {
    eval("(f)");
  } catch (std::runtime_error& e) {
    EXPECT_EQ(e.what(), std::string{"foo"});
    SUCCEED();
    return;
  }

  FAIL();
}

TEST_F(control, apply_with_single_argument) {
  auto result = eval("(apply + '(1 2 3))");
  EXPECT_EQ(expect<integer>(result).value(), 6);
}

TEST_F(control, apply_with_multiple_arguments) {
  auto result = eval(R"(
    (let ((f (lambda (a b c d e)
               (+ (* 2 a)
                  (* 3 b)
                  (* 5 c)
                  (* 7 d)
                  (* 11 e)))))
      (apply f 1 2 '(3 4 5)))
  )");
  EXPECT_EQ(expect<integer>(result).value(), 106);
}

TEST_F(control, call_with_values_single_value) {
  auto result = eval(R"(
    (call-with-values
      (lambda () 4)
      (lambda (x) x))
  )");
  EXPECT_EQ(expect<integer>(result).value(), 4);
}

TEST_F(control, call_with_values_multiple_values) {
  auto result = eval(R"(
    (call-with-values
      (lambda () (values 3 4))
      (lambda (x y) (+ x y)))
  )");
  EXPECT_EQ(expect<integer>(result).value(), 7);
}

TEST_F(control, call_with_values_zero_values) {
  auto result = eval(R"(
    (call-with-values
      (lambda () (values))
      (lambda () 8))
  )");
  EXPECT_EQ(expect<integer>(result).value(), 8);
}

TEST_F(control, values_with_single_value_is_identity) {
  auto result = eval("(values 2)");
  EXPECT_EQ(expect<integer>(result).value(), 2);
}

TEST_F(control, multiple_values_delivered_from_continuation_jump) {
  auto result = eval(R"(
    (call-with-values
      (lambda ()
        (capture-stack
          (lambda (k)
            (replace-stack! k (values 2 3)))))
      (lambda (x y)
        (+ x y)))
  )");
  EXPECT_EQ(expect<integer>(result).value(), 5);
}

TEST_F(control, turn_list_to_multiple_values) {
  auto result = eval(R"(
    (call-with-values
      (lambda ()
        (apply values '(1 2 3)))
      (lambda (x y z)
        (+ x y z)))
  )");
  EXPECT_EQ(expect<integer>(result).value(), 6);
}

TEST_F(control, continuation_jump_to_native_tail_call) {
  define_procedure(ctx, "call-two", ctx.internal_module, true,
                   [] (context& ctx, ptr<> f, ptr<> g) {
                     return call_continuable(
                       ctx, f, {},
                       [g = track(ctx, g)] (context& ctx, ptr<> result) {
                         return tail_call(ctx, g.get(), {result}).get();
                       }
                     );
                   });

  define_procedure(ctx, "f", ctx.internal_module, true,
                   [] (ptr<> value) {
                     return value;
                   });

  auto result = eval_module(R"(
    (import (insider internal))

    (call-two
      (lambda ()
        (capture-stack
          (lambda (k)
            (replace-stack! k 4))))
      (lambda (x)
        (f x)))
  )");
  EXPECT_EQ(expect<integer>(result).value(), 4);
}

TEST_F(control, call_parameterized_with_continuation_barrier_sets_parameter_in_scheme_frame) {
  auto tag = track(ctx, create_parameter_tag(ctx, integer_to_ptr(0)));
  define_top_level(ctx, "p", ctx.internal_module, true, tag.get());

  auto get_value = eval(R"(
    (lambda ()
      (find-parameter-value p))
  )");

  auto result1 = call_with_continuation_barrier(ctx, get_value, {});
  EXPECT_EQ(expect<integer>(result1).value(), 0);

  auto result2 = call_parameterized_with_continuation_barrier(ctx, get_value, {}, tag.get(), integer_to_ptr(4));
  EXPECT_EQ(expect<integer>(result2).value(), 4);
}

TEST_F(control, call_parameterized_with_continuation_brrier_sets_parameter_in_native_frame) {
  auto tag = track(ctx, create_parameter_tag(ctx, integer_to_ptr(0)));
  define_top_level(ctx, "p", ctx.internal_module, true, tag.get());

  auto get_value = make_tracked<native_procedure>(
    ctx,
    [&] (context& ctx, object_span) {
      return find_parameter_value(ctx, tag.get());
    }
  );

  auto result1 = call_with_continuation_barrier(ctx, get_value.get(), {});
  EXPECT_EQ(expect<integer>(result1).value(), 0);

  auto result2 = call_parameterized_with_continuation_barrier(ctx, get_value.get(), {}, tag.get(), integer_to_ptr(4));
  EXPECT_EQ(expect<integer>(result2).value(), 4);
}

TEST_F(control, native_parameterize_sets_parameter_in_scheme_frame) {
  auto tag = track(ctx, create_parameter_tag(ctx, integer_to_ptr(0)));
  define_top_level(ctx, "p", ctx.internal_module, true, tag.get());

  auto get_value = eval(R"(
    (lambda ()
      (find-parameter-value p))
  )");

  {
    parameterize p{ctx, tag.get(), integer_to_ptr(4)};
    auto result1 = call_with_continuation_barrier(ctx, get_value, {});
    EXPECT_EQ(expect<integer>(result1).value(), 4);
  }

  auto result2 = call_with_continuation_barrier(ctx, get_value, {});
  EXPECT_EQ(expect<integer>(result2).value(), 0);
}

TEST_F(control, native_parameterization_sets_parameter_in_native_frame) {
  auto tag = track(ctx, create_parameter_tag(ctx, integer_to_ptr(0)));
  define_top_level(ctx, "p", ctx.internal_module, true, tag.get());

  auto get_value = make_tracked<native_procedure>(
    ctx,
    [&] (context& ctx, object_span) {
      return find_parameter_value(ctx, tag.get());
    }
  );

  {
    parameterize p{ctx, tag.get(), integer_to_ptr(4)};
    auto result1 = call_with_continuation_barrier(ctx, get_value.get(), {});
    EXPECT_EQ(expect<integer>(result1).value(), 4);
  }

  auto result2 = call_with_continuation_barrier(ctx, get_value.get(), {});
  EXPECT_EQ(expect<integer>(result2).value(), 0);
}

TEST_F(control, native_parameterization_sets_parameter_value_when_called_from_scheme) {
  auto tag = track(ctx, create_parameter_tag(ctx, integer_to_ptr(0)));
  define_top_level(ctx, "p", ctx.internal_module, true, tag.get());

  auto get_value = eval(R"(
    (lambda ()
      (find-parameter-value p))
  )");

  define_procedure<ptr<>(context&)>(ctx, "get-value-parameterized", ctx.internal_module, true,
                                    [&] (context& ctx) {
                                      parameterize p{ctx, tag.get(), integer_to_ptr(4)};
                                      return call_with_continuation_barrier(ctx, get_value, {});
                                    });

  auto result = eval("(get-value-parameterized)");
  EXPECT_EQ(expect<integer>(result).value(), 4);
}
