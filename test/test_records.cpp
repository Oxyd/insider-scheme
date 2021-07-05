#include "scheme_fixture.hpp"

#include "basic_types.hpp"
#include "integer.hpp"
#include "records.hpp"

using namespace insider;

struct records : scheme_fixture { };

TEST_F(records, create_instance_of_record) {
  auto type = make<record_type>(ctx, 0);
  auto inst = make_instance(ctx, type);
  EXPECT_EQ(inst->type(), type);
}

TEST_F(records, access_first_field_of_record) {
  auto type = make<record_type>(ctx, 1);
  auto inst = make_instance(ctx, type);
  inst->set(0, integer_to_ptr(42));
  EXPECT_EQ(expect<integer>(inst->ref(0)).value(), 42);
}

TEST_F(records, access_two_fields_of_record) {
  auto type = make<record_type>(ctx, 2);
  auto inst = make_instance(ctx, type);
  inst->set(0, ctx.intern("first"));
  inst->set(1, ctx.intern("second"));
  EXPECT_EQ(expect<symbol>(inst->ref(0))->value(), "first");
  EXPECT_EQ(expect<symbol>(inst->ref(1))->value(), "second");
}

TEST_F(records, create_record_type_and_instance) {
  auto result = eval(R"(
    (let ((t (make-record-type 1)))
      (let ((r (make-record-instance t)))
        (record-set! r 0 'foo)
        r))
  )");
  EXPECT_EQ(expect<symbol>(expect<record_instance>(result)->ref(0))->value(), "foo");
}

TEST_F(records, set_and_read_record_field) {
  auto result = eval(R"(
    (let ((point-type (make-record-type 2))
          (point.x (lambda (p) (record-ref p 0)))
          (point.y (lambda (p) (record-ref p 1))))
      (let ((p (make-record-instance point-type)))
        (record-set! p 0 3)
        (record-set! p 1 4)
        (+ (* (point.x p) (point.x p))
           (* (point.y p) (point.y p)))))
  )");
  EXPECT_EQ(expect<integer>(result).value(), 25);
}

TEST_F(records, test_record_type) {
  auto result = eval_module(R"(
    (import (insider internal))

    (define foo-type (make-record-type 0))

    (define foo?
      (lambda (x)
        (if (eq? (type x) 'insider::record_instance)
            (if (eq? (record-type x) foo-type)
                #t
                #f)
            #f)))

    (define list (lambda x x))

    (list (foo? (make-record-instance foo-type))
          (foo? foo-type)
          (foo? 'foo))
  )");
  std::vector<ptr<>> result_v = list_to_std_vector(result);
  ASSERT_EQ(result_v.size(), 3);
  EXPECT_EQ(result_v[0], ctx.constants->t.get());
  EXPECT_EQ(result_v[1], ctx.constants->f.get());
  EXPECT_EQ(result_v[2], ctx.constants->f.get());
}
