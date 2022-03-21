(library (insider numeric-test))
(import (insider syntax) (insider test) (insider numeric) (insider control))
(export test-numeric)

(define (test-comparison)
  (test-group "comparison"
    (test-equal 3 (min 3 4))
    (test (exact? (min 3 4)))
    (test-equal 3.0 (min 3 4.5))
    (test (inexact? (min 3 4.5)))

    (test-equal 4 (max 3 4))
    (test (exact? (max 3 4)))
    (test-equal 4.0 (max 3.9 4))
    (test (inexact? (max 3.9 4)))))

(define (test-division)
  (test-group "division"
    (test-values-equal (2 1) (truncate/ 5 2))
    (test-values-equal (-2 -1) (truncate/ -5 2))
    (test-values-equal (-2 1) (truncate/ 5 -2))
    (test-values-equal (2 -1) (truncate/ -5 -2))

    (test-values-equal (9223372036854775808 1) (truncate/ 18446744073709551617 2))
    (test-values-equal (-9223372036854775808 -1) (truncate/ -18446744073709551617 2))
    (test-values-equal (-9223372036854775808 1) (truncate/ 18446744073709551617 -2))
    (test-values-equal (9223372036854775808 -1) (truncate/ -18446744073709551617 -2))

    (test-values-equal (2 1) (floor/ 5 2))
    (test-values-equal (-3 1) (floor/ -5 2))
    (test-values-equal (-3 -1) (floor/ 5 -2))
    (test-values-equal (2 -1) (floor/ -5 -2))
    (test-values-equal (2.0 1.0) (floor/ 5.0 2))))

(define (test-fraction)
  (test-group "fraction"
    (test-equal 3 (numerator (/ 6 4)))
    (test-equal 2 (denominator (/ 6 4)))
    (test-equal 3.0 (numerator (inexact (/ 6 4))))
    (test-equal 2.0 (denominator (inexact (/ 6 4))))
    (test-equal 1 (denominator 0))
    (test-equal 1.0 (denominator 0.0))))

(define (test-categories)
  (test-group "categories"
    (test (complex? 3+4i))
    (test (complex? 3))
    (test (real? 3))
    (test (real? -2.5+0i))
    (test (real? -2.5+0.0i))
    (test (real? #e1e10))
    (test (real? +inf.0))
    (test (real? +nan.0))
    (test-false (rational? -inf.0))
    (test (rational? 3.5))
    (test (rational? 6/10))
    (test (rational? 6/3))
    (test (integer? 3+0i))
    (test (integer? 3.0))
    (test (integer? 8/4))

    (test (positive? 1))
    (test (positive? 1.0))
    (test (positive? 1/2))
    (test (positive? 0.1))
    (test-false (positive? 0))
    (test-false (positive? -1))
    (test-false (positive? -1/2))

    (test (negative? -1))
    (test (negative? -1.0))
    (test (negative? -1/2))
    (test (negative? -0.1))
    (test-false (negative? 1))
    (test-false (negative? 0))))

(define (test-rounding)
  (test-group "rounding"
    (test-equal -5.0 (floor -4.3))
    (test-equal 3.0 (floor 3.5))
    (test-equal 1 (floor 12/7))
    (test-equal -2 (floor -12/7))

    (test-equal -4.0 (ceiling -4.3))
    (test-equal 4.0 (ceiling 3.5))
    (test-equal 2 (ceiling 12/7))
    (test-equal -1 (ceiling -12/7))

    (test-equal -4.0 (truncate -4.3))
    (test-equal 3.0 (truncate 3.5))
    (test-equal 1 (truncate 12/7))
    (test-equal -1 (truncate -12/7))

    (test-equal -4.0 (round -4.3))
    (test-equal 4.0 (round 3.5))
    (test-equal 2.0 (round 2.5))
    (test-equal 13.0 (round 13.1))
    (test-equal 13.0 (round 12.8))
    (test-equal 4 (round 7/2))
    (test-equal -4 (round -7/2))
    (test-equal 2 (round 12/7))
    (test-equal -2 (round -12/7))
    (test-equal 7 (round 7))))

(define (test-numeric)
  (test-group "numeric"
    (test-comparison)
    (test-division)
    (test-fraction)
    (test-categories)
    (test-rounding)))

(when-main-module
 (test-numeric))
