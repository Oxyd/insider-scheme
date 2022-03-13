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

(define (test-numeric)
  (test-group "numeric"
    (test-comparison)
    (test-division)
    (test-fraction)))

(when-main-module
 (test-numeric))
