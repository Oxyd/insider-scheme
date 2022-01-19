(library (insider numeric-test))
(import (insider syntax) (insider test) (insider numeric) (insider control))
(export test-numeric)

(define (comparison)
  (test-group "comparison"
    (test-equal 3 (min 3 4))
    (test (exact? (min 3 4)))
    (test-equal 3.0 (min 3 4.5))
    (test (inexact? (min 3 4.5)))

    (test-equal 4 (max 3 4))
    (test (exact? (max 3 4)))
    (test-equal 4.0 (max 3.9 4))
    (test (inexact? (max 3.9 4)))))

(define (division)
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

(define (test-numeric)
  (test-group "numeric"
    (comparison)
    (division)))

(when-main-module
 (test-numeric))
