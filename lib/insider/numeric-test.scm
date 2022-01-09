(library (insider numeric-test))
(import (insider syntax) (insider test) (insider numeric) (insider control))
(export test-numeric)

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
    (division)))

(when-main-module
 (test-numeric))
