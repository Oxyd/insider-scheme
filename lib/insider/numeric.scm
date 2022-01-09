(library (insider numeric))
(import (insider syntax)
        (except (insider internal) define let))
(export
 ;; From core
 + - * / = < <= > >= truncate/ truncate-quotient truncate-remainder
 gcd arithmetic-shift bitwise-and bitwise-or bitwise-not integer? odd? even? zero? number? exp log
 abs floor

 ;; Defined here
 floor/ floor-quotient floor-remainder)

(define (floor/ n m)
  (let* ((q (floor (/ n m)))
         (r (- n (* m q))))
    (values q r)))

(define (floor-quotient n m)
  (let-values (((q r) (floor/ n m)))
    q))

(define (floor-remainder n m)
  (let-values (((q r) (floor/ n m)))
    r))

