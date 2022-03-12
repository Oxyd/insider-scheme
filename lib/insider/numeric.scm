(library (insider numeric))
(import (insider syntax)
        (except (insider internal) define let))
(export
 ;; From core
 + - * / = < <= > >= truncate/ truncate-quotient truncate-remainder
 gcd arithmetic-shift bitwise-and bitwise-or bitwise-not integer? odd? even? zero? number? exp log
 abs floor inexact? exact? exact-integer? inexact exact expt

 ;; Defined here
 floor/ floor-quotient floor-remainder min max)

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

(define (min/inexact x xs)
  (if (eq? xs '())
      (inexact x)
      (min/inexact (if (< (car xs) x) (car xs) x)
                   (cdr xs))))

(define (min* x xs)
  (cond ((eq? xs '())
         x)
        ((inexact? (car xs))
         (min/inexact x xs))
        (else
         (min* (if (< (car xs) x) (car xs) x) (cdr xs)))))

(define (min x . xs)
  (if (exact? x)
      (min* x xs)
      (min/inexact x xs)))

(define (max/inexact x xs)
  (if (eq? xs '())
      (inexact x)
      (max/inexact (if (> (car xs) x) (car xs) x)
                   (cdr xs))))

(define (max* x xs)
  (cond ((eq? xs '())
         x)
        ((inexact? (car xs))
         (max/inexact x xs))
        (else
         (max* (if (> (car xs) x) (car xs) x) (cdr xs)))))

(define (max x . xs)
  (if (exact? x)
      (max* x xs)
      (max/inexact x xs)))
