(library (insider syntax-test))
(import (insider syntax) (insider basic-procedures) (insider numeric) (insider list) (insider control)
        (insider define-values)
        (insider test))
(export test-syntax)

(define (test-syntax)
  (test-group "syntax"
    (test-eq 'greater
             (cond ((> 3 2) 'greater)
                   ((< 3 2) 'less)))

    (test-eq 'equal
             (cond ((> 3 3) 'greater)
                   ((< 3 3) 'less)
                   (else 'equal)))

    (test-eq 2
             (cond ((assv 'b '((a 1) (b 2))) => cadr)
                   (else #f)))

    (test-eq 'composite
             (case (* 2 3)
               ((2 3 5 7) 'prime)
               ((1 4 6 8 9) 'composite)))

    (test-eq 'c
             (case (car '(c d))
               ((a e i o u) 'vowel)
               ((w y) 'semivowel)
               (else => (lambda (x) x))))

    (test-equal '((other . z) (semivowel . y) (other . x)
                  (semivowel . w) (vowel . u))
                (map (lambda (x)
                       (case x
                         ((a e i o u) => (lambda (w) (cons 'vowel w)))
                         ((w y) (cons 'semivowel x))
                         (else => (lambda (w) (cons 'other w)))))
                     '(z y x w u)))

    (test-eq #t (and (= 2 2) (> 2 1)))
    (test-eq #f (and (= 2 2) (< 2 1)))
    (test-equal '(f g) (and 1 2 'c '(f g)))
    (test-eq #t (and))

    (test-eq #t (or (= 2 2) (> 2 1)))
    (test-eq #t (or (= 2 2) (< 2 1)))
    (test-eq #f (or #f #f #f))
    (test-equal '(b c) (or (memq 'b '(a b c))
                           (/ 3 0)))

    (test-eq 6 (let ((x 2) (y 3))
                 (* x y)))

    (test-eq 35 (let ((x 2) (y 3))
                  (let ((x 7)
                        (z (+ x y)))
                    (* z x))))

    (test-eq 70 (let ((x 2) (y 3))
                  (let* ((x 7)
                         (z (+ x y)))
                    (* z x))))

    (test-eq #t
             (letrec ((even?
                       (lambda (n)
                         (if (zero? n)
                             #t
                             (odd? (- n 1)))))
                      (odd?
                       (lambda (n)
                         (if (zero? n)
                             #f
                             (even? (- n 1))))))
               (even? 88)))

    (test-eq 5
             (letrec* ((p
                        (lambda (x)
                          (+ 1 (q (- x 1)))))
                       (q
                        (lambda (y)
                          (if (zero? y)
                              0
                              (+ 1 (p (- y 1))))))
                       (x (p 5))
                       (y x))
               y))

    ;; By Jussi Piitulainen <jpiitula@ling.helsinki.fi>
    ;; and John Cowan <cowan@mercury.ccil.org>:
    ;; http://lists.scheme-reports.org/pipermail/scheme-reports/2013-December/003876.html
    (let ()
      (define (means ton)
        (letrec*
            ((mean
              (lambda (f g)
                (f (/ (sum g ton) n))))
             (sum
              (lambda (g ton)
                (if (null? ton)
                    (+)
                    (if (number? ton)
                        (g ton)
                        (+ (sum g (car ton))
                           (sum g (cdr ton)))))))
             (n (sum (lambda (x) 1) ton)))
          (values (mean values values)
                  (mean exp log)
                  (mean / /))))
      (let*-values (((a b c) (means '(8 5 99 1 22))))
        (test-equal 27 a)
        (test-approximate 9.728 b 1e-6)
        (test-equal 1800/497 c)))

    (test-equal '(x y x y) (let ((a 'a) (b 'b) (x 'x) (y 'y))
                             (let*-values (((a b) (values x y))
                                           ((x y) (values a b)))
                               (list a b x y))))

    (test-eq 'ok (let-values () 'ok))

    (test-eq 1 (let ((x 1))
	         (let*-values ()
	           (define x 2)
	           #f)
	         x))

    (let ()
      (define x 0)
      (set! x 5)
      (test-eq 6 (+ x 1)))

    (test-equal #(0 1 2 3 4) (do ((vec (make-vector 5))
                                  (i 0 (+ i 1)))
                                 ((= i 5) vec)
                               (vector-set! vec i i)))

    (test-equal 25 (let ((x '(1 3 5 7 9)))
                     (do ((x x (cdr x))
                          (sum 0 (+ sum (car x))))
                         ((null? x) sum))))

    (test-equal '((6 1 3) (-5 -2))
                (let loop ((numbers '(3 -2 1 6 -5))
                           (nonneg '())
                           (neg '()))
                  (cond ((null? numbers) (list nonneg neg))
                        ((>= (car numbers) 0)
                         (loop (cdr numbers)
                               (cons (car numbers) nonneg)
                               neg))
                        ((< (car numbers) 0)
                         (loop (cdr numbers)
                               nonneg
                               (cons (car numbers) neg))))))

    (test-equal '(1 2)
                (let ()
                  (define-values (a b) (values 1 2))
                  (list a b)))

    (test-equal '(1)
                (let ()
                  (define-values (a) (values 1))
                  (list a)))

    (test-equal '()
                (let ()
                  (define-values () (values))
                  '()))

    (test-equal '(1 2 (3 4 5))
                (let ()
                  (define-values (a b . rest) (values 1 2 3 4 5))
                  (list a b rest)))

    (test-equal '(1 2 ())
                (let ()
                  (define-values (a b . rest) (values 1 2))
                  (list a b rest)))))

(when-main-module
 (test-syntax))
