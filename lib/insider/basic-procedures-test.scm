(library (insider basic-procedures-test))
(import (insider syntax) (insider basic-procedures) (insider test) (insider control) (insider list)
        (insider numeric))
(export test-basic-procedures)

(define (test-eq?)
  (test-group "eq?"
    (test (eq? 'a 'a))
    (test-false (eq? (list 'a) (list 'a)))
    (test (eq? '() '()))
    (test (let ((x '(a)))
            (eq? x x)))
    (test (let ((x '#()))
            (eq? x x)))
    (test (let ((p (lambda (x) x)))
            (eq? p p)))))

(define (test-eqv?)
  (define (gen-counter)
    (let ((n 0))
      (lambda ()
        (set! n (+ n 1))
        n)))

  (define (gen-loser)
    (let ((n 0))
      (lambda ()
        (set! n (+ n 1))
        27)))

  (test-group "eqv?"
    (test (eqv? 'a 'a))
    (test (let ((x '(a)))
            (eqv? x x)))
    (test-false (eqv? 'a 'b))
    (test (eqv? 2 2))
    (test (eqv? 100000000 100000000))
    (test-false (eqv? (cons 1 2) (cons 1 2)))
    (test-false (eqv? (lambda () 1)
                      (lambda () 2)))
    (test-false (eqv? #f 'nil))
    (test (let ((g (gen-counter)))
            (eqv? g g)))
    (test-false (eqv? (gen-counter) (gen-counter)))
    (test (let ((g (gen-loser)))
            (eqv? g g)))
    (test-false (letrec ((f (lambda () (if (eqv? f g) 'f 'both)))
                         (g (lambda () (if (eqv? f g) 'g 'both))))
                  (eqv? f g)))))

(define (test-equal?)
  (test-group "equal?"
    (test (equal? 'a 'a))
    (test (equal? '(a) '(a)))
    (test (equal? '(a (b) (c))
                  '(a (b) (c))))
    (test (equal? "abc" "abc"))
    (test (equal? 2 2))
    (test (equal? (make-vector 5 'a)
                  (make-vector 5 'a)))))

(define (test-equivalence-predicates)
  (test-group "Equivalence predicates"
    (test-eq?)
    (test-eqv?)
    (test-equal?)))

(define (test-basic-procedures)
  (test-group "(insider basic-procedures)"
    (test-equivalence-predicates)))

(when-main-module
 (test-basic-procedures))

;; Local variables:
;; eval: (put 'test-group 'scheme-indent-function 1)
;; End:
