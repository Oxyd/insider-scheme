(define (basics)
  (test-group "basics"
    (test (pair? '(1 . 2)))
    (test (pair? '(1 2)))
    (test-false (pair? '()))
    (test-false (pair? 2))
    (test (null? '()))
    (test-false (null? '(1 . 2)))
    (test-false (null? #f))
    (test-false (null? 0))

    (test (pair? (cons 'a 'b)))
    (test-equal '(a) (cons 'a '()))
    (test-equal '((a) b c d) (cons '(a) '(b c d)))
    (test-equal '("a" b c) (cons "a" '(b c)))
    (test-equal '(a . 3) (cons 'a 3))
    (test-equal '((a b) . c) (cons '(a b) 'c))

    (test-equal 'a (car '(a . b)))
    (test-equal 'b (cdr '(a . b)))

    (test-equal '(x . b) (let ((p (cons 'a 'b)))
                           (set-car! p 'x)
                           p))
    (test-equal '(a . x) (let ((p (cons 'a 'b)))
                           (set-cdr! p 'x)
                           p))

    (test-equal 'a (caar '((a . b) c)))
    (test-equal 'c (cadr '((a . b) c)))
    (test-equal 'b (cdar '((a . b) c)))
    (test-equal '() (cddr '((a . b) c)))

    (test (list? '(1 2 3)))
    (test-false (list? '(1 2 . 3)))
    (test (list? '()))
    (test-false (list? 2))
    (test-false (let ((x (list 'a)))
                  (set-cdr! x x)
                  (list? x)))

    (test-equal '(3 3) (make-list 2 3))

    (test-equal 3 (length '(a b c)))
    (test-equal 3 (length '(a (b) (c d e))))
    (test-equal 0 (length '()))

    (test-equal '(x y) (append '(x) '(y)))
    (test-equal '(a b c d) (append '(a) '(b c d)))
    (test-equal '(a (b) (c)) (append '(a (b)) '((c))))
    (test-equal '(a b c . d) (append '(a b) '(c . d)))
    (test-equal 'a (append '() 'a))

    (test-equal '(c b a) (reverse '(a b c)))
    (test-equal '((e (f)) d (b c) a) (reverse '(a (b c) d (e (f)))))

    (test-equal '(c d) (list-tail '(a b c d) 2))

    (test-equal 'c (list-ref '(a b c d) 2))

    (test-equal '(one two three)
                (let ((ls (list 'one 'two 'five!)))
                  (list-set! ls 2 'three)
                  ls))

    (test-equal '(1 2 3) (list-copy '(1 2 3)))))

(define (searching)
  (test-group "searching"
    (test-equal '(a b c) (memq 'a '(a b c)))
    (test-equal '(b c) (memq 'b '(a b c)))
    (test-false (memq 'a '(b c d)))
    (test-false (memq (list 'a) '(b (a) c)))
    (test-equal '((a) c) (member (list 'a) '(b (a) c)))
    (test-equal '("b" "c") (member "B" '("a" "b" "c") string-ci=?))
    (test-equal '(101 102) (memv 101 '(100 101 102)))

    (let ((e '((a 1) (b 2) (c 3))))
      (test-equal '(a 1) (assq 'a e))
      (test-equal '(b 2) (assq 'b e))
      (test-false (assq 'd e))
      (test-false (assq (list 'a) '(((a)) ((b)) ((c)))))
      (test-equal '((a)) (assoc (list 'a) '(((a)) ((b)) ((c)))))
      (test-equal '(2 4) (assoc 2.0 '((1 1) (2 4) (3 9)) =))
      (test-equal '(5 7) (assv 5 '((2 3) (5 7) (11 13)))))))

(define (iterating)
  (test-group "iterating"
    (test-group "for-each"
      (test-equal 6 (let ((result 0))
                      (for-each (lambda (n) (set! result (+ result n))) '(1 2 3))
                      result))

      (test-equal 32 (let ((result 0))
                       (for-each
                        (lambda (x y)
                          (set! result (+ result (* x y))))
                        '(1 2 3)
                        '(4 5 6))
                       result)))

    (test-group "filter"
      (test-equal '(2 4 6)
                  (filter even? '(1 2 3 4 5 6 7))))

    (test-group "fold"
      (test-equal 15 (fold + 0 '(1 2 3 4 5)))
      (test-equal '(5 4 3 2 1) (fold cons '() '(1 2 3 4 5)))
      (test-equal '(c 3 b 2 a 1)
                  (fold (lambda (x y accum) (cons x (cons y accum)))
                        '()
                        '(a b c)
                        '(1 2 3 4 5))))))

(define (test-list)
  (test-group "list"
    (basics)
    (searching)
    (iterating)))

(when-main-module
 (test-list))
