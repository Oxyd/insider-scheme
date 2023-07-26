(define (test-vector)
  (test-group "vector"
    (test (vector? #(1 2 3)))
    (test-false (vector? #u8(1 2 3)))

    (test-equal #(5 5 5) (make-vector 3 5))
    (test-equal #(1 2 3) (vector 1 2 3))
    (test-equal #(foo bar baz) (vector 'foo 'bar 'baz))

    (test-equal 3 (vector-length #(1 2 3)))
    (test-equal 0 (vector-length #()))

    (test-equal 12 (vector-ref #(10 11 12 13 14) 2))

    (test-equal #(1 3 3 4)
                (let ((v (vector 1 2 3 4)))
                  (vector-set! v 1 3)
                  v))

    (test-equal #(3 4) (vector-copy #(1 2 3 4 5) 2 4))

    (test-equal #(10 1 2 40 50)
                (let ((a (vector 1 2 3 4 5))
                      (b (vector 10 20 30 40 50)))
                  (vector-copy! b 1 a 0 2)
                  b))

    (test-equal #(1 1 2 4 5)
                (let ((a (vector 1 2 3 4 5)))
                  (vector-copy! a 1 a 0 2)
                  a))

    (test-equal #(0 1 2 3 4 5)
                (vector-append #(0 1 2) #(3 4 5)))

    (test-equal '(dah dah didah) (vector->list #(dah dah didah)))
    (test-equal '(dah) (vector->list #(dah dah didah) 1 2))
    (test-equal #(dididit dah) (list->vector '(dididit dah)))

    (test-equal #(3 8 2 8)
                (let ((v (vector-copy #(1 8 2 8))))
                  (vector-set! v 0 3)
                  v))

    (test-equal #(8 2) (vector-copy #(1 8 2 8) 1 3))

    (test-equal #(a b c d e f) (vector-append #(a b c) #(d e f)))

    (test-equal #(1 2 smash smash 5)
                (let ((a (vector 1 2 3 4 5)))
                  (vector-fill! a 'smash 2 4)
                  a))

    (test-equal '(0 1 4 9 16)
                (let ((v (make-list 5)))
                  (vector-for-each
                   (lambda (i) (list-set! v i (* i i)))
                   #(0 1 2 3 4))
                  v))

    (test-equal 17
                (let ((sum-of-products 0))
                  (vector-for-each
                   (lambda (i j) (set! sum-of-products (+ sum-of-products (* i j))))
                   #(1 2 3 4)
                   #(5 6))
                  sum-of-products))

    (test-equal #(b e h)
                (vector-map cadr #((a b) (d e) (g h))))
    (test-equal #(1 4 27 256 3125)
                (vector-map (lambda (n) (expt n n))
                            #(1 2 3 4 5)))
    (test-equal #(5 7 9)
                (vector-map + #(1 2 3) #(4 5 6 7)))

    (test-equal '(#(3 6 9) #(2 4 6))
                (let ((cont #f)
                      (input #(1 2 3))
                      (multiplier 2)
                      (result '())
                      (again? #t))
                  (let ((elem (vector-map (lambda (n)
                                            (unless cont
                                              (call/cc (lambda (k)
                                                         (set! cont k))))
                                            (* n multiplier))
                                          input)))
                    (set! result (cons elem result))
                    (when again?
                      (set! again? #f)
                      (set! multiplier 3)
                      (cont #f))
                    result)))

    (test-equal '(#(15 21 27) #(10 14 18))
                (let ((cont #f)
                      (input-1 #(1 2 3))
                      (input-2 #(4 5 6))
                      (multiplier 2)
                      (result '())
                      (again? #t))
                  (let ((elem (vector-map (lambda (x y)
                                            (unless cont
                                              (call/cc (lambda (k)
                                                         (set! cont k))))
                                            (* (+ x y) multiplier))
                                          input-1
                                          input-2)))
                    (set! result (cons elem result))
                    (when again?
                      (set! again? #f)
                      (set! multiplier 3)
                      (cont #f))
                    result)))))

(when-main-module
 (test-vector))
