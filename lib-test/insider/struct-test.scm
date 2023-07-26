(define (test-struct)
  (test-group "struct"
    (let ()
      (struct foo (a b c))

      (let ((x (foo 1 2 3)))
        (test (foo? x))
        (test-false (foo? '()))
        (test-equal 1 (foo-a x))
        (test-equal 2 (foo-b x))
        (test-equal 3 (foo-c x))))

    (let ()
      (struct foo (a b (c #f)))

      (let ((x (foo 1 2)))
        (test-equal 1 (foo-a x))
        (test-equal 2 (foo-b x))
        (test-equal #f (foo-c x)))

      (let ((x (foo 1 2 3)))
        (test-equal 3 (foo-c x))))

    (let ()
      (struct foo (a #:b b #:c (c #f)))

      (let ((x (foo #:b 2 1)))
        (test-equal 1 (foo-a x))
        (test-equal 2 (foo-b x))
        (test-equal #f (foo-c x))))

    (let ()
      (struct foo (x)
              #:constructor-name foo
              #:predicate-name bar?
              #:type-name >foo<)

      (let ((x (foo 1)))
        (test-equal 1 (foo-x x))
        (test (bar? x))
        (test-false (bar? #f))))

    (let ()
      (struct foo (a (b #:mutable)))

      (let ((x (foo 1 2)))
        (test-equal 1 (foo-a x))
        (test-equal 2 (foo-b x))
        (foo-b-set! x 10)
        (test-equal 10 (foo-b x))))

    (let ()
      (struct foo ((x #:getter-name foo.x)
                   (y 0
                      #:mutable
                      #:getter-name foo.y
                      #:setter-name set-foo.y!)))

      (let ((x (foo 1)))
        (test-equal 1 (foo.x x))
        (test-equal 0 (foo.y x))
        (set-foo.y! x 12)
        (test-equal 12 (foo.y x))))))

(when-main-module
 (test-struct))
