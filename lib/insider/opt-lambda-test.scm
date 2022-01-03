(library (insider opt-lambda-test))
(import (insider syntax) (insider opt-lambda) (insider test) (insider numeric) (insider control) (insider list))
(export test-opt-lambda)

(define (test-opt-lambda)
  (test-group "opt-lambda"
    (let ((f (opt-lambda (x y) (+ x y))))
      (test-equal 5 (f 2 3))
      (test-error (f 2))
      (test-error (f))
      (test-error (f 1 2 3)))

    (let ((f (opt-lambda (x (y 1)) (+ x y))))
      (test-equal 5 (f 2 3))
      (test-equal 3 (f 2))
      (test-error (f))
      (test-error (f 1 2 3)))

    (let ((f (opt-lambda (x (y x)) (+ x y))))
      (test-equal 5 (f 2 3))
      (test-equal 4 (f 2)))

    (let ((f (opt-lambda (x (y 'one) (z 'two))
               (list x y z))))
      (test-equal '(zero one two) (f 'zero))
      (test-equal '(zero eleven two) (f 'zero 'eleven))
      (test-equal '(zero eleven twenty) (f 'zero 'eleven 'twenty)))))

(when-main-module
 (test-opt-lambda))
