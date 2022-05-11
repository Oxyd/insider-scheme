(library (insider control-test))
(import (insider syntax) (insider control) (insider test))
(export test-control)

(define (test-eval)
  (test-group "eval"
    (test-equal 21 (eval '(* 7 3) (environment '(scheme base))))
    (test-error (eval '(define foo 32) (environment '(scheme base))))))

(define (test-control)
  (test-group "control"
    (test-eval)))

(when-main-module
 (test-control))
