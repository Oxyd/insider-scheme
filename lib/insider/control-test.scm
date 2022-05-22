(library (insider control-test))
(import (insider syntax) (insider control) (insider test) (scheme repl))
(export test-control)

(define (test-eval)
  (test-group "eval"
    (test-equal 21 (eval '(* 7 3) (environment '(scheme base))))
    (test-error (eval '(define foo 32) (environment '(scheme base))))))

(define (test-repl)
  (test-group "repl"
    (parameterize ((interaction-environment-specifier '(insider interactive)))
      (test-equal 6
                  (let ((env (interaction-environment)))
                    (eval '(define foo 3) env)
                    (eval '(define (double x) (* x 2)) env)
                    (eval '(double foo) env))))))

(define (test-control)
  (test-group "control"
    (test-eval)
    (test-repl)))

(when-main-module
 (test-control))
