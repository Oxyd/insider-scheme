(library (insider values))
(import (insider base-scheme) (insider syntax))
(export let-values let*-values)

(define-syntax let-values
  (syntax-rules ()
    ((let-values (((names ...) init-exprs) ...) . body)
     (let-values "collect" () () (((names ...) init-exprs) ...) . body))

    ((let-values "collect"
                 (temps ...) (names ...)
                 (((names-1 ...) init-expr-1) ((names-rest ...) init-exprs-rest) ...) . body)
     (let ((temp init-expr-1))
       (let-values "collect"
                   (temps ... temp) (names ... (names-1 ...))
                   (((names-rest ...) init-exprs-rest) ...) . body)))

    ((let-values "collect" (temps ...) (names ...) () . body)
     (let-values "call" (temps ...) (names ...) . body))

    ((let-values "call" (temp temps ...) ((names ...) names-rest ...) . body)
     (call-with-values
       (lambda () temp)
       (lambda (names ...)
         (let-values "call" (temps ...) (names-rest ...) . body))))

    ((let-values "call" () () . body)
     (begin . body))))

(define-syntax let*-values
  (syntax-rules ()
    ((let*-values (((names-1 ...) init-expr-1) ((names-rest ...) init-exprs-rest) ...) . body)
     (call-with-values
       (lambda () init-expr-1)
       (lambda (names-1 ...)
         (let*-values (((names-rest ...) init-exprs-rest) ...) . body))))

    ((let*-values () . body)
     (begin . body))))
