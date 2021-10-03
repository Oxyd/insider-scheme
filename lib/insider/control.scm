(library (insider control))
(import (insider base-scheme)
        (insider syntax)
        (insider error)
        (only (insider internal) capture-stack replace-stack!
              create-parameter-tag  find-parameter-value set-parameter-value! call-parameterized
              apply values call-with-values))
(export call-with-current-continuation call/cc make-parameter
        make-parameter-from-tag parameterize apply values call-with-values)

(define (call-with-current-continuation f)
  (capture-stack
   (lambda (stack)
     (f (lambda vals
          (replace-stack! stack (apply values vals)))))))

(define call/cc call-with-current-continuation)

(define (make-parameter-from-tag tag . args)
  (let ((converter (if (null? args)
                       (lambda (x) x)
                       (car args))))
    (lambda args
      (cond ((null? args)
             (find-parameter-value tag))
            ((eq? (car args) 'get-tag)
             tag)
            ((eq? (car args) 'get-converter)
             converter)
            ((eq? (car args) 'set-value)
             (let ((value (cadr args)))
               (set-parameter-value! tag (converter value))))
            (else
             (error "Invalid parameter procedure call"))))))

(define (make-parameter init . args)
  (let ((converter (if (null? args)
                       (lambda (x) x)
                       (car args))))
    (let ((tag (create-parameter-tag (converter init))))
      (make-parameter-from-tag tag converter))))

(define-syntax do-call-parameterized
  (syntax-rules ()
    ((do-call-parameterized () body ...)
     (begin body ...))

    ((d-call-parameterized ((tag0 value0) (tag1 value1) ...) body ...)
     (call-parameterized tag0 value0
                         (lambda ()
                           (do-call-parameterized ((tag1 value1) ...) body ...))))))

(define-syntax parameterize-collect
  (syntax-rules ()
    ((parameterize-collect ((tags transformed-values) ...) () body ...)
     (do-call-parameterized ((tags transformed-values) ...) body ...))

    ((parameterize-collect ((tags transformed-values) ...) ((param0 value0) (param1 value1) ...) body ...)
     (let ((p param0))
       (let ((tag (p 'get-tag))
             (transformed-value ((p 'get-converter) value0)))
         (parameterize-collect ((tags transformed-values) ... (tag transformed-value))
                               ((param1 value1) ...)
                               body ...))))))

(define-syntax parameterize
  (syntax-rules ()
    ((parameterize ((params values) ...) body ...)
     (parameterize-collect () ((params values) ...) body ...))))
