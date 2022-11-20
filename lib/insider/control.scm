(library (insider control))
(import (insider syntax)
        (insider basic-procedures)
        (insider list)
        (insider error)
        (only (insider internal)
              eq? capture-stack replace-stack! create-parameter-tag
              find-parameter-value set-parameter-value! call-parameterized apply
              values call-with-values with-exception-handler raise
              raise-continuable dynamic-wind main-module?-tag environment
              interactive-environment eval meta current-expand-module-tag
              dynamic-import interaction-environment-specifier-tag))
(export call-with-current-continuation call/cc let/cc make-parameter
        make-parameter-from-tag parameterize apply values call-with-values
        with-exception-handler raise raise-continuable dynamic-wind guard
        when-main-module environment interactive-environment eval meta
        current-expand-module dynamic-import interaction-environment-specifier)

(define (call-with-current-continuation f)
  (capture-stack
   (lambda (stack)
     (f (lambda vals
          (replace-stack! stack (apply values vals)))))))

(define call/cc call-with-current-continuation)

(define-syntax let/cc
  (syntax-rules ()
    ((let/cc var . body)
     (call/cc
      (lambda (var) . body)))))

(define <get-tag> (list 'get-tag))
(define <get-converter> (list 'get-converter))

(define (make-parameter-from-tag tag (converter (lambda (x) x)))
  (lambda args
    (cond ((null? args)
           (find-parameter-value tag))
          ((eq? (car args) <get-tag>)
           tag)
          ((eq? (car args) <get-converter>)
           converter)
          (else
           (let ((value (car args)))
             (set-parameter-value! tag (converter value)))))))

(define (make-parameter init (converter (lambda (x) x)))
  (let ((tag (create-parameter-tag (converter init))))
    (make-parameter-from-tag tag converter)))

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
       (let ((tag (p <get-tag>))
             (transformed-value ((p <get-converter>) value0)))
         (parameterize-collect ((tags transformed-values) ... (tag transformed-value))
                               ((param1 value1) ...)
                               body ...))))))

(define-syntax parameterize
  (syntax-rules ()
    ((parameterize ((params values) ...) body ...)
     (parameterize-collect () ((params values) ...) body ...))))

(define-syntax make-guard-cond
  (syntax-rules (else =>)
    ((make-guard-cond reraise (else result1 result2 ...))
     (begin result1 result2 ...))

    ((make-guard-cond reraise (test => result))
     (let ((temp test))
       (if temp
           (result temp)
           reraise)))

    ((make-guard-cond reraise (test => result) clause1 clause2 ...)
     (let ((temp test))
       (if temp
           (result temp)
           (make-guard-cond reraise clause1 clause2 ...))))

    ((make-guard-cond reraise (test))
     (or test reraise))

    ((make-guard-cond reraise (test) clause1 clause2 ...)
     (let ((temp test))
       (if temp
           temp
           (make-guard-cond reraise clause1 clause2 ...))))

    ((make-guard-cond reraise (test result1 result2 ...))
     (if test
         (begin result1 result2 ...)
         reraise))

    ((make-guard-cond reraise (test result1 result2 ...) clause1 clause2 ...)
     (if test
         (begin result1 result2 ...)
         (make-guard-cond reraise clause1 clause2 ...)))))

(define-syntax make-guard-handler
  (syntax-rules ()
    ((make-guard-handler condition return-from-guard (var clause ...))
     (let/cc return-from-handler
       (return-from-guard
        (lambda ()
          (let ((var condition))
            (make-guard-cond
             (return-from-handler
              (lambda ()
                (raise-continuable condition)))
             clause ...))))))))

(define-syntax make-guard-result-thunk
  (syntax-rules ()
    ((make-guard-result-thunk (var clause ...) body1 body2 ...)
     (let/cc return-from-guard
       (with-exception-handler
        (lambda (condition)
          (let ((handler-result-thunk (make-guard-handler condition return-from-guard (var clause ...))))
            (handler-result-thunk)))
        (lambda ()
          (call-with-values
            (lambda () body1 body2 ...)
            (lambda results
              (return-from-guard
               (lambda ()
                 (apply values results)))))))))))

(define-syntax guard
  (syntax-rules ()
    ((guard (var clause ...) body1 body2 ...)
     (let ((result-thunk (make-guard-result-thunk (var clause ...) body1 body2 ...)))
       (result-thunk)))))

(define main-module? (make-parameter-from-tag main-module?-tag))

(define-syntax when-main-module
  (lambda (stx)
    (syntax-match stx ()
      ((_ . body)
       (if (main-module?)
           #`(begin . #,body)
           #'(begin))))))

(define current-expand-module
  (make-parameter-from-tag current-expand-module-tag))

(define interaction-environment-specifier
  (make-parameter-from-tag interaction-environment-specifier-tag))
