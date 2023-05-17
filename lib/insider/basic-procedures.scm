(define-syntax define-type-predicate
  (syntax-rules ()
    ((define-type-predicate name expected-type)
     (define (name x)
       (eq? (type x) 'expected-type)))))

(define-type-predicate box? insider::box)
(define-type-predicate syntax? insider::syntax)
(define-type-predicate scheme-procedure? insider::procedure)
(define-type-predicate native-procedure? insider::native_procedure)
(define-type-predicate procedure-prototype? insider::procedure_prototype)
(define-type-predicate symbol? insider::symbol)
(define-type-predicate string? insider::string)
(define-type-predicate boolean? insider::boolean)
(define-type-predicate keyword? insider::keyword)

(define (procedure? x)
  (or (scheme-procedure? x) (native-procedure? x)))

(define (not x)
  (if x #f #t))

(define (all-eq? first second . rest)
  (and (eq? first second)
       (or (eq? rest '())
           (apply all-eq? second rest))))

(define boolean=? all-eq?)
(define symbol=? all-eq?)
