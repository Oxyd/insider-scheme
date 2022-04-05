(library (insider basic-procedures))
(import (insider syntax)
        (only (insider internal)
              eq? eqv? equal? type apply))
(export
 ;; From core
 eq? eqv? equal?
 type

 ;; Defined here
 define-type-predicate
 box? syntax? plain-procedure? native-procedure? closure? procedure? scheme-procedure?
 symbol? boolean?
 not
 boolean=? symbol=?)

(define-syntax define-type-predicate
  (syntax-rules ()
    ((define-type-predicate name expected-type)
     (define (name x)
       (eq? (type x) 'expected-type)))))

(define-type-predicate box? insider::box)
(define-type-predicate syntax? insider::syntax)
(define-type-predicate plain-procedure? insider::procedure)
(define-type-predicate native-procedure? insider::native_procedure)
(define-type-predicate closure? insider::closure)
(define-type-predicate symbol? insider::symbol)
(define-type-predicate string? insider::string)
(define-type-predicate boolean? insider::boolean)

(define (procedure? x)
  (or (plain-procedure? x) (native-procedure? x) (closure? x)))

(define (scheme-procedure? x)
  (or (plain-procedure? x) (closure? x)))

(define (not x)
  (if x #f #t))

(define (all-eq? first second . rest)
  (and (eq? first second)
       (or (eq? rest '())
           (apply all-eq? second rest))))

(define boolean=? all-eq?)
(define symbol=? all-eq?)
