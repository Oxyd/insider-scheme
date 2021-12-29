(library (insider basic-procedures))
(import (insider syntax) (except (insider internal) let define))
(export
 ;; From core
 eq? eqv? equal?
 cons car cdr set-car! set-cdr!
 make-vector vector-ref vector-set! vector-length
 type

 ;; Defined here
 define-type-predicate
 box? syntax? vector? plain-procedure? native-procedure? closure? procedure? scheme-procedure?
 symbol? string?
 not)

(define-syntax define-type-predicate
  (syntax-rules ()
    ((define-type-predicate name expected-type)
     (define (name x)
       (eq? (type x) 'expected-type)))))

(define-type-predicate box? insider::box)
(define-type-predicate syntax? insider::syntax)
(define-type-predicate vector? insider::vector)
(define-type-predicate plain-procedure? insider::procedure)
(define-type-predicate native-procedure? insider::native_procedure)
(define-type-predicate closure? insider::closure)
(define-type-predicate symbol? insider::symbol)
(define-type-predicate string? insider::string)

(define (procedure? x)
  (or (plain-procedure? x) (native-procedure? x) (closure? x)))

(define (scheme-procedure? x)
  (or (plain-procedure? x) (closure? x)))

(define (not x)
  (if x #f #t))
