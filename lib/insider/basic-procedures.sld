(define-library (insider basic-procedures)
  (import (insider syntax)
          (only (insider internal)
                eq? eqv? equal? type apply))
  (export
   ;; From core
   eq? eqv? equal?
   type

   ;; Defined here
   define-type-predicate
   box? syntax? native-procedure? procedure-prototype? procedure?
   scheme-procedure?
   symbol? boolean? keyword?
   not
   boolean=? symbol=?)
  (include "basic-procedures.scm"))
