(define-library (insider basic-procedures)
  (import (insider syntax)
          (only (insider internal)
                eq? eqv? equal? type apply))
  (export
   ;; From core
   eq? eqv? equal?
   type

   ;; Defined here
   box? syntax? native-procedure? procedure-prototype? procedure?
   scheme-procedure?
   symbol? boolean? keyword? pair? vector? bytevector? char? string?
   string-cursor? textual-input-port? binary-input-port? textual-output-port?
   binary-output-port? values-tuple?
   not
   boolean=? symbol=?)
  (include "basic-procedures.scm"))
