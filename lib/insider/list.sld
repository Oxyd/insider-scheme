(define-library (insider list)
  (import (insider syntax) (insider basic-procedures)
          (only (insider internal)
                car cdr set-car! set-cdr! cons cadr caddr cadddr cddr cdddr
                append list + - = apply values))
  (export
   ;; From core
   cons car cdr cadr caddr cadddr cddr cdddr set-car! set-cdr! append list

   ;; Defined here: R7RS procedures:
   null? pair? list?
   caar cdar caaar caadr cadar cdaar cdadr cddar
   caaaar caaadr caadar caaddr cadaar cadadr caddar
   cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
   assoc assq assv member memq memv length reverse map for-each
   make-list list-tail list-ref list-set! list-copy

   ;; SRFI-1:
   any every filter fold)
  (include "list.scm"))
