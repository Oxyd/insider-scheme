(define-library (insider syntax)
  (import (rename (insider internal)
                  (define %define)
                  (let %let))
          (insider syntax-rules))
  (export
   ;; From core
   set! lambda if box unbox box-set! define-syntax begin quote quasiquote unquote
   unquote-splicing syntax quasisyntax unsyntax unsyntax-splicing syntax-trap
   syntax-error letrec* let-syntax letrec-syntax

   syntax-expression syntax-scopes syntax-add-scope syntax->datum syntax->list
   datum->syntax free-identifier=? bound-identifier=? syntax-location

   ;; From (insider syntax-rules)
   syntax-match syntax-rules

   ;; Defined here
   ... _ define let let* letrec let-values let*-values case when unless do
   or and define-auxiliary-syntax cond else =>)
  (include "syntax.scm"))
