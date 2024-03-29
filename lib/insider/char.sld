(define-library (insider char)
  (import (insider syntax) (insider list) (insider basic-procedures)
          (insider control) (insider numeric)
          (only (insider internal)
                char->integer char-foldcase char-alphabetic? char-numeric?
                char-whitespace? char-upper-case? char-lower-case? digit-value
                integer->char char-upcase char-downcase
                char=? char<? char<=? char>? char>=?
                char-ci=? char-ci<? char-ci<=? char-ci>? char-ci>=?))
  (export
   char-alphabetic? char-numeric? char-whitespace? char-upper-case?
   char-lower-case? digit-value char->integer integer->char char-upcase
   char-downcase char-foldcase

   char? char=? char<? char<=? char>? char>=?
   char-ci=? char-ci<? char-ci<=? char-ci>? char-ci>=?)
  (begin
    (define-type-predicate char? insider::character)))
