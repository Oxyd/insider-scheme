(define-library (scheme char)
  (import (insider char) (insider string))
  (export
   char-alphabetic?
   char-ci<?
   char-ci>=?
   char-downcase
   char-lower-case?
   char-upcase
   char-whitespace?
   string-ci<=?
   string-ci=?
   string-ci>?
   string-foldcase
   char-ci<=?
   char-ci=?
   char-ci>?
   char-foldcase
   char-numeric?
   char-upper-case?
   digit-value
   string-ci<?
   string-ci>=?
   string-downcase
   string-upcase))
