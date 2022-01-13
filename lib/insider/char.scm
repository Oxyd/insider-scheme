(library (insider char))
(import (insider syntax) (insider list) (insider basic-procedures)
        (except (insider internal) define let))
(export
 char-alphabetic? char-numeric? char-whitespace? char-upper-case? char-lower-case? digit-value
 char->integer integer->char char-upcase char-downcase char-foldcase

 char? char=? char<? char<=? char>? char>=?
 char-ci=? char-ci<? char-ci<=? char-ci>? char-ci>=?)

(define-type-predicate char? insider::character)

(define (compare-chars op chars)
  (apply op (map char->integer chars)))

(define (char=? . chars)
  (compare-chars = chars))

(define (char<? . chars)
  (compare-chars < chars))

(define (char<=? . chars)
  (compare-chars <= chars))

(define (char>? . chars)
  (compare-chars > chars))

(define (char>=? . chars)
  (compare-chars >= chars))

(define (compare-chars-ci op chars)
  (apply op (map (lambda (c) (char->integer (char-foldcase c))) chars)))

(define (char-ci=? . chars)
  (compare-chars-ci = chars))

(define (char-ci<? . chars)
  (compare-chars-ci < chars))

(define (char-ci<=? . chars)
  (compare-chars-ci <= chars))

(define (char-ci>? . chars)
  (compare-chars-ci > chars))

(define (char-ci>=? . chars)
  (compare-chars-ci >= chars))
