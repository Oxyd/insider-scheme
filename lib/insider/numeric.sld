(define-library (insider numeric)
  (import (insider syntax) (insider error) (insider list)
          (insider basic-procedures) (scheme case-lambda)
          (rename (only (insider internal)
                        + - * / = < <= > >= truncate/ truncate-quotient
                        truncate-remainder gcd arithmetic-shift bitwise-and
                        bitwise-ior bitwise-xor bitwise-not bit-count
                        integer-length first-set-bit integer? odd? even? zero?
                        positive? negative? number? infinite? finite? nan? exp
                        log abs floor ceiling truncate round inexact? exact?
                        exact-integer? real? rational? inexact exact expt square
                        sqrt angle magnitude make-rectangular make-polar
                        real-part imag-part sin cos tan asin acos atan atan2

                        fraction-numerator fraction-denominator

                        values make-vector vector-ref vector-set! vector-length)
                  (bitwise-and %bitwise-and)
                  (bitwise-ior %bitwise-ior)
                  (bitwise-xor %bitwise-xor)
                  (gcd %gcd)
                  (log %log)
                  (atan %atan)))
  (export
   ;; From core
   + - * / = < <= > >= truncate/ truncate-quotient truncate-remainder gcd
   arithmetic-shift bitwise-and bitwise-ior bitwise-xor bitwise-not bitwise-eqv
   bitwise-nand bitwise-nor bitwise-andc1 bitwise-andc2 bitwise-orc1
   bitwise-orc2 bit-count integer-length first-set-bit integer? odd? even? zero?
   positive? negative? number? infinite? finite? nan? exp abs floor ceiling
   truncate round inexact? exact? exact-integer? real? rational? inexact exact
   expt square sqrt angle magnitude make-rectangular make-polar real-part
   imag-part sin cos tan asin acos atan

   ;; Defined here
   complex? floor/ floor-quotient floor-remainder modulo quotient remainder min
   max numerator denominator bitwise-if bit-set? copy-bit bit-swap any-bit-set?
   every-bit-set? bit-field bit-field-any? bit-field-every? bit-field-clear
   bit-field-set bit-field-replace bit-field-replace-same bit-field-rotate
   bit-field-reverse bits->list bits->vector list->bits vector->bits bits
   bitwise-fold bitwise-for-each bitwise-unfold make-bitwise-generator
   exact-integer-sqrt lcm rationalize log)
  (include "numeric.scm"))
