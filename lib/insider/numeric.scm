(library (insider numeric))
(import (insider syntax) (insider error) (insider list) (insider basic-procedures)
        (rename (only (insider internal)
                      + - * / = < <= > >= truncate/ truncate-quotient truncate-remainder
                      gcd arithmetic-shift bitwise-and bitwise-ior bitwise-xor bitwise-not
                      bit-count integer-length first-set-bit
                      integer? odd? even? zero? positive? negative?
                      number? exp log
                      abs floor ceiling truncate round
                      inexact? exact? exact-integer? real? rational? inexact exact expt

                      fraction-numerator fraction-denominator

                      values make-vector vector-ref vector-set! vector-length)
                (bitwise-and %bitwise-and)
                (bitwise-ior %bitwise-ior)
                (bitwise-xor %bitwise-xor)))
(export
 ;; From core
 + - * / = < <= > >= truncate/ truncate-quotient truncate-remainder
 gcd arithmetic-shift bitwise-and bitwise-ior bitwise-xor bitwise-not bitwise-eqv
 bitwise-nand bitwise-nor bitwise-andc1 bitwise-andc2 bitwise-orc1 bitwise-orc2
 bit-count integer-length first-set-bit
 integer? odd? even? zero? positive? negative?
 number? exp log
 abs floor ceiling truncate round
 inexact? exact? exact-integer? real? rational? inexact exact expt

 ;; Defined here
 complex? floor/ floor-quotient floor-remainder min max
 numerator denominator
 bitwise-if bit-set? copy-bit bit-swap any-bit-set? every-bit-set?
 bit-field bit-field-any? bit-field-every? bit-field-clear bit-field-set
 bit-field-replace bit-field-replace-same bit-field-rotate bit-field-reverse
 bits->list bits->vector list->bits vector->bits bits
 bitwise-fold bitwise-for-each bitwise-unfold
 make-bitwise-generator)

(define complex? number?)

(define (floor/ n m)
  (let* ((q (floor (/ n m)))
         (r (- n (* m q))))
    (values q r)))

(define (floor-quotient n m)
  (let-values (((q r) (floor/ n m)))
    q))

(define (floor-remainder n m)
  (let-values (((q r) (floor/ n m)))
    r))

(define (min/inexact x xs)
  (if (eq? xs '())
      (inexact x)
      (min/inexact (if (< (car xs) x) (car xs) x)
                   (cdr xs))))

(define (min* x xs)
  (cond ((eq? xs '())
         x)
        ((inexact? (car xs))
         (min/inexact x xs))
        (else
         (min* (if (< (car xs) x) (car xs) x) (cdr xs)))))

(define (min x . xs)
  (if (exact? x)
      (min* x xs)
      (min/inexact x xs)))

(define (max/inexact x xs)
  (if (eq? xs '())
      (inexact x)
      (max/inexact (if (> (car xs) x) (car xs) x)
                   (cdr xs))))

(define (max* x xs)
  (cond ((eq? xs '())
         x)
        ((inexact? (car xs))
         (max/inexact x xs))
        (else
         (max* (if (> (car xs) x) (car xs) x) (cdr xs)))))

(define (max x . xs)
  (if (exact? x)
      (max* x xs)
      (max/inexact x xs)))

(define (numerator q)
  (case (type q)
    ((insider::integer insider::big_integer)
     q)
    ((insider::fraction)
     (fraction-numerator q))
    ((insider::floating_point)
     (inexact (numerator (exact q))))
    (else
     (error "Expected a rational number" q))))

(define (denominator q)
  (case (type q)
    ((insider::integer insider::big_integer)
     1)
    ((insider::fraction)
     (fraction-denominator q))
    ((insider::floating_point)
     (inexact (denominator (exact q))))
    (else
     (error "Expected a rational number" q))))

(define (make-bitwise-folder f identity)
  (lambda args
    (if (null? args)
        identity
        (let loop ((x args) (accum identity))
          (if (null? x)
              accum
              (loop (cdr x) (f (car x) accum)))))))

(define bitwise-and (make-bitwise-folder %bitwise-and -1))
(define bitwise-ior (make-bitwise-folder %bitwise-ior 0))
(define bitwise-xor (make-bitwise-folder %bitwise-xor 0))

(define (%bitwise-eqv x y)
  (bitwise-not (bitwise-xor x y)))

(define bitwise-eqv (make-bitwise-folder %bitwise-eqv -1))

(define (bitwise-nand i j)
  (bitwise-not (bitwise-and i j)))

(define (bitwise-nor i j)
  (bitwise-not (bitwise-ior i j)))

(define (bitwise-andc1 i j)
  (bitwise-and (bitwise-not i) j))

(define (bitwise-andc2 i j)
  (bitwise-and i (bitwise-not j)))

(define (bitwise-orc1 i j)
  (bitwise-ior (bitwise-not i) j))

(define (bitwise-orc2 i j)
  (bitwise-ior i (bitwise-not j)))

(define (bitwise-if mask i j)
  (bitwise-ior (bitwise-and mask i)
               (bitwise-andc1 mask j)))

(define (least-significant-bit i)
  (bitwise-and i 1))

(define (number->boolean i)
  (if (= i 0) #f #t))

(define (boolean->number b)
  (if b 1 0))

(define (bit-set? index i)
  (number->boolean (least-significant-bit (arithmetic-shift i (- index)))))

(define (copy-bit index i set?)
  (if set?
      (bitwise-ior i (arithmetic-shift 1 index))
      (bitwise-and i (bitwise-not (arithmetic-shift 1 index)))))

(define (bit-swap index-1 index-2 i)
  (let ((b1 (bit-set? index-1 i))
        (b2 (bit-set? index-2 i)))
    (copy-bit index-2 (copy-bit index-1 i b2) b1)))

(define (any-bit-set? test-bits i)
  (not (zero? (bitwise-and test-bits i))))

(define (every-bit-set? test-bits i)
  (= test-bits (bitwise-and test-bits i)))

(define (lowest-bits-mask n)
  (- (arithmetic-shift 1 n) 1))

(define (bits-mask start end)
  (arithmetic-shift (lowest-bits-mask (- end start)) start))

(define (bit-field i start end)
  (bitwise-and (arithmetic-shift i (- start))
               (lowest-bits-mask (- end start))))

(define (bit-field-any? i start end)
  (not (zero? (bit-field i start end))))

(define (bit-field-every? i start end)
  (= (bit-field i start end) (lowest-bits-mask (- end start))))

(define (bit-field-clear i start end)
  (bitwise-and i (bitwise-not (bits-mask start end))))

(define (bit-field-set i start end)
  (bitwise-ior i (bits-mask start end)))

(define (bit-field-replace dest source start end)
  (let ((replacement-bits (bitwise-and source (lowest-bits-mask (- end start)))))
    (bitwise-ior (bit-field-clear dest start end)
                 (arithmetic-shift replacement-bits start))))

(define (bit-field-replace-same dest source start end)
  (let ((replacement-bits (bitwise-and source (bits-mask start end))))
    (bitwise-ior (bit-field-clear dest start end) replacement-bits)))

(define (bit-field-rotate i count start end)
  (let* ((len (- end start))
         (truncated-count (floor-remainder count len))
         (bits (bit-field i start end))
         (shifted-bits (bitwise-and (lowest-bits-mask len) (arithmetic-shift bits truncated-count)))
         (low-bits (arithmetic-shift bits (- truncated-count len)))
         (rotated-bits (bitwise-ior shifted-bits low-bits)))
    (bitwise-ior (bit-field-clear i start end) (arithmetic-shift rotated-bits start))))

(define (reverse-bits n len)
  (do ((n n (arithmetic-shift n -1))
       (count 0 (+ count 1))
       (accum 0 (bitwise-ior (arithmetic-shift accum 1)
                             (bitwise-and n 1))))
      ((= count len) accum)))

(define (bit-field-reverse i start end)
  (bitwise-ior
   (bit-field-clear i start end)
   (arithmetic-shift (reverse-bits (bit-field i start end) (- end start))
                     start)))

(define (bits->list i . maybe-len)
  (let ((len (if (null? maybe-len) (integer-length i) (car maybe-len))))
    (let loop ((n len) (i i))
      (if (zero? n)
          '()
          (cons (number->boolean (bitwise-and 1 i))
                (loop (- n 1) (arithmetic-shift i -1)))))))

(define (bits->vector i . maybe-len)
  (let ((len (if (null? maybe-len) (integer-length i) (car maybe-len))))
    (do ((result (make-vector len))
         (n 0 (+ n 1))
         (i i (arithmetic-shift i -1)))
        ((= n len) result)
      (vector-set! result n (number->boolean (bitwise-and i 1))))))

(define (list->bits lst)
  (let loop ((lst lst) (shift 0) (accum 0))
    (if (null? lst)
        accum
        (loop (cdr lst)
              (+ shift 1)
              (bitwise-ior accum
                           (arithmetic-shift (boolean->number (car lst)) shift))))))

(define (vector->bits v)
  (do ((i 0 (+ i 1))
       (result 0 (bitwise-ior result
                              (arithmetic-shift (boolean->number (vector-ref v i)) i))))
      ((= i (vector-length v)) result)))

(define (bits . bs)
  (list->bits bs))

(define (bitwise-fold proc seed i)
  (let loop ((i i) (accum seed))
    (if (zero? i)
        accum
        (loop (arithmetic-shift i -1)
              (proc (number->boolean (bitwise-and i 1)) accum)))))

(define (bitwise-for-each proc i)
  (let loop ((i i))
    (unless (zero? i)
      (proc (number->boolean (bitwise-and i 1)))
      (loop (arithmetic-shift i -1)))))

(define (bitwise-unfold stop? mapper successor seed)
  (let loop ((state seed) (shift 0) (accum 0))
    (if (stop? state)
        accum
        (loop (successor state)
              (+ shift 1)
              (bitwise-ior accum (arithmetic-shift (boolean->number (mapper state)) shift))))))

(define (make-bitwise-generator i)
  (lambda ()
    (let ((current (number->boolean (bitwise-and i 1))))
      (set! i (arithmetic-shift i -1))
      current)))
