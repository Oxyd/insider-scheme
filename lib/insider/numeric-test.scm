(define (test-comparison)
  (test-group "comparison"
    (test-equal 3 (min 3 4))
    (test (exact? (min 3 4)))
    (test-equal 3.0 (min 3 4.5))
    (test (inexact? (min 3 4.5)))

    (test-equal 4 (max 3 4))
    (test (exact? (max 3 4)))
    (test-equal 4.0 (max 3.9 4))
    (test (inexact? (max 3.9 4)))))

(define (test-division)
  (test-group "division"
    (test-values-equal (2 1) (truncate/ 5 2))
    (test-values-equal (-2 -1) (truncate/ -5 2))
    (test-values-equal (-2 1) (truncate/ 5 -2))
    (test-values-equal (2 -1) (truncate/ -5 -2))
    (test-values-equal (0 1208925819614629174706176) (truncate/ 1208925819614629174706176 1606938044258990275541962092341162602522202993782792835301376))

    (test-values-equal (9223372036854775808 1) (truncate/ 18446744073709551617 2))
    (test-values-equal (-9223372036854775808 -1) (truncate/ -18446744073709551617 2))
    (test-values-equal (-9223372036854775808 1) (truncate/ 18446744073709551617 -2))
    (test-values-equal (9223372036854775808 -1) (truncate/ -18446744073709551617 -2))

    (test-values-equal (2 1) (floor/ 5 2))
    (test-values-equal (-3 1) (floor/ -5 2))
    (test-values-equal (-3 -1) (floor/ 5 -2))
    (test-values-equal (2 -1) (floor/ -5 -2))
    (test-values-equal (2.0 1.0) (floor/ 5.0 2))))

(define (test-fraction)
  (test-group "fraction"
    (test-equal 3 (numerator (/ 6 4)))
    (test-equal 2 (denominator (/ 6 4)))
    (test-equal 3.0 (numerator (inexact (/ 6 4))))
    (test-equal 2.0 (denominator (inexact (/ 6 4))))
    (test-equal 1 (denominator 0))
    (test-equal 1.0 (denominator 0.0))))

(define (test-categories)
  (test-group "categories"
    (test (complex? 3+4i))
    (test (complex? 3))
    (test (real? 3))
    (test (real? -2.5+0i))
    (test (real? -2.5+0.0i))
    (test (real? #e1e10))
    (test (real? +inf.0))
    (test (real? +nan.0))
    (test-false (rational? -inf.0))
    (test (rational? 3.5))
    (test (rational? 6/10))
    (test (rational? 6/3))
    (test (integer? 3+0i))
    (test (integer? 3.0))
    (test (integer? 8/4))

    (test (positive? 1))
    (test (positive? 1.0))
    (test (positive? 1/2))
    (test (positive? 0.1))
    (test-false (positive? 0))
    (test-false (positive? -1))
    (test-false (positive? -1/2))

    (test (negative? -1))
    (test (negative? -1.0))
    (test (negative? -1/2))
    (test (negative? -0.1))
    (test-false (negative? 1))
    (test-false (negative? 0))

    (test-false (infinite? 3))
    (test (infinite? +inf.0))
    (test-false (infinite? +nan.0))
    (test (infinite? 3.0+inf.0i))

    (test (nan? +nan.0))
    (test-false (nan? 32))
    (test (nan? +nan.0+5.0i))
    (test-false (nan? 1+2i))))

(define (test-rounding)
  (test-group "rounding"
    (test-equal -5.0 (floor -4.3))
    (test-equal 3.0 (floor 3.5))
    (test-equal 1 (floor 12/7))
    (test-equal -2 (floor -12/7))

    (test-equal -4.0 (ceiling -4.3))
    (test-equal 4.0 (ceiling 3.5))
    (test-equal 2 (ceiling 12/7))
    (test-equal -1 (ceiling -12/7))

    (test-equal -4.0 (truncate -4.3))
    (test-equal 3.0 (truncate 3.5))
    (test-equal 1 (truncate 12/7))
    (test-equal -1 (truncate -12/7))

    (test-equal -4.0 (round -4.3))
    (test-equal 4.0 (round 3.5))
    (test-equal 2.0 (round 2.5))
    (test-equal 13.0 (round 13.1))
    (test-equal 13.0 (round 12.8))
    (test-equal 4 (round 7/2))
    (test-equal -4 (round -7/2))
    (test-equal 2 (round 12/7))
    (test-equal -2 (round -12/7))
    (test-equal 7 (round 7))))

(define (test-bitwise)
  (test-group "bitwise operations"
    (test-equal -11 (bitwise-not 10))
    (test-equal 36 (bitwise-not -37))
    (test-equal -1 (bitwise-not 0))
    (test-equal -18446744073709551618 (bitwise-not 18446744073709551617))
    (test-equal 18446744073709551616 (bitwise-not -18446744073709551617))
    (test-equal -1606938044258990275541962092341162602522202993782792835301377 (bitwise-not 1606938044258990275541962092341162602522202993782792835301376))

    (test-equal 10 (bitwise-and 11 26))
    (test-equal 0 (bitwise-and #b1101 #b0010))
    (test-equal #b0010 (bitwise-and #b1110 #b0010))
    (test-equal -1 (bitwise-and))
    (test-equal 5 (bitwise-and 5))
    (test-equal #b0001 (bitwise-and #b1101 #b1001 #b0011))
    (test-equal #x8AC1000000000000000000 (bitwise-and #xABCD000000000000000000 #xDEF1000000000000000000))
    (test-equal 5 (bitwise-and 5 -1))
    (test-equal #x7fffffffffffffff (bitwise-and -1 #x7fffffffffffffff))
    (test-equal #x4000000000000000 (bitwise-and -1 #x4000000000000000))
    (test-equal #x8000000000000000 (bitwise-and -1 #x8000000000000000))
    (test-equal #x80000000000000000 (bitwise-and -1 #x80000000000000000))

    (test-equal 11 (bitwise-ior 3 10))
    (test-equal -1 (bitwise-ior 5 -1))
    (test-equal #b1101 (bitwise-ior #b1100 #b0001))
    (test-equal #xc000000000000000 (bitwise-ior #x4000000000000000 #x8000000000000000))
    (test-equal 0 (bitwise-ior))
    (test-equal 4 (bitwise-ior 4))
    (test-equal #b111 (bitwise-ior #b100 #b010 #b001))

    (test-equal 9 (bitwise-xor 3 10))
    (test-equal -11 (bitwise-xor 10 -1))
    (test-equal #b1101 (bitwise-xor #b1011 #b0110))
    (test-equal 0 (bitwise-xor))
    (test-equal 9 (bitwise-xor 9))
    (test-equal #b100 (bitwise-xor #b101 #b011 #b010))

    (test-equal -42 (bitwise-eqv 37 12))
    (test-equal -1 (bitwise-eqv))
    (test-equal 15 (bitwise-eqv 15))
    (test-equal #b010 (bitwise-eqv #b001 #b101 #b110))

    (test-equal -11 (bitwise-nand 11 26))
    (test-equal -28 (bitwise-nor 11 26))
    (test-equal 16 (bitwise-andc1 11 26))
    (test-equal 1 (bitwise-andc2 11 26))
    (test-equal -2 (bitwise-orc1 11 26))
    (test-equal -17 (bitwise-orc2 11 26))

    (test-equal 32 (arithmetic-shift 8 2))
    (test-equal 4 (arithmetic-shift 4 0))
    (test-equal 4 (arithmetic-shift 8 -1))
    (test-equal -79 (arithmetic-shift -100000000000000000000000000000000 -100))
    (test-equal 78 (arithmetic-shift 100000000000000000000000000000000 -100))
    (test-equal 287342913912354160942190067590682971928513585409425408 (arithmetic-shift 12 174))
    (test-equal -287342913912354160942190067590682971928513585409425408 (arithmetic-shift -12 174))
    (test-equal 2305843009213693952 (arithmetic-shift 1 61))
    (test-equal 4611686018427387904 (arithmetic-shift 1 62))
    (test-equal 9223372036854775808 (arithmetic-shift 1 63))
    (test-equal 18446744073709551616 (arithmetic-shift 1 64))
    (test-equal 36893488147419103232 (arithmetic-shift 1 65))
    (test-equal -2305843009213693952 (arithmetic-shift -1 61))
    (test-equal -4611686018427387904 (arithmetic-shift -1 62))
    (test-equal -9223372036854775808 (arithmetic-shift -1 63))
    (test-equal -18446744073709551616 (arithmetic-shift -1 64))
    (test-equal -36893488147419103232 (arithmetic-shift -1 65))

    (test-equal 0 (bit-count 0))
    (test-equal 0 (bit-count -1))
    (test-equal 3 (bit-count 7))
    (test-equal 3 (bit-count 13))
    (test-equal 2 (bit-count -13))
    (test-equal 4 (bit-count 30))
    (test-equal 4 (bit-count -30))
    (test-equal 1 (bit-count (expt 2 100)))
    (test-equal 100 (bit-count (- (expt 2 100))))
    (test-equal 1 (bit-count (- (+ 1 (expt 2 100)))))

    (test-equal 0 (integer-length 0))
    (test-equal 1 (integer-length 1))
    (test-equal 0 (integer-length -1))
    (test-equal 3 (integer-length 7))
    (test-equal 3 (integer-length -7))
    (test-equal 4 (integer-length 8))
    (test-equal 3 (integer-length -8))
    (test-equal 65 (integer-length (expt 2 64)))
    (test-equal 65 (integer-length (expt -2 64)))
    (test-equal 97 (integer-length 100000000000000000000000000000))

    (test-equal 9 (bitwise-if 3 1 8))
    (test-equal 0 (bitwise-if 3 8 1))
    (test-equal 3 (bitwise-if 1 1 2))
    (test-equal #b00110011 (bitwise-if #b00111100 #b11110000 #b00001111))

    (test-false (bit-set? 1 1))
    (test (bit-set? 0 1))
    (test (bit-set? 3 10))
    (test (bit-set? 1000000 -1))
    (test (bit-set? 2 6))
    (test-false (bit-set? 0 6))

    (test-equal 1 (copy-bit 0 0 #t))
    (test-equal #b100 (copy-bit 2 0 #t))
    (test-equal #b1011 (copy-bit 2 #b1111 #f))

    (test-equal 1 (bit-swap 0 2 4))

    (test (any-bit-set? 3 6))
    (test-false (any-bit-set? 3 12))
    (test (every-bit-set? 4 6))
    (test-false (every-bit-set? 7 6))

    (test-equal 0 (first-set-bit 1))
    (test-equal 1 (first-set-bit 2))
    (test-equal -1 (first-set-bit 0))
    (test-equal 3 (first-set-bit 40))
    (test-equal 2 (first-set-bit -28))
    (test-equal 99 (first-set-bit (expt 2 99)))
    (test-equal 99 (first-set-bit (expt -2 99)))

    (test-equal #b1010 (bit-field #b1101101010 0 4))
    (test-equal #b101101 (bit-field #b1101101010 3 9))
    (test-equal #b10110 (bit-field #b1101101010 4 9))
    (test-equal #b110110 (bit-field #b1101101010 4 10))
    (test-equal 0 (bit-field 6 0 1))
    (test-equal 3 (bit-field 6 1 3))
    (test-equal 1 (bit-field 6 2 999))
    (test-equal 1 (bit-field #x100000000000000000000000000000000 128 129))

    (test (bit-field-any? #b1001001 1 6))
    (test-false (bit-field-any? #b1000001 1 6))
    (test (bit-field-every? #b1011110 1 5))
    (test-false (bit-field-every? #b1011010 1 5))

    (test-equal #b100000 (bit-field-clear #b101010 1 4))
    (test-equal #b101110 (bit-field-set #b101010 1 4))

    (test-equal #b100100 (bit-field-replace #b101010 #b010 1 4))
    (test-equal #b111 (bit-field-replace #b110 1 0 1))
    (test-equal #b110 (bit-field-replace #b110 1 1 2))

    (test-equal #b110 (bit-field-rotate #b110 0 0 10))
    (test-equal #b110 (bit-field-rotate #b110 0 0 256))
    (test-equal 1 (bit-field-rotate #x100000000000000000000000000000000 1 0 129))
    (test-equal #b110 (bit-field-rotate #b110 1 1 2))
    (test-equal #b1010 (bit-field-rotate #b110 1 2 4))
    (test-equal #b1011 (bit-field-rotate #b0111 -1 1 4))

    (test-equal 6 (bit-field-reverse 6 1 3))
    (test-equal 12 (bit-field-reverse 6 1 4))
    (test-equal #x80000000 (bit-field-reverse 1 0 32))
    (test-equal #x40000000 (bit-field-reverse 1 0 31))
    (test-equal #x20000000 (bit-field-reverse 1 0 30))
    (test-equal 5 (bit-field-reverse #x140000000000000000000000000000000 0 129))

    (test-equal '(#t #f #t #f #t #t #t) (bits->list #b1110101))
    (test-equal '(#t #t #f #f #f) (bits->list 3 5))
    (test-equal '(#f #t #t #f) (bits->list 6 4))

    (test-equal #(#t #f #t #f #t #t #t) (bits->vector #b1110101))

    (test-equal #b1110101 (list->bits '(#t #f #t #f #t #t #t)))
    (test-equal #b111010100 (list->bits '(#f #f #t #f #t #f #t #t #t)))
    (test-equal 6 (list->bits '(#f #t #t)))
    (test-equal 6 (list->bits '(#f #t #t #f)))
    (test-equal 12 (list->bits '(#f #f #t #t)))

    (test-equal #b1110101 (vector->bits '#(#t #f #t #f #t #t #t)))
    (test-equal #b111010100 (vector->bits '#(#f #f #t #f #t #f #t #t #t)))
    (test-equal 6 (vector->bits '#(#f #t #t)))
    (test-equal 6 (vector->bits '#(#f #t #t #f)))
    (test-equal 12 (vector->bits '#(#f #f #t #t)))

    (test-equal #b1110101 (bits #t #f #t #f #t #t #t))
    (test-equal #b111010100 (bits #f #f #t #f #t #f #t #t #t))

    (test-equal '(#t #f #t #f #t #t #t) (bitwise-fold cons '() #b1010111))

    (test-equal 5
                (let ((count 0))
                  (bitwise-for-each (lambda (b) (if b (set! count (+ count 1))))
                                    #b1010111)
                  count))

    (test-equal #b101010101
                (bitwise-unfold (lambda (i) (= i 10))
                                even?
                                (lambda (i) (+ i 1))
                                0))

    (test-equal '(#f #t #t #f)
                (let ((g (make-bitwise-generator #b110)))
                  (let* ((a (g))
                         (b (g))
                         (c (g))
                         (d (g)))
                    (list a b c d))))))

(define (test-squares)
  (test-group "squares"
    (test-group "square"
      (test-equal 4 (square 2))
      (test-equal (expt 2 64) (square (expt 2 32)))
      (test-equal (expt 2 66) (square (expt 2 33)))
      (test-equal (expt 2 128) (square (expt 2 64)))
      (test-equal (expt 2 130) (square (expt 2 65)))
      (test-equal 2658455991569831744034644674968146176 (square 1630477228166597776)))

    (test-group "exact-integer-sqrt"
      (test-values-equal (2 0) (exact-integer-sqrt 4))
      (test-values-equal (2 1) (exact-integer-sqrt 5))
      (test-values-equal (100000000000000000000000000000 0) (exact-integer-sqrt 10000000000000000000000000000000000000000000000000000000000))
      (test-values-equal (100000000000000000000000000000 500) (exact-integer-sqrt 10000000000000000000000000000000000000000000000000000000500))
      (test-values-equal (2.0 1.0) (exact-integer-sqrt 5.0))
      (test-values-equal (1152921504606846976 0) (exact-integer-sqrt (expt 2 120)))
      (test-values-equal (1630477228166597776 1772969445592542976) (exact-integer-sqrt (expt 2 121)))
      (test-values-equal (31622776601683793319 62545769258890964239) (exact-integer-sqrt (expt 10 39)))
      (test-values-equal ((expt 2 70) 0) (exact-integer-sqrt (expt 2 140))))

    (test-group "sqrt"
      (test-equal 3.0 (sqrt 9.0))
      (test-equal 3.0 (sqrt 9))
      (test-equal 1.0i (sqrt -1)))))

(define (test-gcd/lcm)
  (test-group "gcd/lcm"
    (test-equal 4 (gcd 32 -36))
    (test-equal 4.0 (gcd 32.0 -36))
    (test-equal 0 (gcd))
    (test-equal 2 (gcd 6 8 12))

    (test-equal 288 (lcm 32 -36))
    (test-equal 288.0 (lcm 32.0 -36))
    (test-equal 1 (lcm))
    (test-equal 24 (lcm 6 8 12))))

(define (test-rationalize)
  (test-group "rationalize"
    (test-equal 1/3 (rationalize 3/10 1/10))
    (test-equal #i1/3 (rationalize 0.3 1/10))))

(define pi 3.141592654)
(define pi/2 1.570796327)

(define (test-complex)
  (test-group "complex"
    (test-equal 1+2i (make-rectangular 1 2))
    (test (< (magnitude (- 1.0+1.0i (make-polar (sqrt 2) (atan 1))))
             1e-6))
    (test-equal 1 (real-part 1+2i))
    (test-equal 2 (imag-part 1+2i))
    (test-equal (sqrt 2) (magnitude 1+1i))

    (test-equal (atan 1) (angle 1+1i))
    (test-equal 0 (angle +inf.0))
    (test-approximate pi (angle -inf.0) 1e-6)
    (test-equal 0 (angle +0.0))
    (test-approximate pi (angle -0.0) 1e-6)))

(define-syntax test-close
  (syntax-rules ()
    ((test-close expected expr error)
     (test (< (magnitude (- expr expected)) error)))))

(define (test-transcendental)
  (test-group "transcendental"
    (test-group "exp"
      (test-equal 1.0 (exp 0))
      (test-close 20.0855369231877 (exp 3) 1e-6)
      (test-close -1.0 (exp (* -i (* 4 (atan 1)))) 1e-6))

    (test-group "log"
      (test-equal 0.0 (log 1))
      (test-equal 1.0 (log (exp 1)))
      (test-close 42.0 (log (exp 42)) 1e-6)
      (test-close 2.0 (log 100 10) 1e-6)
      (test-close 12.0 (log 4096 2) 1e-6))

    (test-group "sin"
      (test-equal 0.0 (sin 0))
      (test-approximate 1.0 (sin pi/2) 1e-6))

    (test-group "cos"
      (test-equal 1.0 (cos 0))
      (test-approximate -1.0 (cos pi) 1e-6))

    (test-group "tan"
      (test-equal 0.0 (tan 0))
      (test-approximate 1.5574077246549 (tan 1) 1e-6))

    (test-group "asin"
      (test-equal 0.0 (asin 0))
      (test-approximate pi/2 (asin 1) 1e-6))

    (test-group "acos"
      (test-equal 0.0 (acos 1))
      (test-approximate pi (acos -1) 1e-6))

    (test-group "atan"
      (test-approximate (/ pi 4) (atan 1 1) 1e-6)
      (test-approximate 0.5880026035475675 (atan 2 3) 1e-6)
      (test-equal 0.0 (atan 0.0 1.0))
      (test-equal -0.0 (atan -0.0 1.0))
      (test-approximate pi/2 (atan 1.0 0.0) 1e-6)
      (test-approximate pi (atan 0.0 -1.0) 1e-6)
      (test-approximate (- pi) (atan -0.0 -1.0) 1e-6)
      (test-approximate (- pi/2) (atan -1.0 0.0) 1e-6)
      (test-equal 0.0 (atan 0.0 0.0))
      (test-equal -0.0 (atan -0.0 0.0))
      (test-approximate pi (atan 0.0 -0.0) 1e-6)
      (test-approximate (- pi) (atan -0.0 -0.0) 1e-6)
      (test-approximate (- pi/2) (atan -inf.0) 1e-6)
      (test-approximate pi/2 (atan +inf.0) 1e-6))))

(define (test-numeric)
  (test-group "numeric"
    (test-comparison)
    (test-division)
    (test-fraction)
    (test-categories)
    (test-rounding)
    (test-bitwise)
    (test-squares)
    (test-gcd/lcm)
    (test-rationalize)
    (test-complex)
    (test-transcendental)))

(when-main-module
 (test-numeric))
