(library (insider bytevector-test))
(import (insider syntax) (insider test) (insider bytevector) (insider control))
(export test-bytevector)

(define (test-bytevector)
  (test-group "bytevector"
    (test (bytevector? #u8(1 2 3)))
    (test-false (bytevector? #(1 2 3)))

    (test-equal #u8(5 5 5) (make-bytevector 3 5))
    (test-equal #u8(1 2 3) (bytevector 1 2 3))
    (test-error (bytevector 400 500))

    (test-equal 3 (bytevector-length #u8(1 2 3)))
    (test-equal 0 (bytevector-length #u8()))

    (test-equal 12 (bytevector-u8-ref #u8(10 11 12 13 14) 2))

    (test-equal #u8(1 3 3 4)
                (let ((bv (bytevector 1 2 3 4)))
                  (bytevector-u8-set! bv 1 3)
                  bv))

    (test-equal #u8(3 4) (bytevector-copy #u8(1 2 3 4 5) 2 4))

    (test-equal #u8(10 1 2 40 50)
                (let ((a (bytevector 1 2 3 4 5))
                      (b (bytevector 10 20 30 40 50)))
                  (bytevector-copy! b 1 a 0 2)
                  b))

    (test-equal #u8(1 1 2 4 5)
                (let ((a (bytevector 1 2 3 4 5)))
                  (bytevector-copy! a 1 a 0 2)
                  a))

    (test-equal #u8(0 1 2 3 4 5)
                (bytevector-append #u8(0 1 2) #u8(3 4 5)))))

(when-main-module
 (test-bytevector))
