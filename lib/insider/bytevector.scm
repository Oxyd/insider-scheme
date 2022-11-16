(library (insider bytevector))
(import (insider syntax) (insider basic-procedures) (insider list) (insider numeric)
        (only (insider internal)
              bytevector bytevector-length bytevector-u8-ref bytevector-u8-set! make-bytevector))
(export bytevector? make-bytevector bytevector bytevector-length bytevector-u8-ref bytevector-u8-set!
        bytevector-copy bytevector-copy! bytevector-append)

(define-type-predicate bytevector? insider::bytevector)

(define (bytevector-copy bv (start 0) (end (bytevector-length bv)))
  (let ((length (- end start)))
    (do ((result (make-bytevector length))
         (src start (+ src 1))
         (dst 0 (+ dst 1)))
        ((= src end) result)
      (bytevector-u8-set! result dst (bytevector-u8-ref bv src)))))

(define (bytevector-copy!/forward to at from start end)
  (do ((from-current start (+ from-current 1))
       (to-current at (+ to-current 1)))
      ((= from-current end))
    (bytevector-u8-set! to to-current (bytevector-u8-ref from from-current))))

(define (bytevector-copy!/backward to at from start end)
  (do ((from-current end (- from-current 1))
       (to-current (+ at (- end start)) (- to-current 1)))
      ((= from-current start))
    (bytevector-u8-set! to (- to-current 1) (bytevector-u8-ref from (- from-current 1)))))

(define (bytevector-copy! to at from (start 0) (end (bytevector-length from)))
  (if (and (eq? to from) (< start at))
      (bytevector-copy!/backward to at from start end)
      (bytevector-copy!/forward to at from start end)))

(define (bytevectors-total-length bvs)
  (let loop ((accum 0) (bvs bvs))
    (if (null? bvs)
        accum
        (loop (+ accum (bytevector-length (car bvs))) (cdr bvs)))))

(define (bytevector-append . bvs)
  (let ((result (make-bytevector (bytevectors-total-length bvs))))
    (do ((bvs bvs (cdr bvs))
         (current 0 (+ current (bytevector-length (car bvs)))))
        ((null? bvs) result)
      (bytevector-copy! result current (car bvs)))))
