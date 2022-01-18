(library (insider vector))
(import (insider syntax) (insider basic-procedures) (insider opt-lambda)
        (except (insider internal) define let))
(export vector make-vector list->vector vector->list vector-append vector-length vector-ref vector-set!
        vector? vector-tabulate vector-copy vector-copy! vector->list vector-fill!)

(define-type-predicate vector? insider::vector)

(define (vector-tabulate proc len)
  (do ((i 0 (+ i 1))
       (result (make-vector len)))
      ((= i len) result)
    (vector-set! result i (proc i))))

(define vector-copy
  (opt-lambda (v (start 0) (end (vector-length v)))
    (let ((length (- end start)))
      (do ((result (make-vector length))
           (src start (+ src 1))
           (dst 0 (+ dst 1)))
          ((= src end) result)
        (vector-set! result dst (vector-ref v src))))))

(define (vector-copy!/forward to at from start end)
  (do ((from-current start (+ from-current 1))
       (to-current at (+ to-current 1)))
      ((= from-current end))
    (vector-set! to to-current (vector-ref from from-current))))

(define (vector-copy!/backward to at from start end)
  (do ((from-current end (- from-current 1))
       (to-current (+ at (- end start)) (- to-current 1)))
      ((= from-current start))
    (vector-set! to (- to-current 1) (vector-ref from (- from-current 1)))))

(define vector-copy!
  (opt-lambda (to at from (start 0) (end (vector-length from)))
    (if (and (eq? to from) (< start at))
        (vector-copy!/backward to at from start end)
        (vector-copy!/forward to at from start end))))

(define vector-fill!
  (opt-lambda (v fill (start 0) (end (vector-length v)))
    (do ((current start (+ current 1)))
        ((= current end))
      (vector-set! v current fill))))
