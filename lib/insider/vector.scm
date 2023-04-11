(library (insider vector))
(import (insider syntax) (insider basic-procedures) (insider numeric) (insider list)
        (insider control)
        (only (insider internal)
              vector list->vector vector->list vector-append vector->list
              vector-ref vector-set! make-vector vector-length))
(export vector make-vector list->vector vector->list vector-append vector-length vector-ref vector-set!
        vector? vector-tabulate vector-copy vector-copy! vector->list vector-fill!
        vector-for-each vector-map)

(define-type-predicate vector? insider::vector)

(define (vector-tabulate proc len)
  (do ((i 0 (+ i 1))
       (result (make-vector len)))
      ((= i len) result)
    (vector-set! result i (proc i))))

(define (vector-copy v (start 0) (end (vector-length v)))
  (let ((length (- end start)))
    (do ((result (make-vector length))
         (src start (+ src 1))
         (dst 0 (+ dst 1)))
        ((= src end) result)
      (vector-set! result dst (vector-ref v src)))))

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

(define (vector-copy! to at from (start 0) (end (vector-length from)))
  (if (and (eq? to from) (< start at))
      (vector-copy!/backward to at from start end)
      (vector-copy!/forward to at from start end)))

(define (vector-fill! v fill (start 0) (end (vector-length v)))
  (do ((current start (+ current 1)))
      ((= current end))
    (vector-set! v current fill)))

(define (vector-for-each/1 proc v)
  (let ((l (vector-length v)))
    (do ((i 0 (+ i 1)))
        ((= i l))
      (proc (vector-ref v i)))))

(define (min-length list-of-vectors)
  (apply min (map vector-length list-of-vectors)))

(define (vector-for-each/many proc list-of-vectors)
  (let ((length (min-length list-of-vectors)))
    (do ((i 0 (+ i 1)))
        ((= i length))
      (apply proc (map (lambda (v) (vector-ref v i)) list-of-vectors)))))

(define (vector-for-each proc v1 . vectors-rest)
  (if (null? vectors-rest)
      (vector-for-each/1 proc v1)
      (vector-for-each/many proc (cons v1 vectors-rest))))

(define (vector-map/1 proc v)
  (let ((l (vector-length v)) (result #f))
    (let f ((i 0))
      (cond ((= i l)
             (set! result (make-vector l)))
            (else
             (let ((elem (proc (vector-ref v i))))
               (f (+ i 1))
               (vector-set! result i elem)))))
    result))

(define (vector-map/many proc list-of-vectors)
  (let ((l (min-length list-of-vectors)) (result #f))
    (let f ((i 0))
      (cond ((= i l)
             (set! result (make-vector l)))
            (else
             (let ((elem (apply proc (map (lambda (v)
                                            (vector-ref v i))
                                          list-of-vectors))))
               (f (+ i 1))
               (vector-set! result i elem)))))
    result))

(define (vector-map proc v1 . vectors-rest)
  (if (null? vectors-rest)
      (vector-map/1 proc v1)
      (vector-map/many proc (cons v1 vectors-rest))))
