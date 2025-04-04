;;> Returns a newly allocated bytevector containing the bytes in @c{bv} between
;;> @c{start} and @c{end}.
;;>
;;> @example{
;;>   @code{
;;>     (bytevector-copy #u8(1 2 3 4 5) 2 4)
;;>     @evaluates-to{#u8(3 4)}
;;>   }
;;> }
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

;;> Copies the bytes of bytevector @c{from} between @c{start} and @c{end} to
;;> bytevector @c{to} starting at @c{at}. If the source and destination overlap,
;;> copying takes place as if the source is first copied to a temporary
;;> bytevector and then into the destination.
;;>
;;> @example{
;;>   @code{
;;>     (define a (bytevector 1 2 3 4 5))
;;>     (define b (bytevector 10 20 30 40 50))
;;>     (bytevector-copy! b 1 a 0 2)
;;>     b @evaluates-to{#u8(10 1 2 40 50)}
;;>   }
;;> }
(define (bytevector-copy! to at from (start 0) (end (bytevector-length from)))
  (if (and (eq? to from) (< start at))
      (bytevector-copy!/backward to at from start end)
      (bytevector-copy!/forward to at from start end)))

(define (bytevectors-total-length bvs)
  (let loop ((accum 0) (bvs bvs))
    (if (null? bvs)
        accum
        (loop (+ accum (bytevector-length (car bvs))) (cdr bvs)))))

;;> Returns a newly allocated bytevector whose elements are the concatenation
;;> of the elements in @c{bvs}.
;;>
;;> @example{
;;>   @code{
;;>     (bytevector-append #u8(0 1 2) #u8(3 4 5))
;;>     @evaluates-to{#u8(0 1 2 3 4 5)}
;;>   }
;;> }
(define (bytevector-append . bvs)
  (let ((result (make-bytevector (bytevectors-total-length bvs))))
    (do ((bvs bvs (cdr bvs))
         (current 0 (+ current (bytevector-length (car bvs)))))
        ((null? bvs) result)
      (bytevector-copy! result current (car bvs)))))
