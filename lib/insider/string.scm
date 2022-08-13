(library (insider string))
(import (insider syntax) (insider numeric) (insider basic-procedures) (scheme case-lambda) (insider opt-lambda)
        (insider vector) (insider control) (insider list) (insider error) (insider bytevector)
        (insider char)
        (only (insider internal)
              string string-ref string-set! string-append string-null? string-length make-string
              string-byte-length next-code-point-byte-index previous-code-point-byte-index
              string-ref/byte-index string-set!/byte-index string-copy/byte-indexes
              string-contains/byte-indexes string-contains-right/byte-indexes
              string-append-char! string-append! string-reverse*
              string=?/pair string<?/pair string<=?/pair string>?/pair string>=?/pair
              string-foldcase string->utf8/byte-indexes utf8->string*
              symbol->string number->string datum->string string->number string-upcase string-downcase))
(export
 string?

 string-append string-append! string-append-char! string-length
 symbol->string string make-string number->string datum->string
 string-ref string-set! string->number

 string=? string<? string<=? string>? string>=?
 string-ci=? string-ci<? string-ci<=? string-ci>? string-ci>=?

 string-upcase string-downcase string-foldcase
 string->utf8 utf8->string

 string-cursor? string-cursor-start string-cursor-end string-cursor-next string-cursor-prev
 string-cursor-forward string-cursor-back
 string-cursor=? string-cursor<? string-cursor<=? string-cursor>? string-cursor>=? string-cursor-diff
 string-cursor->index string-index->cursor
 string-ref/cursor

 string-null? string-every string-any

 string-tabulate string-unfold string-unfold-right

 string->list string->list/cursors list->string reverse-list->string
 string->vector string->vector/cursors string-join

 string-copy string-copy/cursors substring substring/cursors string-copy! string-fill!
 string-take string-drop string-take-right string-drop-right
 string-pad string-pad-right
 string-trim string-trim-right string-trim-both

 string-prefix-length string-suffix-length string-prefix? string-suffix?

 string-index string-index-right string-skip string-skip-right string-contains string-contains-right

 string-reverse string-concatenate string-concatenate-reverse string-fold string-fold-right
 string-for-each-cursor string-for-each string-map
 string-replicate string-count string-replace string-split string-filter string-remove

 vector->string string->vector)

(define-type-predicate string? insider::string)

;; String cursors are represented as exact negative integers. Byte index b is
;; represented as the string cursor with value -b - 1. This is to ensure that
;; the start-of-string cursor is distinct from the start-of-string index.

(define (string-cursor? obj)
  (and (exact-integer? obj) (< obj 0)))

(define (string-index? obj)
  (and (exact-integer? obj) (>= obj 0)))

(define (string-cursor->byte-index c)
  ;; -(c + 1) = -c - 1 = -1 - c
  (- -1 c))

(define (byte-index->string-cursor bi)
  ;; -b - 1 = -b - 1 = -1 - b
  (- -1 bi))

(define (string-cursor-start s)
  (byte-index->string-cursor 0))

(define (string-cursor-end s)
  (byte-index->string-cursor (string-byte-length s)))

(define (string-cursor-next s cursor)
  (if (string-cursor? cursor)
      (byte-index->string-cursor (next-code-point-byte-index s (string-cursor->byte-index cursor)))
      (+ cursor 1)))

(define (string-cursor-prev s cursor)
  (if (string-cursor? cursor)
      (byte-index->string-cursor (previous-code-point-byte-index s (string-cursor->byte-index cursor)))
      (- cursor 1)))

(define (string-cursor-forward s cursor nchars)
  (if (string-cursor? cursor)
      (do ((i 0 (+ i 1))
           (result cursor (string-cursor-next s result)))
          ((= i nchars) result))
      (+ cursor nchars)))

(define (string-cursor-back s cursor nchars)
  (if (string-cursor? cursor)
      (do ((i nchars (- i 1))
           (result cursor (string-cursor-prev s result)))
          ((= i 0) result))
      (- cursor nchars)))

(define (string-ref/cursor s cursor)
  (if (string-cursor? cursor)
      (string-ref/byte-index s (string-cursor->byte-index cursor))
      (string-ref s cursor)))

(define (string-cursor=? c1 c2)
  (= c1 c2))

(define (string-cursor<? c1 c2)
  ;; -x - 1 < -y - 1 <=> -x < -y <=> x > y
  (if (and (string-cursor? c1) (string-cursor? c2))
      (> c1 c2)
      (< c1 c2)))

(define (string-cursor<=? c1 c2)
  (or (string-cursor=? c1 c2) (string-cursor<? c1 c2)))

(define (string-cursor>? c1 c2)
  (not (string-cursor<=? c1 c2)))

(define (string-cursor>=? c1 c2)
  (not (string-cursor<? c1 c2)))

(define (string-byte-index-diff s i j)
  (do ((result 0 (+ result 1))
       (index i (next-code-point-byte-index s index)))
      ((= index j) result)))

(define (string-cursor-diff s start end)
  (if (and (string-cursor? start) (string-cursor? end))
      (string-byte-index-diff s (string-cursor->byte-index start) (string-cursor->byte-index end))
      (- end start)))

(define (string-cursor->index s cursor)
  (if (string-index? cursor)
      cursor
      (string-cursor-diff s (string-cursor-start s) cursor)))

(define (string-index->cursor s index)
  (if (string-cursor? index)
      index
      (string-cursor-forward s (string-cursor-start s) index)))

(define string-every
  (opt-lambda (pred s (start* (string-cursor-start s)) (end* (string-cursor-end s)))
    (let ((start (string-index->cursor s start*))
          (end (string-index->cursor s end*)))
      (let loop ((current start) (last-value #t))
        (if (= current end)
            last-value
            (let ((current-value (pred (string-ref/cursor s current))))
              (if current-value
                  (loop (string-cursor-next s current) current-value)
                  #f)))))))

(define string-any
  (opt-lambda (pred s (start* (string-cursor-start s)) (end* (string-cursor-end s)))
    (let ((start (string-index->cursor s start*))
          (end (string-index->cursor s end*)))
      (let loop ((current start))
        (if (= current end)
            #f
            (let ((value (pred (string-ref/cursor s current))))
              (if value
                  value
                  (loop (string-cursor-next s current)))))))))

(define (string-tabulate proc len)
  (vector->string (vector-tabulate proc len)))

(define (string-append/share x y)
  (if (string-null? y)
      x
      (string-append x y)))

(define string-unfold
  (opt-lambda (stop? mapper successor initial-seed (base (string)) (make-final (lambda (_) "")))
    (do ((result (string-copy base))
         (seed initial-seed (successor seed)))
        ((stop? seed) (string-append/share result (make-final seed)))
      (string-append-char! result (mapper seed)))))

(define (string-unfold-right . args)
  (string-reverse (apply string-unfold args)))

(define (->byte-index s i)
  (string-cursor->byte-index (string-index->cursor s i)))

(define string->list/cursors
  (opt-lambda (s (start* (string-cursor-start s)) (end* (string-cursor-end s)))
    (let ((start (->byte-index s start*))
          (end (->byte-index s end*)))
      (do ((index start (next-code-point-byte-index s index))
           (accum '() (cons (string-ref/byte-index s index) accum)))
          ((= index end) (reverse accum))))))

(define string->list string->list/cursors)

(define (list->string list)
  (do ((result (string))
       (current list (cdr current)))
      ((null? current) result)
    (string-append-char! result (car current))))

(define (reverse-list->string list)
  (let ((result (string)))
    (let loop ((l list))
      (unless (null? l)
        (loop (cdr l))
        (string-append-char! result (car l))))
    result))

(define string->vector/cursors
  (opt-lambda (s (start* (string-cursor-start s)) (end* (string-cursor-end s)))
    (let ((start (->byte-index s start*))
          (end (->byte-index s end*)))
      (do ((result (make-vector (string-cursor-diff s start end)))
           (vector-index 0 (+ vector-index 1))
           (string-index start (next-code-point-byte-index s string-index)))
          ((= string-index end) result)
        (vector-set! result vector-index (string-ref/byte-index s string-index))))))

(define string->vector string->vector/cursors)

(define (string-join/prefix string-list delimiter)
  (do ((result (string))
       (element string-list (cdr element)))
      ((null? element) result)
    (string-append! result delimiter)
    (string-append! result (car element))))

(define (string-join/suffix string-list delimiter)
  (do ((result (string))
       (element string-list (cdr element)))
      ((null? element) result)
    (string-append! result (car element))
    (string-append! result delimiter)))

(define (string-join/infix string-list delimiter strict?)
  (if (null? string-list)
      (if strict? (error "string-join: Empty list") "")
      (string-append (car string-list) (string-join/prefix (cdr string-list) delimiter))))

(define string-join
  (opt-lambda (string-list (delimiter "") (grammar 'infix))
    (case grammar
      ((infix strict-infix)
       (string-join/infix string-list delimiter (eq? grammar 'strict-infix)))
      ((prefix)
       (string-join/prefix string-list delimiter))
      ((suffix)
       (string-join/suffix string-list delimiter))
      (else
       (error "Invalid grammar" grammar)))))

(define string-copy/cursors
  (opt-lambda (s (start* (string-cursor-start s)) (end* (string-cursor-end s)))
    (let ((start (->byte-index s start*)) (end (->byte-index s end*)))
      (string-copy/byte-indexes s start end))))

(define string-copy string-copy/cursors)

(define (entire-string? s start end)
  (and (= (string-index->cursor s start) (string-cursor-start s))
       (= (string-index->cursor s end) (string-cursor-end s))))

(define (substring/cursors s start end)
  (if (entire-string? s start end)
      s
      (string-copy/cursors s start end)))

(define (substring s start end)
  (string-copy/cursors s start end))

(define (string-copy!/forward to at from start end)
  (let ((at (->byte-index to at))
        (start (->byte-index from start))
        (end (->byte-index from end)))
    (do ((from-current start (next-code-point-byte-index from from-current))
         (to-current at (next-code-point-byte-index to to-current)))
        ((= from-current end))
      (string-set!/byte-index to to-current (string-ref/byte-index from from-current)))))

(define (string-copy!/backward to at from start end)
  (let* ((start* (->byte-index from start))
         (end* (->byte-index from end))
         (length (string-cursor-diff from start end))
         (to-end (->byte-index to (string-cursor-forward to at length))))
    (let loop ((from-current end*) (to-current to-end))
      (let ((from-current-1 (previous-code-point-byte-index from from-current))
            (to-current-1 (previous-code-point-byte-index to to-current)))
        (string-set!/byte-index to to-current-1 (string-ref/byte-index from from-current-1))
        (unless (= from-current-1 start*)
          (loop from-current-1 to-current-1))))))

(define string-copy!
  (opt-lambda (to at from (start (string-cursor-start from)) (end (string-cursor-end from)))
    (if (and (eq? to from) (string-cursor<? start at))
        (string-copy!/backward to at from start end)
        (string-copy!/forward to at from start end))))

(define string-fill!
  (opt-lambda (s fill (start (string-cursor-start s)) (end (string-cursor-end s)))
    (let ((length (string-cursor-diff s start end)))
      (do ((current (->byte-index s start) (next-code-point-byte-index s current))
           (count length (- count 1)))
          ((= count 0))
        (string-set!/byte-index s current fill)))))

(define (string-take s nchars)
  (string-copy/cursors s 0 nchars))

(define (string-drop s nchars)
  (string-copy/cursors s nchars))

(define (string-take-right s nchars)
  (string-copy/cursors s (- (string-length s) nchars)))

(define (string-drop-right s nchars)
  (string-copy/cursors s 0 (- (string-length s) nchars)))

(define string-pad
  (opt-lambda (s len (char #\space) (start 0) (end (string-length s)))
    (let ((original-length (string-cursor-diff s start end)))
      (cond ((= len original-length)
             s)
            ((< len original-length)
             (string-copy/cursors s (- original-length len)))
            (else
             (string-append (make-string (- len original-length) char) s))))))

(define string-pad-right
  (opt-lambda (s len (char #\space) (start 0) (end (string-length s)))
    (let ((original-length (string-cursor-diff s start end)))
      (cond ((= len original-length)
             s)
            ((< len original-length)
             (string-drop-right s (- original-length len)))
            (else
             (string-append s (make-string (- len original-length) char)))))))

(define (find-trimmed-string-start-byte-index s pred start end)
  (do ((result start (next-code-point-byte-index s result)))
      ((or (= result end)
           (not (pred (string-ref/byte-index s result))))
       result)))

(define (find-trimmed-string-end-byte-index s pred start end)
  (do ((result end (previous-code-point-byte-index s result)))
      ((or (= result start)
           (let ((before-result (previous-code-point-byte-index s result)))
             (not (pred (string-ref/byte-index s before-result)))))
       result)))

(define (string-trim* s pred start end left? right?)
  (let ((start* (->byte-index s start)) (end* (->byte-index s end)))
    (string-copy/byte-indexes s
                              (if left?
                                  (find-trimmed-string-start-byte-index s pred start* end*)
                                  start*)
                              (if right?
                                  (find-trimmed-string-end-byte-index s pred start* end*)
                                  end*))))

(define (make-string-trim-procedure left? right?)
  (opt-lambda (s (pred char-whitespace?) (start (string-cursor-start s)) (end (string-cursor-end s)))
    (string-trim* s pred start end left? right?)))

(define string-trim (make-string-trim-procedure #t #f))
(define string-trim-right (make-string-trim-procedure #f #t))
(define string-trim-both (make-string-trim-procedure #t #t))

(define string-prefix-length
  (opt-lambda (s1 s2
               (start1* (string-cursor-start s1)) (end1* (string-cursor-end s1))
               (start2* (string-cursor-start s2)) (end2* (string-cursor-end s2)))
    (let ((start1 (->byte-index s1 start1*)) (end1 (->byte-index s1 end1*))
          (start2 (->byte-index s2 start2*)) (end2 (->byte-index s2 end2*)))
      (do ((result 0 (+ result 1))
           (current1 start1 (next-code-point-byte-index s1 current1))
           (current2 start2 (next-code-point-byte-index s2 current2)))
          ((or (= current1 end1) (= current2 end2)
               (not (eq? (string-ref/byte-index s1 current1) (string-ref/byte-index s2 current2))))
           result)))))

(define string-suffix-length
  (opt-lambda (s1 s2
               (start1* (string-cursor-start s1)) (end1* (string-cursor-end s1))
               (start2* (string-cursor-start s2)) (end2* (string-cursor-end s2)))
    (let ((start1 (->byte-index s1 start1*)) (end1 (->byte-index s1 end1*))
          (start2 (->byte-index s2 start2*)) (end2 (->byte-index s2 end2*)))
      (do ((result 0 (+ result 1))
           (current1 end1 (previous-code-point-byte-index s1 current1))
           (current2 end2 (previous-code-point-byte-index s2 current2)))
          ((or (= current1 start1) (= current2 start2)
               (let ((c1 (previous-code-point-byte-index s1 current1))
                     (c2 (previous-code-point-byte-index s2 current2)))
                 (not (eq? (string-ref/byte-index s1 c1) (string-ref/byte-index s2 c2)))))
           result)))))

(define string-prefix?
  (opt-lambda (s1 s2
               (start1* (string-cursor-start s1)) (end1* (string-cursor-end s1))
               (start2* (string-cursor-start s2)) (end2* (string-cursor-end s2)))
    (let ((start1 (->byte-index s1 start1*)) (end1 (->byte-index s1 end1*))
          (start2 (->byte-index s2 start2*)) (end2 (->byte-index s2 end2*)))
      (let loop ((current1 start1) (current2 start2))
        (or (= current1 end1)
            (and (< current2 end2)
                 (eq? (string-ref/byte-index s1 current1) (string-ref/byte-index s2 current2))
                 (loop (next-code-point-byte-index s1 current1)
                       (next-code-point-byte-index s2 current2))))))))

(define string-suffix?
  (opt-lambda (s1 s2
               (start1* (string-cursor-start s1)) (end1* (string-cursor-end s1))
               (start2* (string-cursor-start s2)) (end2* (string-cursor-end s2)))
    (let ((start1 (->byte-index s1 start1*)) (end1 (->byte-index s1 end1*))
          (start2 (->byte-index s2 start2*)) (end2 (->byte-index s2 end2*)))
      (let loop ((current1 end1) (current2 end2))
        (or (= current1 start1)
            (and (> current2 start2)
                 (let ((c1 (previous-code-point-byte-index s1 current1))
                       (c2 (previous-code-point-byte-index s2 current2)))
                   (and (eq? (string-ref/byte-index s1 c1) (string-ref/byte-index s2 c2))
                        (loop c1 c2)))))))))

(define string-index
  (opt-lambda (s pred (start* (string-cursor-start s)) (end* (string-cursor-end s)))
    (let ((start (string-index->cursor s start*)) (end (string-index->cursor s end*)))
      (do ((current start (string-cursor-next s current)))
          ((or (= current end) (pred (string-ref/cursor s current)))
           current)))))

(define string-index-right
  (opt-lambda (s pred (start* (string-cursor-start s)) (end* (string-cursor-end s)))
    (let ((start (string-index->cursor s start*)) (end (string-index->cursor s end*)))
      (do ((current end (string-cursor-prev s current)))
          ((or (= current start) (pred (string-ref/cursor s (string-cursor-prev s current))))
           current)))))

(define string-skip
  (opt-lambda (s pred (start (string-cursor-start s)) (end (string-cursor-end s)))
    (string-index s (lambda (c) (not (pred c))) start end)))

(define string-skip-right
  (opt-lambda (s pred (start (string-cursor-start s)) (end (string-cursor-end s)))
    (string-index-right s (lambda (c) (not (pred c))) start end)))

(define string-contains
  (opt-lambda (haystack needle
               (haystack-start (string-cursor-start haystack))
               (haystack-end (string-cursor-end haystack))
               (needle-start (string-cursor-start needle))
               (needle-end (string-cursor-end needle)))
    (let ((result (byte-index->string-cursor
                   (string-contains/byte-indexes haystack needle
                                                 (->byte-index haystack haystack-start)
                                                 (->byte-index haystack haystack-end)
                                                 (->byte-index needle needle-start)
                                                 (->byte-index needle needle-end)))))
      (and (not (= result (string-index->cursor haystack haystack-end)))
           result))))

(define string-contains-right
  (opt-lambda (haystack needle
               (haystack-start (string-cursor-start haystack))
               (haystack-end (string-cursor-end haystack))
               (needle-start (string-cursor-start needle))
               (needle-end (string-cursor-end needle)))
    (let ((result (byte-index->string-cursor
                   (string-contains-right/byte-indexes haystack needle
                                                       (->byte-index haystack haystack-start)
                                                       (->byte-index haystack haystack-end)
                                                       (->byte-index needle needle-start)
                                                       (->byte-index needle needle-end)))))
      (and (not (= result (string-index->cursor haystack haystack-end)))
           result))))

(define string-reverse
  (opt-lambda (s (start (string-cursor-start s)) (end (string-cursor-end s)))
    (string-reverse* s (->byte-index s start) (->byte-index s end))))

(define (string-concatenate string-list)
  (string-join/infix string-list "" #f))

(define string-concatenate-reverse
  (opt-lambda (string-list (final-string "") (end (string-cursor-end final-string)))
    (let ((result (string)))
      (let loop ((elem string-list))
        (cond ((null? elem)
               #void)
              (else
               (loop (cdr elem))
               (string-append! result (car elem)))))
      (string-append result
                     (substring final-string
                                (string-cursor-start final-string)
                                (string-index->cursor final-string end))))))

(define (string-fold* kons knil s start end)
  (if (= start end)
      knil
      (string-fold kons
                   (kons (string-ref/byte-index s start) knil)
                   s
                   (next-code-point-byte-index s start)
                   end)))

(define string-fold
  (opt-lambda (kons knil s (start (string-cursor-start s)) (end (string-cursor-end s)))
    (string-fold* kons knil s (->byte-index s start) (->byte-index s end))))

(define (string-fold-right* kons knil s start end)
  (if (= start end)
      knil
      (let ((end-1 (previous-code-point-byte-index s end)))
        (string-fold-right* kons (kons (string-ref/byte-index s end-1) knil) s start end-1))))

(define string-fold-right
  (opt-lambda (kons knil s (start (string-cursor-start s)) (end (string-cursor-end s)))
    (string-fold-right* kons knil s (->byte-index s start) (->byte-index s end))))

(define string-for-each-cursor
  (opt-lambda (proc s (start* (string-cursor-start s)) (end* (string-cursor-end s)))
    (let ((start (string-index->cursor s start*)) (end (string-index->cursor s end*)))
      (do ((current start (string-cursor-next s current)))
          ((= current end))
        (proc current)))))

(define (string-for-each-1 proc string)
  (string-for-each-cursor (lambda (c) (proc (string-ref/cursor string c))) string))

(define (string-for-each-multi proc list-of-strings)
  (do ((current-byte-indexes
        (map (lambda (_) 0) list-of-strings)
        (map (lambda (string current-index) (next-code-point-byte-index string current-index))
             list-of-strings current-byte-indexes)))
      ((any (lambda (string current-index) (= (string-byte-length string) current-index))
            list-of-strings current-byte-indexes))
    (apply proc (map (lambda (string current-index) (string-ref/byte-index string current-index))
                     list-of-strings current-byte-indexes))))

(define (string-for-each proc string1 . strings-rest)
  (if (null? strings-rest)
      (string-for-each-1 proc string1)
      (string-for-each-multi proc (cons string1 strings-rest))))

(define (string-map proc string1 . strings-rest)
  (let ((result (string)))
    (apply string-for-each
           (lambda chars
             (string-append-char! result (apply proc chars)))
           string1 strings-rest)
    result))

(define string-replicate
  (opt-lambda (s from to (start (string-cursor-start s)) (end (string-cursor-end s)))
    (if (= from to)
        (string)
        (let* ((length (string-cursor-diff s start end))
               (count (- to from))
               (from-cursor (string-cursor-forward s start (floor-remainder from length)))
               (from-byte-index (->byte-index s from-cursor))
               (start-byte-index (->byte-index s start))
               (end-byte-index (->byte-index s end)))
          (do ((current from-byte-index
                        (let ((next (next-code-point-byte-index s current)))
                          (if (= next end-byte-index) start-byte-index next)))
               (count count (- count 1))
               (result (string)))
              ((= count 0) result)
            (string-append-char! result (string-ref/byte-index s current)))))))

(define string-count
  (opt-lambda (s pred (start (string-cursor-start s)) (end (string-cursor-end s)))
    (do ((current (->byte-index s start) (next-code-point-byte-index s current))
         (end-byte-index (->byte-index s end))
         (result 0 (+ result (if (pred (string-ref/byte-index s current)) 1 0))))
        ((= current end-byte-index) result))))

(define string-replace
  (opt-lambda (s1 s2 start1 end1 (start2 (string-cursor-start s2)) (end2 (string-cursor-end s2)))
    (string-append (substring/cursors s1 (string-cursor-start s1) start1)
                   (substring/cursors s2 start2 end2)
                   (substring/cursors s1 end1 (string-cursor-end s1)))))

(define (decrease-limit limit)
  (if limit (- limit 1) limit))

(define (make-final-split-element s start end allow-empty-final?)
  (if (and (= start end) (not allow-empty-final?))
      '()
      (list (string-copy/byte-indexes s start end))))

(define (string-split-infix s delimiter limit start end allow-empty-final?)
  (if (and limit (= limit 0))
      (make-final-split-element s start end allow-empty-final?)
      (let ((delimiter-byte-index (string-contains/byte-indexes s delimiter
                                                                start end
                                                                0 (string-byte-length delimiter))))
        (if (= delimiter-byte-index end)
            (make-final-split-element s start end allow-empty-final?)
            (cons (string-copy/byte-indexes s start delimiter-byte-index)
                  (string-split-infix s
                                      delimiter
                                      (decrease-limit limit)
                                      (+ delimiter-byte-index (string-length delimiter))
                                      end
                                      allow-empty-final?))))))

(define string-split
  (opt-lambda (s delimiter (grammar 'infix) (limit #f) (start (string-cursor-start s)) (end (string-cursor-end s)))
    (cond
     ((string-null? delimiter)
      (map string (string->list s start end)))
     (else
      (case grammar
        ((infix strict-infix)
         (cond ((and (string-null? s) (eq? grammar 'strict-infix))
                (error "Empty string"))
               ((string-null? s)
                '())
               (else
                (string-split-infix s delimiter limit (->byte-index s start) (->byte-index s end) #t))))
        ((prefix)
         (let ((result (string-split-infix s delimiter limit (->byte-index s start) (->byte-index s end) #t)))
           (if (and (pair? result) (string-null? (car result)))
               (cdr result)
               result)))
        ((suffix)
         (string-split-infix s delimiter limit (->byte-index s start) (->byte-index s end) #f))
        (else
         (error "Invalid grammar" grammar)))))))

(define string-filter
  (opt-lambda (pred s (start (string-cursor-start s)) (end (string-cursor-end s)))
    (do ((current (->byte-index s start) (next-code-point-byte-index s current))
         (end* (->byte-index s end))
         (result (string)))
        ((= current end*) result)
      (let ((c (string-ref/byte-index s current)))
        (when (pred c)
          (string-append-char! result c))))))

(define string-remove
  (opt-lambda (pred s (start (string-cursor-start s)) (end (string-cursor-end s)))
    (string-filter (lambda (c) (not (pred c))) s start end)))

(define (string-compare predicate first-string other-strings)
  (or (null? other-strings)
      (and (predicate first-string (car other-strings))
           (string-compare predicate (car other-strings) (cdr other-strings)))))

(define (make-string-comparator predicate)
  (lambda (string1 string2 . strings-rest)
    (string-compare predicate string1 (cons string2 strings-rest))))

(define string=? (make-string-comparator string=?/pair))
(define string<? (make-string-comparator string<?/pair))
(define string<=? (make-string-comparator string<=?/pair))
(define string>? (make-string-comparator string>?/pair))
(define string>=? (make-string-comparator string>=?/pair))

(define (make-string-comparator-ci predicate)
  (lambda (string1 string2 . strings-rest)
    (string-compare
     predicate
     (string-foldcase string1)
     (cons (string-foldcase string2) (map string-foldcase strings-rest)))))

(define string-ci=? (make-string-comparator-ci string=?/pair))
(define string-ci<? (make-string-comparator-ci string<?/pair))
(define string-ci<=? (make-string-comparator-ci string<=?/pair))
(define string-ci>? (make-string-comparator-ci string>?/pair))
(define string-ci>=? (make-string-comparator-ci string>=?/pair))

(define string->utf8
  (opt-lambda (s (start (string-cursor-start s)) (end (string-cursor-end s)))
    (string->utf8/byte-indexes s (->byte-index s start) (->byte-index s end))))

(define utf8->string
  (opt-lambda (bv (start 0) (end (bytevector-length bv)))
    (utf8->string* bv start end)))

(define vector->string
  (opt-lambda (v (start 0) (end (vector-length v)))
    (do ((result (string))
         (current start (+ current 1)))
        ((= current end) result)
      (string-append-char! result (vector-ref v current)))))
