(define-type-predicate string? insider::string)
(define-type-predicate string-cursor? insider::string_cursor)

(define (string-cursor-next s cursor)
  (if (string-cursor? cursor)
      (string-cursor-next* s cursor)
      (+ cursor 1)))

(define (string-cursor-prev s cursor)
  (if (string-cursor? cursor)
      (string-cursor-prev* s cursor)
      (- cursor 1)))

(define (string-cursor-forward s cursor nchars)
  (if (string-cursor? cursor)
      (do ((i 0 (+ i 1))
           (result cursor (string-cursor-next* s result)))
          ((= i nchars) result))
      (+ cursor nchars)))

(define (string-cursor-back s cursor nchars)
  (if (string-cursor? cursor)
      (do ((i nchars (- i 1))
           (result cursor (string-cursor-prev* s result)))
          ((= i 0) result))
      (- cursor nchars)))

(define string-ref/cursor string-ref)

(define (string-cursor<=? c1 c2)
  (or (string-cursor=? c1 c2) (string-cursor<? c1 c2)))

(define (string-cursor>? c1 c2)
  (not (string-cursor<=? c1 c2)))

(define (string-cursor>=? c1 c2)
  (not (string-cursor<? c1 c2)))

(define (string-cursor-diff s start end)
  (if (and (exact-integer? start) (exact-integer? end))
      (- end start)
      (string-cursor-diff* s
                           (string-index->cursor s start)
                           (string-index->cursor s end))))

(define (string-index->cursor s index)
  (if (string-cursor? index)
      index
      (string-cursor-forward s (string-cursor-start s) index)))

(define (string-every pred s
                      (start* (string-cursor-start s))
                      (end* (string-cursor-end s)))
  (let ((start (string-index->cursor s start*))
        (end (string-index->cursor s end*)))
    (let loop ((current start) (last-value #t))
      (if (eq? current end)
          last-value
          (let ((current-value (pred (string-ref/cursor s current))))
            (if current-value
                (loop (string-cursor-next s current) current-value)
                #f))))))

(define (string-any pred s
                    (start* (string-cursor-start s))
                    (end* (string-cursor-end s)))
  (let ((start (string-index->cursor s start*))
        (end (string-index->cursor s end*)))
    (let loop ((current start))
      (if (eq? current end)
          #f
          (let ((value (pred (string-ref/cursor s current))))
            (if value
                value
                (loop (string-cursor-next s current))))))))

(define (string-tabulate proc len)
  (vector->string (vector-tabulate proc len)))

(define (string-append/share x y)
  (if (string-null? y)
      x
      (string-append x y)))

(define (string-unfold stop? mapper successor initial-seed
                       (base (string))
                       (make-final (lambda (_) "")))
  (do ((result (string-copy base))
       (seed initial-seed (successor seed)))
      ((stop? seed) (string-append/share result (make-final seed)))
    (string-append-char! result (mapper seed))))

(define (string-unfold-right . args)
  (string-reverse (apply string-unfold args)))

(define (string->list/cursors s
                              (start (string-cursor-start s))
                              (end (string-cursor-end s)))
  (do ((index (string-index->cursor s start) (string-cursor-next* s index))
       (accum '() (cons (string-ref s index) accum)))
      ((string-cursor=? index end) (reverse accum))))

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

(define (string->vector/cursors s
                                (start (string-cursor-start s))
                                (end (string-cursor-end s)))
  (do ((result (make-vector (string-cursor-diff s start end)))
       (vector-index 0 (+ vector-index 1))
       (string-index (string-index->cursor s start)
                     (string-cursor-next* s string-index)))
      ((string-cursor=? string-index end) result)
    (vector-set! result vector-index (string-ref s string-index))))

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
      (string-append (car string-list)
                     (string-join/prefix (cdr string-list) delimiter))))

(define (string-join string-list (delimiter "") (grammar 'infix))
  (case grammar
    ((infix strict-infix)
     (string-join/infix string-list delimiter (eq? grammar 'strict-infix)))
    ((prefix)
     (string-join/prefix string-list delimiter))
    ((suffix)
     (string-join/suffix string-list delimiter))
    (else
     (error "Invalid grammar" grammar))))

(define (string-copy/cursors s
                             (start (string-cursor-start s))
                             (end (string-cursor-end s)))
  (string-copy* s start end))

(define string-copy string-copy/cursors)

(define (entire-string? s start end)
  (and (string-cursor=? start (string-cursor-start s))
       (string-cursor=? end (string-cursor-end s))))

(define (substring/cursors s start end)
  (if (entire-string? s start end)
      s
      (string-copy/cursors s start end)))

(define (substring s start end)
  (string-copy/cursors s start end))

(define (string-copy! to at from
                      (start (string-cursor-start from))
                      (end (string-cursor-end from)))
  (string-copy!* to at from start end))

(define (string-fill! s fill
                      (start (string-cursor-start s))
                      (end (string-cursor-end s)))
  (let ((length (string-cursor-diff s start end)))
    (do ((current (string-index->cursor s start)
                  (string-cursor-next* s current))
         (count length (- count 1)))
        ((= count 0))
      (string-set! s current fill))))

(define (string-take s nchars)
  (string-copy/cursors s 0 nchars))

(define (string-drop s nchars)
  (string-copy/cursors s nchars))

(define (string-take-right s nchars)
  (string-copy/cursors s (- (string-length s) nchars)))

(define (string-drop-right s nchars)
  (string-copy/cursors s 0 (- (string-length s) nchars)))

(define (string-pad s len (char #\space) (start 0) (end (string-length s)))
  (let ((original-length (string-cursor-diff s start end)))
    (cond ((= len original-length)
           s)
          ((< len original-length)
           (string-copy/cursors s (- original-length len)))
          (else
           (string-append (make-string (- len original-length) char) s)))))

(define (string-pad-right s len (char #\space) (start 0) (end (string-length s)))
  (let ((original-length (string-cursor-diff s start end)))
    (cond ((= len original-length)
           s)
          ((< len original-length)
           (string-drop-right s (- original-length len)))
          (else
           (string-append s (make-string (- len original-length) char))))))

(define (find-trimmed-string-start-cursor s pred start end)
  (do ((result start (string-cursor-next* s result)))
      ((or (string-cursor=? result end) (not (pred (string-ref s result))))
       result)))

(define (find-trimmed-string-end-cursor s pred start end)
  (do ((result end (string-cursor-prev* s result)))
      ((or (string-cursor=? result start)
           (let ((before-result (string-cursor-prev* s result)))
             (not (pred (string-ref s before-result)))))
       result)))

(define (string-trim* s pred start end left? right?)
  (let ((start* (string-index->cursor s start))
        (end* (string-index->cursor s end)))
    (string-copy
     s
     (if left?
         (find-trimmed-string-start-cursor s pred start* end*)
         start*)
     (if right?
         (find-trimmed-string-end-cursor s pred start* end*)
         end*))))

(define (make-string-trim-procedure left? right?)
  (lambda (s
           (pred char-whitespace?)
           (start (string-cursor-start s))
           (end (string-cursor-end s)))
    (string-trim* s pred start end left? right?)))

(define string-trim (make-string-trim-procedure #t #f))
(define string-trim-right (make-string-trim-procedure #f #t))
(define string-trim-both (make-string-trim-procedure #t #t))

(define (string-prefix-length s1 s2
                              (start1* (string-cursor-start s1))
                              (end1* (string-cursor-end s1))
                              (start2* (string-cursor-start s2))
                              (end2* (string-cursor-end s2)))
  (let ((start1 (string-index->cursor s1 start1*))
        (end1 (string-index->cursor s1 end1*))
        (start2 (string-index->cursor s2 start2*))
        (end2 (string-index->cursor s2 end2*)))
    (do ((result 0 (+ result 1))
         (current1 start1 (string-cursor-next* s1 current1))
         (current2 start2 (string-cursor-next* s2 current2)))
        ((or (eq? current1 end1) (eq? current2 end2)
             (not (eq? (string-ref s1 current1) (string-ref s2 current2))))
         result))))

(define (string-suffix-length s1 s2
                              (start1* (string-cursor-start s1))
                              (end1* (string-cursor-end s1))
                              (start2* (string-cursor-start s2))
                              (end2* (string-cursor-end s2)))
  (let ((start1 (string-index->cursor s1 start1*))
        (end1 (string-index->cursor s1 end1*))
        (start2 (string-index->cursor s2 start2*))
        (end2 (string-index->cursor s2 end2*)))
    (do ((result 0 (+ result 1))
         (current1 end1 (string-cursor-prev* s1 current1))
         (current2 end2 (string-cursor-prev* s2 current2)))
        ((or (eq? current1 start1) (eq? current2 start2)
             (let ((c1 (string-cursor-prev* s1 current1))
                   (c2 (string-cursor-prev* s2 current2)))
               (not (eq? (string-ref s1 c1)
                         (string-ref s2 c2)))))
         result))))

(define (string-prefix? s1 s2
                        (start1* (string-cursor-start s1))
                        (end1* (string-cursor-end s1))
                        (start2* (string-cursor-start s2))
                        (end2* (string-cursor-end s2)))
  (let ((start1 (string-index->cursor s1 start1*))
        (end1 (string-index->cursor s1 end1*))
        (start2 (string-index->cursor s2 start2*))
        (end2 (string-index->cursor s2 end2*)))
    (let loop ((current1 start1) (current2 start2))
      (or (eq? current1 end1)
          (and (string-cursor<? current2 end2)
               (eq? (string-ref s1 current1)
                    (string-ref s2 current2))
               (loop (string-cursor-next* s1 current1)
                     (string-cursor-next* s2 current2)))))))

(define (string-suffix? s1 s2
                        (start1* (string-cursor-start s1))
                        (end1* (string-cursor-end s1))
                        (start2* (string-cursor-start s2))
                        (end2* (string-cursor-end s2)))
  (let ((start1 (string-index->cursor s1 start1*))
        (end1 (string-index->cursor s1 end1*))
        (start2 (string-index->cursor s2 start2*))
        (end2 (string-index->cursor s2 end2*)))
    (let loop ((current1 end1) (current2 end2))
      (or (eq? current1 start1)
          (and (string-cursor>? current2 start2)
               (let ((c1 (string-cursor-prev* s1 current1))
                     (c2 (string-cursor-prev* s2 current2)))
                 (and (eq? (string-ref s1 c1) (string-ref s2 c2))
                      (loop c1 c2))))))))

(define (string-index s pred
                      (start* (string-cursor-start s))
                      (end* (string-cursor-end s)))
  (let ((start (string-index->cursor s start*))
        (end (string-index->cursor s end*)))
    (do ((current start (string-cursor-next s current)))
        ((or (eq? current end) (pred (string-ref/cursor s current)))
         current))))

(define (string-index-right s pred
                            (start* (string-cursor-start s))
                            (end* (string-cursor-end s)))
  (let ((start (string-index->cursor s start*))
        (end (string-index->cursor s end*)))
    (do ((current end (string-cursor-prev s current)))
        ((or (eq? current start)
             (pred (string-ref/cursor s (string-cursor-prev s current))))
         current))))

(define (string-skip s pred
                     (start (string-cursor-start s))
                     (end (string-cursor-end s)))
  (string-index s (lambda (c) (not (pred c))) start end))

(define (string-skip-right s pred
                           (start (string-cursor-start s))
                           (end (string-cursor-end s)))
  (string-index-right s (lambda (c) (not (pred c))) start end))

(define (string-contains haystack needle
                         (haystack-start (string-cursor-start haystack))
                         (haystack-end (string-cursor-end haystack))
                         (needle-start (string-cursor-start needle))
                         (needle-end (string-cursor-end needle)))
  (let ((result (string-contains* haystack needle
                                  (string-index->cursor haystack haystack-start)
                                  (string-index->cursor haystack haystack-end)
                                  (string-index->cursor needle needle-start)
                                  (string-index->cursor needle needle-end))))
    (and (not (string-cursor=? result haystack-end))
         result)))

(define (string-contains-right haystack needle
                               (haystack-start (string-cursor-start haystack))
                               (haystack-end (string-cursor-end haystack))
                               (needle-start (string-cursor-start needle))
                               (needle-end (string-cursor-end needle)))
  (let ((result (string-contains-right*
                 haystack needle
                 (string-index->cursor haystack haystack-start)
                 (string-index->cursor haystack haystack-end)
                 (string-index->cursor needle needle-start)
                 (string-index->cursor needle needle-end))))
    (and (not (string-cursor=? result haystack-end))
         result)))

(define (string-reverse s
                        (start (string-cursor-start s))
                        (end (string-cursor-end s)))
  (string-reverse* s start end))

(define (string-concatenate string-list)
  (string-join/infix string-list "" #f))

(define (string-concatenate-reverse string-list
                                    (final-string "")
                                    (end (string-cursor-end final-string)))
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
                              (string-index->cursor final-string end)))))

(define (string-fold* kons knil s start end)
  (if (eq? start end)
      knil
      (string-fold kons
                   (kons (string-ref s start) knil)
                   s
                   (string-cursor-next* s start)
                   end)))

(define (string-fold kons knil s
                     (start (string-cursor-start s))
                     (end (string-cursor-end s)))
  (string-fold* kons knil s
                (string-index->cursor s start) (string-index->cursor s end)))

(define (string-fold-right* kons knil s start end)
  (if (eq? start end)
      knil
      (let ((end-1 (string-cursor-prev* s end)))
        (string-fold-right* kons
                            (kons (string-ref s end-1) knil)
                            s
                            start
                            end-1))))

(define (string-fold-right kons knil s
                           (start (string-cursor-start s))
                           (end (string-cursor-end s)))
  (string-fold-right* kons knil s
                      (string-index->cursor s start)
                      (string-index->cursor s end)))

(define (string-for-each-cursor proc s
                                (start* (string-cursor-start s))
                                (end* (string-cursor-end s)))
  (let ((start (string-index->cursor s start*))
        (end (string-index->cursor s end*)))
    (do ((current start (string-cursor-next s current)))
        ((eq? current end))
      (proc current))))

(define (string-for-each-1 proc string)
  (string-for-each-cursor
   (lambda (c) (proc (string-ref/cursor string c)))
   string))

(define (string-for-each-multi proc list-of-strings)
  (do ((current-cursors
        (map (lambda (string) (string-cursor-start string)) list-of-strings)
        (map (lambda (string current-index)
               (string-cursor-next* string current-index))
             list-of-strings current-cursors)))
      ((any (lambda (string current-index)
              (eq? current-index (string-cursor-end string)))
            list-of-strings current-cursors))
    (apply proc (map (lambda (string current-index)
                       (string-ref string current-index))
                     list-of-strings current-cursors))))

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

(define (string-replicate s from to
                          (start (string-cursor-start s))
                          (end (string-cursor-end s)))
  (if (string-cursor=? from to)
      (string)
      (let* ((length (string-cursor-diff s start end))
             (count (- to from))
             (from-cursor (string-cursor-forward s
                                                 start
                                                 (floor-remainder from length)))
             (from-cursor (string-index->cursor s from-cursor))
             (start-cursor (string-index->cursor s start))
             (end-cursor (string-index->cursor s end)))
        (do ((current from-cursor
                      (let ((next (string-cursor-next* s current)))
                        (if (eq? next end-cursor) start-cursor next)))
             (count count (- count 1))
             (result (string)))
            ((= count 0) result)
          (string-append-char! result (string-ref s current))))))

(define (string-count s pred
                      (start (string-cursor-start s))
                      (end (string-cursor-end s)))
  (do ((current (string-index->cursor s start)
                (string-cursor-next* s current))
       (end-cursor (string-index->cursor s end))
       (result 0 (+ result (if (pred (string-ref s current)) 1 0))))
      ((eq? current end-cursor) result)))

(define (string-replace s1 s2 start1 end1
                        (start2 (string-cursor-start s2))
                        (end2 (string-cursor-end s2)))
  (string-append (substring/cursors s1 (string-cursor-start s1) start1)
                 (substring/cursors s2 start2 end2)
                 (substring/cursors s1 end1 (string-cursor-end s1))))

(define (decrease-limit limit)
  (if limit (- limit 1) limit))

(define (make-final-split-element s start end allow-empty-final?)
  (if (and (string-cursor=? start end) (not allow-empty-final?))
      '()
      (list (string-copy s start end))))

(define (string-split-infix s delimiter limit start end allow-empty-final?)
  (if (and limit (= limit 0))
      (make-final-split-element s start end allow-empty-final?)
      (let ((delimiter-cursor (string-contains s delimiter start end)))
        (if (not delimiter-cursor)
            (make-final-split-element s start end allow-empty-final?)
            (cons (string-copy s start delimiter-cursor)
                  (string-split-infix
                   s
                   delimiter
                   (decrease-limit limit)
                   (string-cursor-forward s
                                          delimiter-cursor
                                          (string-length delimiter))
                   end
                   allow-empty-final?))))))

(define (string-split s delimiter (grammar 'infix) (limit #f)
                      (start (string-cursor-start s))
                      (end (string-cursor-end s)))
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
              (string-split-infix s
                                  delimiter
                                  limit
                                  (string-index->cursor s start)
                                  (string-index->cursor s end)
                                  #t))))
      ((prefix)
       (let ((result (string-split-infix s
                                         delimiter
                                         limit
                                         (string-index->cursor s start)
                                         (string-index->cursor s end)
                                         #t)))
         (if (and (pair? result) (string-null? (car result)))
             (cdr result)
             result)))
      ((suffix)
       (string-split-infix s
                           delimiter
                           limit
                           (string-index->cursor s start)
                           (string-index->cursor s end)
                           #f))
      (else
       (error "Invalid grammar" grammar))))))

(define (string-filter pred s
                       (start (string-cursor-start s))
                       (end (string-cursor-end s)))
  (do ((current (string-index->cursor s start)
                (string-cursor-next* s current))
       (end* (string-index->cursor s end))
       (result (string)))
      ((eq? current end*) result)
    (let ((c (string-ref s current)))
      (when (pred c)
        (string-append-char! result c)))))

(define (string-remove pred s
                       (start (string-cursor-start s))
                       (end (string-cursor-end s)))
  (string-filter (lambda (c) (not (pred c))) s start end))

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

(define (string->utf8 s
                      (start (string-cursor-start s))
                      (end (string-cursor-end s)))
  (string->utf8* s (string-index->cursor s start) (string-index->cursor s end)))

(define (utf8->string bv (start 0) (end (bytevector-length bv)))
  (utf8->string* bv start end))

(define (vector->string v (start 0) (end (vector-length v)))
  (do ((result (string))
       (current start (+ current 1)))
      ((= current end) result)
    (string-append-char! result (vector-ref v current))))
