(define-type-predicate pair? insider::pair)

(define (null? x)
  (eq? x '()))

(define (caar x)
  (car (car x)))

(define (cdar x)
  (cdr (car x)))

(define (caaar x) (car (car (car x))))
(define (caadr x) (car (car (cdr x))))
(define (cadar x) (car (cdr (car x))))
(define (cdaar x) (cdr (car (car x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cddar x) (cdr (cdr (car x))))
(define (caaaar x) (car (car (car (car x)))))
(define (caaadr x) (car (car (car (cdr x)))))
(define (caadar x) (car (car (cdr (car x)))))
(define (caaddr x) (car (car (cdr (cdr x)))))
(define (cadaar x) (car (cdr (car (car x)))))
(define (cadadr x) (car (cdr (car (cdr x)))))
(define (caddar x) (car (cdr (cdr (car x)))))
(define (cdaaar x) (cdr (car (car (car x)))))
(define (cdaadr x) (cdr (car (car (cdr x)))))
(define (cdadar x) (cdr (car (cdr (car x)))))
(define (cdaddr x) (cdr (car (cdr (cdr x)))))
(define (cddaar x) (cdr (cdr (car (car x)))))
(define (cddadr x) (cdr (cdr (car (cdr x)))))
(define (cdddar x) (cdr (cdr (cdr (car x)))))
(define (cddddr x) (cdr (cdr (cdr (cdr x)))))

(define (assoc obj alist (compare equal?))
  (let loop ((elem alist))
    (cond ((null? elem)
           #f)
          ((compare obj (caar elem))
           (car elem))
          (else
           (loop (cdr elem))))))

(define (assq obj alist)
  (assoc obj alist eq?))

(define (assv obj alist)
  (assoc obj alist eqv?))

(define (member obj list (compare equal?))
  (let loop ((elem list))
    (cond ((null? elem)
           #f)
          ((compare (car elem) obj)
           elem)
          (else
           (loop (cdr elem))))))

(define (memq obj list)
  (member obj list eq?))

(define (memv obj list)
  (member obj list eqv?))

(define (length lst)
  (do ((elem lst (cdr elem))
       (result 0 (+ result 1)))
      ((null? elem) result)))

(define (reverse lst)
  (let loop ((lst lst) (accum '()))
    (if (null? lst)
        accum
        (loop (cdr lst) (cons (car lst) accum)))))

(define (map-1 proc list)
  (cond ((null? list)
         '())
        (else
         (cons (proc (car list))
               (map-1 proc (cdr list))))))

(define (map-multi proc lists)
  (cond ((any-1 null? lists)
         '())
        (else
         (cons (apply proc (map-1 car lists))
               (map-multi proc (map-1 cdr lists))))))

;;> Apply @c{proc} to the elements of the input @c{list}s and return a list of
;;> the results. It is an error if @c{proc} does not take as many arguments as
;;> there are lists.
;;>
;;> When multiple lists are specified, @c{map} terminates as soon as the
;;> shortest list runs out. It is an error if all the lists are infinite. It is
;;> also an error for @c{proc} to mutate any of the lists.
(define (map proc list1 . lists)
  (cond ((null? lists)
         (map-1 proc list1))
        (else
         (map-multi proc (cons list1 lists)))))

(define (any-1 pred list)
  (cond ((null? list)
         #f)
        ((pred (car list))
         => values)
        (else
         (any-1 pred (cdr list)))))

(define (any-multi pred lists)
  (cond ((any-1 null? lists)
         #f)
        ((apply pred (map-1 car lists))
         => values)
        (else
         (any-multi pred (map-1 cdr lists)))))

(define (any proc list1 . lists)
  (cond ((null? lists)
         (any-1 proc list1))
        (else
         (any-multi proc (cons list1 lists)))))

(define (every-1 pred list)
  (if (null? list)
      #t
      (let loop ((e list))
        (cond ((null? (cdr e))
               (pred (car e)))
              ((pred (car e))
               (loop (cdr e)))
              (else
               #f)))))

(define (every-multi pred lists)
  (if (any-1 null? lists)
      #t
      (let loop ((ls lists))
        (let ((current-elems (map-1 car ls))
              (next-elems (map-1 cdr ls)))
          (cond ((any-1 null? next-elems)
                 (apply pred current-elems))
                ((apply pred current-elems)
                 (loop next-elems))
                (else
                 #f))))))

(define (every proc list1 . lists)
  (cond ((null? lists)
         (every-1 proc list1))
        (else
         (every-multi proc (cons list1 lists)))))

(define (for-each-1 proc list)
  (cond ((null? list)
         #void)
        (else
         (proc (car list))
         (for-each-1 proc (cdr list)))))

(define (for-each-multi proc lists)
  (cond ((any-1 null? lists)
         #void)
        (else
         (apply proc (map-1 car lists))
         (for-each-multi proc (map-1 cdr lists)))))

(define (for-each proc list1 . lists)
  (cond ((null? lists)
         (for-each-1 proc list1))
        (else
         (for-each-multi proc (cons list1 lists)))))

(define (filter pred list)
  (cond ((null? list)
         '())
        ((pred (car list))
         (cons (car list) (filter pred (cdr list))))
        (else
         (filter pred (cdr list)))))

(define (list? x)
  (if (not (pair? x))
      (null? x)
      (let loop ((slow x) (fast (cdr x)))
        (if (not (pair? fast))
            (null? fast)
            (and (not (eq? slow fast))
                 (loop (cdr slow)
                       (let ((fast* (cdr fast)))
                         (if (pair? fast*)
                             (cdr fast*)
                             fast*))))))))

(define (make-list k (fill #void))
  (do ((k k (- k 1))
       (result '() (cons fill result)))
      ((= k 0) result)))

(define (list-tail l k)
  (if (= k 0)
      l
      (list-tail (cdr l) (- k 1))))

(define (list-ref l k)
  (car (list-tail l k)))

(define (list-set! l k x)
  (set-car! (list-tail l k) x))

(define (list-copy x)
  (if (pair? x)
      (cons (car x) (list-copy (cdr x)))
      x))

(define (fold-1 kons knil list)
  (if (null? list)
      knil
      (fold-1 kons (kons (car list) knil) (cdr list))))

(define (fold-multi kons knil lists)
  (if (any-1 null? lists)
      knil
      (fold-multi kons
                  (apply kons (append (map-1 car lists) (list knil)))
                  (map-1 cdr lists))))

(define (fold kons knil list1 . lists-rest)
  (if (null? lists-rest)
      (fold-1 kons knil list1)
      (fold-multi kons knil (cons list1 lists-rest))))
