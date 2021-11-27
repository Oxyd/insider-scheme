(library (insider list))
(import (insider syntax) (insider basic-procedures)
        (except (insider internal) let define))
(export
 ;; From core
 cons car cdr cadr caddr cadddr cddr cdddr set-car! set-cdr! append

 ;; Defined here: R7RS procedures:
 null? pair? caar assoc assq assv member memq memv length reverse map for-each

 ;; SRFI-1:
 any every filter)

(define (caar x)
  (car (car x)))

(define (assoc obj alist . compare*)
  (let ((compare (if (null? compare*)
                     equal?
                     (car compare*))))
    (let loop ((elem alist))
      (cond ((null? elem)
             #f)
            ((compare obj (caar elem))
             (car elem))
            (else
             (loop (cdr elem)))))))

(define (assq obj alist)
  (assoc obj alist eq?))

(define (assv obj alist)
  (assoc obj alist eqv?))

(define (member obj list . compare*)
  (let ((compare (if (null? compare*)
                     equal?
                     (car compare*))))
    (let loop ((elem list))
      (cond ((null? elem)
             #f)
            ((compare (car elem) obj)
             elem)
            (else
             (loop (cdr elem)))))))

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
         (for-each-1 proc (map-1 cdr lists)))))

(define (for-each proc list1 . lists)
  (cond ((null? lists)
         (for-each-1 proc list1))
        (else
         (for-each-multi proc (cons list1 lists)))))

(define (filter pred list)
  (cond ((null? list)
         '())
        ((pred (car list))
         (cons pred (filter pred (cdr list))))
        (else
         (filter pred (cdr list)))))
