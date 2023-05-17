(define (full-expand stx . max-depth)
  (define (do-expand x depth max-depth)
    (cond ((and max-depth (>= depth max-depth))
           x)
          ((pair? x)
           (cons (do-expand (car x) (+ depth 1) max-depth)
                 (do-expand (cdr x) depth max-depth)))
          ((syntax? x)
           (let* ((stx (expand x))
                  (e (syntax-expression stx)))
             (datum->syntax stx (do-expand e (+ depth 1) max-depth))))
          (else
           x)))

  (do-expand stx 0 (if (null? max-depth)
                       #f
                       (car max-depth))))
