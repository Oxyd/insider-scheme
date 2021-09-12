(library (insider cond-expand))
(import (insider base-scheme) (insider syntax))
(export cond-expand)

(begin-for-syntax
 (define (condition-matches? condition)
   (syntax-match condition (else and or library)
     (else
      #t)
     ((and elements ...)
      (all condition-matches? elements))
     ((or elements ...)
      (any condition-matches? elements))
     ((library name)
      (known-module? name))
     (feature
      (memq (syntax->datum feature) (features)))
     (_
      (error "Invalid cond-expand condition")))))

(define-syntax cond-expand
  (lambda (stx)
    (syntax-match stx ()
      ((_ (conditions expressions ...) ...)
       (let loop ((c conditions) (es expressions))
         (cond ((null? c)
                #'#void)
               ((condition-matches? (car c))
                #`(begin #,@(car es)))
               (else
                (loop (cdr c) (cdr es)))))))))

;; Local variables:
;; eval: (put 'syntax-match 'scheme-indent-function 2)
;; End:
