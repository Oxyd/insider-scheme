(library (insider include))
(import (insider base-scheme) (insider file) (insider syntax) (insider error) (insider control))
(export include include-ci)

(begin-for-syntax
 (define (read-source-file reader name)
   (let* ((name* (syntax->datum name))
          (port (open-source-file-relative (current-source-file-origin) name*)))
     (if port
         (call-with-port port reader)
         (error (string-append "Can't open " name* " for inclusion")))))

 (define (reader name)
   (read-source-file read-syntax-multiple name))

 (define (reader-ci name)
   (read-source-file read-syntax-multiple-ci name))

 (define (add-scope! x scope)
   (cond ((pair? x)
          (add-scope! (car x) scope)
          (add-scope! (cdr x) scope))
         ((vector? x)
          (do ((i 0 (+ i 1)))
              ((= i (vector-length x)))
            (add-scope! (vector-ref x i) scope)))
         ((syntax? x)
          (syntax-add-scope! x scope)
          (add-scope! (syntax-expression x) scope))
         (else
          #void)))

 (define (add-scopes! stx scopes)
   (cond ((null? scopes)
          #void)
         (else
          (add-scope! stx (car scopes))
          (add-scopes! stx (cdr scopes)))))

 (define (do-include stx reader)
   (syntax-match stx ()
     ((_ file-names ...)
      (let ((scopes (syntax-scopes stx))
            (expressions (apply append (map reader file-names))))
        (for-each (lambda (stx)
                    (add-scopes! stx scopes))
                  expressions)
        #`(begin #,@expressions))))))

(define-syntax include
  (lambda (stx)
    (do-include stx reader)))

(define-syntax include-ci
  (lambda (stx)
    (do-include stx reader-ci)))

;; Local variables:
;; eval: (put 'syntax-match 'scheme-indent-function 2)
;; End:
