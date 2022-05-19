(library (insider include))
(import (insider io)
        (insider syntax)
        (insider error)
        (insider control)
        (insider string)
        (insider basic-procedures)
        (insider list)
        (insider numeric)
        (insider vector))
(export include include-ci)

(meta
  (define (read-source-file reader name)
    (let* ((name* (syntax->datum name))
           (port (open-source-file-relative (current-source-file-origin) name*)))
      (if port
          (call-with-port port reader)
          (error (string-append "Can't open " name* " for inclusion"))))))

(meta
  (define (reader name)
    (read-source-file read-syntax-multiple name)))

(meta
  (define (reader-ci name)
    (read-source-file read-syntax-multiple-ci name)))

(meta
  (define (add-scope x scope)
    (cond ((pair? x)
           (cons (add-scope (car x) scope) (add-scope (cdr x) scope)))
          ((vector? x)
           (vector-map (lambda (elem) (add-scope elem scope)) x))
          ((syntax? x)
           (syntax-add-scope x scope))
          (else
           x))))

(meta
  (define (add-scopes x scopes)
    (if (null? scopes)
        x
        (add-scopes (add-scope x (car scopes)) (cdr scopes)))))

(meta
  (define (do-include stx reader)
    (syntax-match stx ()
      ((_ file-names ...)
       (let ((scopes (syntax-scopes stx))
             (expressions (apply append (map reader file-names))))
         #`(begin
             #,@(map (lambda (stx)
                       (add-scopes stx scopes))
                     expressions)))))))

(define-syntax include
  (lambda (stx)
    (do-include stx reader)))

(define-syntax include-ci
  (lambda (stx)
    (do-include stx reader-ci)))

;; Local variables:
;; eval: (put 'syntax-match 'scheme-indent-function 2)
;; End:
