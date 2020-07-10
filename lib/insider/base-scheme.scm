(library (insider base-scheme))
(import
 (rename (insider internal)
         (define %define)))
(export sc-macro-transformer rsc-macro-transformer capture-syntactic-environment define
        let set! lambda if box unbox box-set! define-syntax begin
        quote quasiquote unquote unquote-splicing expand-quote
        + - * / = < > gcd
        write-simple newline append list->vector vector-append cons car cdr cadr caddr cadddr cddr cdddr
        make-syntactic-closure type eq?)

(%define pair?
  (lambda (x)
    (eq? (type x) 'insider::pair)))

(%define close-syntax
  (lambda (form env)
    (make-syntactic-closure env '() form)))

(define-syntax sc-macro-transformer
  (lambda (form transformer-env usage-env)
    (let ((transformer (make-syntactic-closure usage-env '() (cadr form))))
      (make-syntactic-closure transformer-env '()
                              `(lambda (form* transformer-env* usage-env*)
                                 (make-syntactic-closure transformer-env* '()
                                                         (,transformer form* usage-env*)))))))

(define-syntax rsc-macro-transformer
  (lambda (form transformer-env usage-env)
    (let ((transformer (make-syntactic-closure usage-env '() (cadr form))))
      (make-syntactic-closure transformer-env '()
                              `(lambda (form* transformer-env* usage-env*)
                                 (,transformer form* transformer-env*))))))

(define-syntax capture-syntactic-environment
  (lambda (form transformer-env usage-env)
    (let ((f (cadr form)))
      `(,f ,usage-env))))

(define-syntax define
  (rsc-macro-transformer
   (lambda (form env)
     (let ((name-form (cadr form))
           (body-forms (cddr form))
           ($define (close-syntax 'define env))
           ($%define (close-syntax '%define env))
           ($lambda (close-syntax 'lambda env)))
       (if (pair? name-form)
           (let ((name (car name-form))
                 (params (cdr name-form)))
             `(,$define ,name
                (,$lambda ,params
                  ,@body-forms)))
           `(,$%define ,name-form ,@body-forms))))))
