(library (insider base-scheme))
(import
 (rename (insider internal)
         (define %define)
         (let %let)))

(export sc-macro-transformer rsc-macro-transformer capture-syntactic-environment define
        let set! lambda if box unbox box-set! define-syntax begin begin-for-syntax
        quote quasiquote unquote unquote-splicing expand-quote
        + - * / = < > gcd
        write-simple newline append list->vector vector-append cons car cdr cadr caddr cadddr cddr cdddr
        make-syntactic-closure type eq? pair? symbol? when)

(begin-for-syntax
 (%define pair?
   (lambda (x)
     (eq? (type x) 'insider::pair)))

 (%define symbol?
   (lambda (x)
     (eq? (type x) 'insider::symbol)))

 (%define close-syntax
   (lambda (form env)
     (make-syntactic-closure env '() form)))

 (%define map
   (lambda (f list)
     (%define go
       (lambda (p)
         (if (eq? p '())
             '()
             (cons (f (car p)) (go (cdr p))))))
     (go list))))

(define-syntax sc-macro-transformer
  (lambda (form transformer-env usage-env)
    (%let ((transformer (make-syntactic-closure usage-env '() (cadr form))))
      (make-syntactic-closure transformer-env '()
                              `(lambda (form* transformer-env* usage-env*)
                                 (make-syntactic-closure transformer-env* '()
                                                         (,transformer form* usage-env*)))))))

(define-syntax rsc-macro-transformer
  (lambda (form transformer-env usage-env)
    (%let ((transformer (make-syntactic-closure usage-env '() (cadr form))))
      (make-syntactic-closure transformer-env '()
                              `(lambda (form* transformer-env* usage-env*)
                                 (,transformer form* transformer-env*))))))

(define-syntax capture-syntactic-environment
  (lambda (form transformer-env usage-env)
    (%let ((f (cadr form)))
      `(,f ,usage-env))))

(define-syntax define
  (rsc-macro-transformer
   (lambda (form env)
     (%let ((name-form (cadr form))
            (body-forms (cddr form))
            ($define (close-syntax 'define env))
            ($%define (close-syntax '%define env))
            ($lambda (close-syntax 'lambda env)))
       (if (pair? name-form)
           (%let ((name (car name-form))
                  (params (cdr name-form)))
             `(,$define ,name
                (,$lambda ,params
                  ,@body-forms)))
           `(,$%define ,name-form ,@body-forms))))))

(define-syntax let
  (rsc-macro-transformer
   (lambda (form env)
     (%let ((variable-or-bindings (cadr form))
            ($let (close-syntax '%let env))
            ($set! (close-syntax 'set! env))
            ($lambda (close-syntax 'lambda env)))
       (if (symbol? variable-or-bindings)
           (%let ((bindings (caddr form))
                  (body (cdddr form)))
             (%let ((names (map car bindings))
                    (initial-values (map cadr bindings)))
               `(,$let ((,variable-or-bindings #void))
                  (,$set! ,variable-or-bindings (,$lambda ,names ,@body))
                  (,variable-or-bindings ,@initial-values))))
           (%let ((body (cddr form)))
             `(,$let ,variable-or-bindings ,@body)))))))

(define-syntax when
  (sc-macro-transformer
   (lambda (form env)
     (let ((test (close-syntax (cadr form) env))
           (body (map (lambda (x) (close-syntax x env)) (cddr form))))
       `(if ,test
            (begin ,@body)
            #void)))))
