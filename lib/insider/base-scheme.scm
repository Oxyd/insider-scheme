(library (insider base-scheme))
(import
 (rename (insider internal)
         (define %define)
         (let %let)))

(export sc-macro-transformer rsc-macro-transformer capture-syntactic-environment define
        let let* set! lambda if box unbox box-set! define-syntax begin begin-for-syntax
        quote quasiquote unquote unquote-splicing expand-quote syntax-trap
        + - * / = < > >= <= gcd arithmetic-shift bitwise-and bitwise-or bitwise-not
        set-verbose-collection!
        write-simple display newline append list->vector vector-append
        vector make-vector vector-length vector-ref vector-set!
        cons car caar caadr cdr cadr cdar caddr cadddr cddr cdddr
        assq assv assoc memq memv member length
        make-string string-length string-append number->string datum->string symbol->string
        list reverse map filter identity
        make-syntactic-closure syntactic-closure-expression syntactic-closure-environment
        type eq? eqv? equal? pair? symbol? syntactic-closure? identifier? null? not when unless cond case
        do or and
        plain-procedure? native-procedure? closure? procedure? scheme-procedure?)

(begin-for-syntax
 (%define pair?
   (lambda (x)
     (eq? (type x) 'insider::pair)))

 (%define symbol?
   (lambda (x)
     (eq? (type x) 'insider::symbol)))

 (%define syntactic-closure?
   (lambda (x)
     (eq? (type x) 'insider::syntactic_closure)))

 (%define identifier?
   (lambda (x)
     (if (symbol? x)
         #t
         (if (syntactic-closure? x)
             (symbol? (syntactic-closure-expression x))
             #f))))

 (%define close-syntax
   (lambda (form env)
     (make-syntactic-closure env '() form)))

 (%define close-list
   (lambda (lst env)
     (map (lambda (x) (close-syntax x env)) lst)))

 (%define null?
   (lambda (x)
     (eq? x '())))

 (%define map
   (lambda (f list)
     (%define go
       (lambda (p)
         (if (null? p)
             '()
             (cons (f (car p)) (go (cdr p))))))
     (go list)))

 (%define not
   (lambda (x)
     (eq? x #f)))

 (%define reverse
   (lambda (lst)
     (%define loop
       (lambda (lst accum)
         (if (null? lst)
             accum
             (loop (cdr lst) (cons (car lst) accum)))))
     (loop lst '()))))

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
       (if (identifier? variable-or-bindings)
           (%let ((bindings (caddr form))
                  (body (cdddr form)))
             (%let ((names (map car bindings))
                    (initial-values (map cadr bindings)))
               `(,$let ((,variable-or-bindings #void))
                  (,$set! ,variable-or-bindings (,$lambda ,names ,@body))
                  (,variable-or-bindings ,@initial-values))))
           (%let ((body (cddr form)))
                 `(,$let ,variable-or-bindings ,@body)))))))

(define-syntax let*
  (rsc-macro-transformer
   (lambda (form env)
     (let ((bindings (cadr form))
           (body (cddr form))
           ($let (close-syntax 'let env))
           ($let* (close-syntax 'let* env)))
       (if (null? bindings)
           `(,$let () ,@body)
           `(,$let (,(car bindings))
              (,$let* ,(cdr bindings)
                ,@body)))))))

(define-syntax when
  (sc-macro-transformer
   (lambda (form env)
     (let ((test (close-syntax (cadr form) env))
           (body (close-list (cddr form) env)))
       `(if ,test
            (begin ,@body)
            #void)))))

(define-syntax unless
  (sc-macro-transformer
   (lambda (form env)
     (let ((test (close-syntax (cadr form) env))
           (body (close-list (cddr form) env)))
       `(if ,test
            #void
            (begin ,@body))))))

(define-syntax cond
  (rsc-macro-transformer
   (lambda (form env)
     (if (not (null? (cdr form)))
         (let ((first-clause (cadr form))
               (rest (cddr form)))
           (let ((check (car first-clause))
                 (body (cdr first-clause) env)
                 ($if (close-syntax 'if env))
                 ($begin (close-syntax 'begin env))
                 ($cond (close-syntax 'cond env)))
             `(,$if ,check
                  (,$begin ,@body)
                  (,$cond ,@rest))))
         #void))))

(define-syntax or
  (sc-macro-transformer
   (lambda (form env)
     (let ((first (cadr form))
           (rest (cddr form)))
       (if (null? rest)
           (close-syntax first env)
           `(let ((e ,(close-syntax first env)))
              (if e e ,(close-syntax `(or ,@rest) env))))))))

(define-syntax and
  (sc-macro-transformer
   (lambda (form env)
     (let ((first (cadr form))
           (rest (cddr form)))
       (if (null? rest)
           (close-syntax first env)
           `(if ,(close-syntax first env)
                ,(close-syntax `(and ,@rest) env)
                #f))))))

(define-syntax case
  (sc-macro-transformer
   (lambda (form env)
     (let ((test-expr (close-syntax (cadr form) env))
           (clauses (cddr form)))
       `(let ((test-value ,test-expr))
          (cond
           ,@(let loop ((clauses clauses)
                        (accum '()))
               (if (null? clauses)
                   (reverse accum)
                   (let ((clause (car clauses))
                         (rest (cdr clauses)))
                     (let ((cases (car clause))
                           (exprs (cdr clause)))
                       (loop rest
                             (cons `((or ,@(map (lambda (c) `(eqv? test-value ',c))
                                                cases))
                                     ,@(close-list exprs env))
                                   accum))))))))))))

(define-syntax do
  (rsc-macro-transformer
   (lambda (form env)
     (let ((bindings (cadr form))
           (test (car (caddr form)))
           (result-exprs (cdr (caddr form)))
           (body (cdddr form))
           ($let (close-syntax 'let env))
           ($if (close-syntax 'if env))
           ($begin (close-syntax 'begin env))
           ($set! (close-syntax 'set env))
           ($lambda (close-syntax 'lambda env))
           (loop (close-syntax 'loop env)))
       (let ((bound-names (map car bindings)))
         `(,$let ,loop ,(map (lambda (binding)
                              `(,(car binding) ,(cadr binding)))
                             bindings)
            (,$if ,test
                  (,$begin ,@result-exprs)
                  (,$begin
                   ,@body
                   (,loop ,@(map (lambda (binding)
                                   (if (null? (cddr binding))
                                       (car binding)
                                       (caddr binding)))
                                 bindings))))))))))

(define (filter pred list)
  (reverse
   (let loop ((l list)
              (accum '()))
     (if (null? l)
         accum
         (loop (cdr l)
               (if (pred (car l))
                   (cons (car l) accum)
                   accum))))))

(define (identity x)
  x)

(define (list . xs)
  xs)

(define (caar x)
  (car (car x)))

(define (caadr x)
  (car (car (cdr x))))

(define (cdar x)
  (cdr (car x)))

(define (assoc x alist . rest)
  (let ((pred (if (null? rest) equal? (car rest))))
    (let loop ((lst alist))
      (if (null? lst)
          #f
          (if (pred x (caar lst))
              (car lst)
              (loop (cdr lst)))))))

(define (assq x alist)
  (assoc x alist eq?))

(define (assv x alist)
  (assoc x alist eqv?))

(define (member x list . rest)
  (let ((pred (if (null? rest) equal? (car rest))))
    (let loop ((lst list))
      (if (null? lst)
          #f
          (if (pred x (car lst))
              lst
              (loop (cdr lst)))))))

(define (memq x list)
  (member x list eq?))

(define (memv x list)
  (member x list eqv?))

(define (length list)
  (do ((lst list (cdr lst))
       (result 0 (+ 1 result)))
      ((null? lst) result)))

(define (plain-procedure? x)
  (eq? (type x) 'insider::procedure))

(define (native-procedure? x)
  (eq? (type x) 'insider::native_procedure))

(define (closure? x)
  (eq? (type x) 'insider::closure))

(define (procedure? x)
  (or (plain-procedure? x) (native-procedure? x) (closure? x)))

(define (scheme-procedure? x)
  (or (plain-procedure? x) (closure? x)))
