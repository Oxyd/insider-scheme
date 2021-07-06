(library (insider base-scheme))
(import
 (rename (insider internal)
         (define %define)
         (let %let)))

(export define let let* let-syntax letrec-syntax set! lambda if box unbox box-set!
        define-syntax begin begin-for-syntax
        quote quasiquote unquote unquote-splicing syntax-trap syntax-error error
        + - * / = < > >= <= gcd arithmetic-shift bitwise-and bitwise-or bitwise-not
        set-verbose-collection!
        write-simple display newline append list->vector vector->list vector-append
        vector vector? make-vector vector-length vector-ref vector-set!
        cons car caar caadr cdr cadr cdar caddr cadddr cddr cdddr cddddr set-car! set-cdr!
        assq assv assoc memq memv member length any all
        make-string string-length string-append number->string datum->string symbol->string
        list reverse map filter identity
        syntax quasisyntax unsyntax unsyntax-splicing syntax?
        syntax-expression define-auxiliary-syntax syntax-null? syntax-car syntax-cdr syntax-cadr syntax-cddr
        syntax->list syntax->datum datum->syntax free-identifier=? bound-identifier=? unwrap-syntax
        type eq? eqv? equal? pair? symbol? identifier? null? not when unless cond else => case
        do or and
        plain-procedure? native-procedure? closure? procedure? scheme-procedure?
        expand full-expand
        make-record-type make-record-instance record-set! record-ref record-type)

(begin-for-syntax
 (%define pair?
   (lambda (x)
     (eq? (type x) 'insider::pair)))

 (%define symbol?
   (lambda (x)
     (eq? (type x) 'insider::symbol)))

 (%define syntax?
   (lambda (x)
     (eq? (type x) 'insider::syntax)))

 (%define identifier?
   (lambda (x)
     (if (syntax? x)
         (symbol? (syntax-expression x))
         #f)))

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
     (loop lst '())))

 (%define syntax-car
   (lambda (x)
     (if (syntax? x)
         (car (syntax-expression x))
         (car x))))

 (%define syntax-cdr
   (lambda (x)
     (if (syntax? x)
         (cdr (syntax-expression x))
         (cdr x))))

 (%define syntax-cadr
   (lambda (x)
     (syntax-car (syntax-cdr x))))

 (%define syntax-cddr
   (lambda (x)
     (syntax-cdr (syntax-cdr x)))))

(define-syntax define
  (lambda (stx)
    (%let ((form (syntax->list stx)))
      (%let ((name-form (cadr form))
             (body-forms (cddr form)))
        (if (pair? (syntax-expression name-form))
            (%let ((name (car (syntax-expression name-form)))
                   (params (cdr (syntax-expression name-form))))
              #`(define #,name
                  (lambda #,params
                    #,@body-forms)))
            #`(%define #,name-form #,@body-forms))))))

(define-syntax let
  (lambda (stx)
    (%let ((form (syntax->list stx)))
      (%let ((variable-or-bindings (cadr form)))
        (if (identifier? variable-or-bindings)
            (%let ((bindings (syntax->list (caddr form)))
                   (body (cdddr form)))
              (%let ((names (map syntax-car bindings))
                     (initial-values (map syntax-cadr bindings)))
                #`(letrec* ((#,variable-or-bindings (lambda #,names #,@body)))
                    (#,variable-or-bindings #,@initial-values))))
            (%let ((body (cddr form)))
              #`(%let #,variable-or-bindings #,@body)))))))

(define-syntax let*
  (lambda (stx)
    (let ((form (syntax->list stx)))
      (let ((bindings (syntax->list (cadr form)))
            (body (cddr form)))
        (if (null? bindings)
            #`(let () #,@body)
            #`(let (#,(car bindings))
                (let* #,(cdr bindings)
                  #,@body)))))))

(define-syntax when
  (lambda (stx)
    (let ((form (syntax->list stx)))
      (let ((test (cadr form))
            (body (cddr form)))
        #`(if #,test
              (begin #,@body)
              #void)))))

(define-syntax unless
  (lambda (stx)
    (let ((form (syntax->list stx)))
      (let ((test (cadr form))
            (body (cddr form)))
        #`(if #,test
              #void
              (begin #,@body))))))

(define-syntax or
  (lambda (stx)
    (let ((form (syntax->list stx)))
      (let ((first (cadr form))
            (rest (cddr form)))
        (if (null? rest)
            first
            #`(let ((e #,first))
                (if e e (or #,@rest))))))))

(define-syntax and
  (lambda (stx)
    (let ((form (syntax->list stx)))
      (let ((first (cadr form))
            (rest (cddr form)))
        (if (null? rest)
            first
            #`(if #,first (and #,@rest) #f))))))

(define-syntax define-auxiliary-syntax
  (lambda (stx)
    (let ((name (cadr (syntax->list stx))))
      #`(define-syntax #,name
          (lambda (stx)
            #`(syntax-error "Invalid use of auxiliary syntax" #,stx))))))

(define-auxiliary-syntax else)
(define-auxiliary-syntax =>)

(define-syntax cond
  (lambda (stx)
    (let ((form (syntax->list stx)))
      (if (null? (cdr form))
          #'#void
          (let ((first-clause (syntax->list (cadr form)))
                (rest (cddr form)))
            (let ((check (car first-clause))
                  (body (cdr first-clause)))
              (if (and (identifier? check) (free-identifier=? check #'else))
                  (if (null? rest)
                      #`(begin #,@body)
                      #'(syntax-error "else not the last clause of cond"))
                  (if (and (pair? body) (identifier? (car body)) (free-identifier=? (car body) #'=>))
                      (let ((continuation (cadr body)))
                        #`(let ((test #,check))
                            (if test
                                (#,continuation test)
                                (cond #,@rest))))
                      #`(if #,check
                            (begin #,@body)
                            (cond #,@rest))))))))))

(define-syntax case
  (lambda (stx)
    (let ((form (syntax->list stx)))
      (let ((test-expr (cadr form))
            (clauses (cddr form)))
        #`(let ((test-value #,test-expr))
            (cond
             #,@(let loop ((clauses clauses) (accum '()))
                  (if (null? clauses)
                      (reverse accum)
                      (let ((clause (syntax->list (car clauses)))
                            (rest (cdr clauses)))
                        (let ((cases (car clause))
                              (exprs (cdr clause)))
                          (let ((test-expr (if (and (identifier? cases)
                                                    (free-identifier=? cases #'else))
                                               #'else
                                               #`(or #,@(map (lambda (c) #`(eqv? test-value '#,c)) (syntax->list cases)))))
                                (then-exprs (if (and (pair? exprs)
                                                     (identifier? (car exprs))
                                                     (free-identifier=? (car exprs) #'=>))
                                                #`((#,(cadr exprs) test-value))
                                                exprs)))
                            (loop rest
                                  (cons #`(#,test-expr #,@then-exprs) accum)))))))))))))

(define-syntax do
  (lambda (stx)
    (let ((form (syntax->list stx)))
      (let ((bindings (syntax->list (cadr form)))
            (test (syntax-car (caddr form)))
            (result-exprs (syntax-cdr (caddr form)))
            (body (cdddr form)))
        (let ((bound-names (map syntax-car bindings)))
          #`(let loop #,(map (lambda (binding)
                               #`(#,(syntax-car binding) #,(syntax-cadr binding)))
                             bindings)
              (if #,test
                  (begin #,@result-exprs)
                  (begin
                    #,@body
                    (loop #,@(map (lambda (binding)
                                    (let ((binding* (syntax->list binding)))
                                      (if (null? (cddr binding*))
                                          (car binding*)
                                          (caddr binding*))))
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

(define (cddddr x)
  (cdr (cdddr x)))

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

(define (any pred lst) ;; TODO: SRFI 1 specifies this for arbitrary number of lists.
  (let loop ((elem lst))
    (if (null? elem)
        #f
        (let ((value (pred (car elem))))
          (or value (loop (cdr elem)))))))

(define (all pred lst)
  (let loop ((elem lst))
    (if (null? elem)
        #t
        (and (pred (car elem))
             (loop (cdr elem))))))

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

(define (vector? x)
  (eq? (type x) 'insider::vector))

(define (unwrap-syntax x)
  (if (syntax? x)
      (syntax-expression x)
      x))

(define (syntax-null? x)
  (or (null? x)
      (and (syntax? x)
           (null? (syntax-expression x)))))

(define (full-expand stx)
  (let* ((stx* (expand stx))
         (e (syntax-expression stx*)))
    (cond ((pair? e)
           (datum->syntax stx (map full-expand (syntax->list stx*))))
          (else
           (expand stx)))))
