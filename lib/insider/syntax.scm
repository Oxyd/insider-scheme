(define-syntax let*
  (syntax-rules ()
    ((let* () body0 body ...)
     (begin body0 body ...))

    ((let* ((var0 expr0) (var expr) ...) body0 body ...)
     (%let ((var0 expr0))
       (let* ((var expr) ...)
         body0 body ...)))))

(define-syntax define
  (syntax-rules ()
    ((define (name . args) body0 body ...)
     (define name
       (lambda args
         body0 body ...)))

    ((define name expr)
     (%define name expr))))

(define-syntax let
  (syntax-rules ()
    ((let name ((var expr) ...) body0 body ...)
     (%let ((var expr) ...)
       (letrec* ((name (lambda (var ...) body0 body ...)))
         (name var ...))))

    ((let ((var expr) ...) body0 body ...)
     (%let ((var expr) ...)
       body0 body ...))))

(define-syntax letrec*
  (syntax-rules ()
    ((letrec* ((name expr) ...) body0 body ...)
     (let ((name #void) ...)
       (set! name expr) ...
       body0
       body ...))))

(define-syntax letrec
  (syntax-rules ()
    ((letrec ((name expr) ...) body0 body ...)
     (letrec* ((name expr) ...) body0 body ...))))

(define-syntax let-values
  (syntax-rules ()
    ((let-values (((names ...) init-exprs) ...) . body)
     (let-values "collect" () () (((names ...) init-exprs) ...) . body))

    ((let-values "collect"
                 (temps ...) (names ...)
                 (((names-1 ...) init-expr-1) ((names-rest ...) init-exprs-rest) ...) . body)
     (let ((temp init-expr-1))
       (let-values "collect"
                   (temps ... temp) (names ... (names-1 ...))
                   (((names-rest ...) init-exprs-rest) ...) . body)))

    ((let-values "collect" (temps ...) (names ...) () . body)
     (let-values "call" (temps ...) (names ...) . body))

    ((let-values "call" (temp temps ...) ((names ...) names-rest ...) . body)
     (call-with-values
       (lambda () temp)
       (lambda (names ...)
         (let-values "call" (temps ...) (names-rest ...) . body))))

    ((let-values "call" () () . body)
     (begin . body))))

(define-syntax let*-values
  (syntax-rules ()
    ((let*-values (((names-1 ...) init-expr-1) ((names-rest ...) init-exprs-rest) ...) . body)
     (call-with-values
       (lambda () init-expr-1)
       (lambda (names-1 ...)
         (let*-values (((names-rest ...) init-exprs-rest) ...) . body))))

    ((let*-values () . body)
     (let () . body))))

(define-syntax define-auxiliary-syntax
  (syntax-rules ()
    ((define-auxiliary-syntax name)
     (define-syntax name
       (lambda (stx)
         #`(syntax-error "Invalid use of auxiliary syntax" #,stx))))))

;;> @auxiliary-syntax
;;> @name{else}
(define-auxiliary-syntax else)

;;> @auxiliary-syntax
;;> @name{=>}
(define-auxiliary-syntax =>)

;;> @auxiliary-syntax
;;> @name{_}
(define-auxiliary-syntax _)

;;> @auxiliary-syntax
;;> @name{...}
(define-auxiliary-syntax ...)

(define-syntax cond
  (syntax-rules (else =>)
    ((cond) #void)

    ((cond (else body0 body ...))
     (begin body0 body ...))

    ((cond (test => expression) rest ...)
     (let ((value test))
       (if value
           (expression value)
           (cond rest ...))))

    ((cond (test body0 body ...) rest ...)
     (if test
         (begin body0 body ...)
         (cond rest ...)))

    ((cond (test) rest ...)
     (let ((value test))
       (if value
           value
           (cond rest ...))))))

(define (memv key list)
  (cond ((eq? list '()) #f)
        ((eqv? key (car list)) #t)
        (else (memv key (cdr list)))))

(define-syntax make-case-clauses
  (syntax-rules (else =>)
    ((make-case-clauses key)
     #void)

    ((make-case-clauses key (else => result))
     (result key))

    ((make-case-clauses key (else body0 body1 ...))
     (begin body0 body1 ...))

    ((make-case-clauses key (else . _) . rest)
     (syntax-error "Invalid case syntax: Extra clauses after an else clause"))

    ((make-case-clauses key ((atoms ...) => result) . rest)
     (if (memv key '(atoms ...))
         (result key)
         (make-case-clauses key . rest)))

    ((make-case-clauses key ((atoms ...) body0 body1 ...) . rest)
     (if (memv key '(atoms ...))
         (begin body0 body1 ...)
         (make-case-clauses key . rest)))))

(define-syntax case
  (syntax-rules ()
    ((case key clause1 clause2 ...)
     (let ((k key))
       (make-case-clauses k clause1 clause2 ...)))))

(define-syntax when
  (syntax-rules ()
    ((when condition body0 body ...)
     (if condition
         (begin body0 body ...)))))

(define-syntax unless
  (syntax-rules ()
    ((unless condition body0 body ...)
     (if condition
         #void
         (begin body0 body ...)))))

(define-syntax bind-do-variables
  (syntax-rules ()
    ((bind-do-variables loop ((name init . _) ...) expr)
     (let loop ((name init) ...)
       expr))))

(define-syntax make-do-iteration
  (syntax-rules ()
    ((make-do-iteration loop (vars ...))
     (make-do-iteration "collect" loop (vars ...) ()))

    ((make-do-iteration "collect" loop () (exprs ...))
     (loop exprs ...))

    ((make-do-iteration "collect" loop ((var0 _ expr0) vars ...) (exprs ...))
     (make-do-iteration "collect" loop (vars ...) (exprs ... expr0)))

    ((make-do-iteration "collect" loop ((var0 _) vars ...) (exprs ...))
     (make-do-iteration "collect" loop (vars ...) (exprs ... var0)))))

(define-syntax do
  (syntax-rules ()
    ((do (vars ...)
         (test final-exprs ...)
       body ...)
     (bind-do-variables
      loop
      (vars ...)
      (if test
          (begin
            final-exprs ...)
          (begin
            body ...
            (make-do-iteration loop (vars ...))))))))

(define-syntax or
  (syntax-rules ()
    ((or) #f)
    ((or e1) e1)
    ((or e1 e2 rest ...)
     (let ((v e1))
       (if v v (or e2 rest ...))))))

;;> @syntax{@repeated{expr}}
;;> All expressions are evaluated left-to-right, if any one of them is @c{#f},
;;> then the whole @c{and} expression evaluates to @c{#f}, and no further
;;> @nonterm{expr} is evaluated. If all expressions evaluate to non-@c{#f}
;;> values, the @c{and} expression evaluates to the result of the last
;;> expression.
;;>
;;> If there are no expressions, @c{and} evaluates to @c{#t}.
;;>
;;> @example{
;;>   @code{
;;>     (and)
;;>     @evaluates-to{#t}
;;>     (and 1 2 'c '(f g))
;;>     @evaluates-to{(f g)}
;;>     (and (= 2 2) (< 2 1))
;;>     @evaluates-to{#f}
;;>   }
;;> }
(define-syntax and
  (syntax-rules ()
    ((and) #t)
    ((and e1) e1)
    ((and e1 e2 rest ...)
     (if e1 (and e2 rest ...) #f))))
