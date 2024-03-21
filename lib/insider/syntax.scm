;;> @syntax{
;;>   @term{(}binding@_{1} @repeated{binding@_{2}}@term{)}
;;>   body
;;> }
;;>
;;> Each binding has the following syntax:
;;> @nonterminal-def[binding]{@term{(}var init-expr@term{)}}
;;>
;;> Similar to @ref[(insider syntax) let]{@c{let}}, except the
;;> @nonterm{init-expr}s are evaluated left-to-right, and each binding is
;;> performed before the successive @nonterm{init-expr}s, making it possible for
;;> latter @nonterm{init-expr}s to refer to the earlier variables.
;;>
;;> @example{
;;>   @code{
;;>     (let* ((x 7)
;;>            (y (+ 2 x)))
;;>       (* x y)) @evaluates-to{63}
;;>   }
;;> }
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

;;> @syntax{
;;>   @optional{name}
;;>   @term{(}binding@_{1} @repeated{binding@_{2}}@term{)}
;;>   body
;;> }
;;>
;;> Each binding has the following syntax:
;;> @nonterminal-def[binding]{@term{(}var init-expr@term{)}}
;;>
;;> @nonterm{body} is a potentially empty sequence of definitions, followed by a
;;> nonempty sequence of expressions.
;;>
;;> First, the @nonterm{init-expr}s are evaluated in an unspecified order. Once
;;> all @nonterm{init-expr}s have been evaluated, the results of each
;;> @nonterm{init-expr} is bound to the corresponding @nonterm{var} and the
;;> @nonterm{body} is evaluated in an environment where these bindings are
;;> visible. The variables are bound to these values only within the
;;> @nonterm{body} of the @c{let}.
;;>
;;> @example{
;;>   @code{
;;>     (let ((x 1) (y 2))
;;>       (let ((y 5))
;;>         (display (+ x y))
;;>         (newline))
;;>       (display (+ x y))
;;>       (newline))
;;>   }
;;>   This example produces the following output:
;;>   @code{
;;>     6
;;>     3
;;>   }
;;> }
;;>
;;> If the optional @nonterm{name} is present, it is bound within the body of
;;> the @c{let} to a procedure whose formal arguments are the @nonterm{var}s and
;;> whose body is @nonterm{body}. This makes it possible to conveniently define
;;> recursive procedures.
;;>
;;> @example{
;;>   @code{
;;>     (let loop ((i 10) (sum 0))
;;>       (if (zero? i)
;;>           sum
;;>           (loop (- i 1) (+ sum i)))) @evaluates-to{55}
;;>   }
;;> }
(define-syntax let
  (syntax-rules ()
    ((let name ((var expr) ...) body0 body ...)
     (%let ((var expr) ...)
       (letrec* ((name (lambda (var ...) body0 body ...)))
         (name var ...))))

    ((let ((var expr) ...) body0 body ...)
     (%let ((var expr) ...)
       body0 body ...))))

;;> @in-group[letrec]
;;> @syntax{
;;>   @optional{name}
;;>   @term{(}binding@_{1} @repeated{binding@_{2}}@term{)}
;;>   body
;;> }
;;>
;;> Each binding has the following syntax:
;;> @nonterminal-def[binding]{@term{(}var init-expr@term{)}}
;;>
;;> First, the @nonterm{var}s are bound to fresh locations. Then each binding's
;;> @nonterm{init-expr} is evaluated and its result assigned to the
;;> corresponding @nonterm{var}. The @nonterm{init-expr}s are evaluated
;;> left-to-right, and each assignment takes place before any further
;;> @nonterm{init-expr} is evaluated. This makes it possible to define variables
;;> whose values depend on the previous variables in the same expression, and
;;> to define recursive procedures.
;;>
;;> @example{
;;>   @code{
;;>     (letrec ((fact (lambda (n)
;;>                      (if (zero? n)
;;>                          1
;;>                          (* n (fact (- n 1)))))))
;;>       (fact 5)) @evaluates-to{120}
;;>   }
;;> }
;;>
;;> In Insider, the @c{letrec} and @c{letrec*} forms are identical to each
;;> other; both evaluate the @nonterm{init-expr}s left-to-right.
(define-syntax letrec*
  (syntax-rules ()
    ((letrec* ((name expr) ...) body0 body ...)
     (let ((name #void) ...)
       (set! name expr) ...
       body0
       body ...))))

;;> @in-group[letrec]
;;> @syntax{
;;>   @optional{name}
;;>   @term{(}binding@_{1} @repeated{binding@_{2}}@term{)}
;;>   body
;;> }
(define-syntax letrec
  (syntax-rules ()
    ((letrec ((name expr) ...) body0 body ...)
     (letrec* ((name expr) ...) body0 body ...))))

;;> @syntax{
;;>   @term{(}mv-binding@_{1} @repeated{mv-binding@_{2}}@term{)}
;;>   body
;;> }
;;>
;;> Each @nonterm{mv-binding} has the following syntax:
;;> @nonterminal-def[mv-binding]{
;;>   @term{(}
;;>     @term{(}var@_{1} @repeated{var@_{2}}@term{)}
;;>     init-expr
;;>   @term{)}
;;> }
;;>
;;> Similar to @ref[(insider syntax) let]{@c{let}}, the @nonterm{init-expr}s
;;> are evaluated in an unspecified order, and the return values of each
;;> @nonterm{init-expr} are bound to the corresponding @nonterm{var}s. It is
;;> an error if the @nonterm{init-expr} evaluates to a different number of
;;> values than there are @nonterm{var}s.
;;>
;;> @example{
;;>   @code{
;;>     (let-values (((root rem) (exact-integer-sqrt 32)))
;;>       (* root rem)) @evaluates-to{35}
;;>   }
;;> }
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

;;> @syntax{
;;>   @term{(}mv-binding@_{1} @repeated{mv-binding@_{2}}@term{)}
;;>   body
;;> }
;;>
;;> Each @nonterm{mv-binding} has the following syntax:
;;> @nonterminal-def[mv-binding]{
;;>   @term{(}
;;>     @term{(}var@_{1} @repeated{var@_{2}}@term{)}
;;>     init-expr
;;>   @term{)}
;;> }
;;>
;;> Similar to @ref[(insider syntax) let-values]{@c{let-values}}, but, similarly
;;> to @ref[(insider syntax) let*]{@c{let*}} the @nonterm{init-expr}s are
;;> evaluated left-to-right, and each binding is performed before the successive
;;> @nonterm{init-expr}s
;;>
;;> @example{
;;>   @code{
;;>     (let*-values (((a b) (values 1 2))
;;>                   ((c d) (values (* 2 a) (* 2 b))))
;;>       (+ c d)) @evaluates-to{6}
;;>   }
;;> }
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

;;> @syntax{@repeated{clause} @optional{else-clause}}
;;>
;;> Each clause has the following syntax:
;;> @nonterminal-def[clause]{@term{(}test @repeated{expr}@term{)}}
;;> @nonterminal-def[clause]{@term{(}test @term{=>} proc-expr@term{)}}
;;> @nonterminal-def[else-clause]{
;;>   @term{(}@term{else} expr@_{1} @repeated{expr@_{2}}@term{)}
;;> }
;;>
;;> A @c{cond} expression is evaluated by evaluating the @nonterm{test}
;;> expressions of each @nonterm{clause} in order until one of them evaluates to
;;> a true value. When a @nonterm{test} evaluates to a true value, the remaining
;;> @nonterm{expr}s of the same @nonterm{clause} are evaluated and the @c{cond}
;;> expression evaluates to the last @nonterm{expr} of the selected
;;> @nonterm{clause}. The last @nonterm{expr} is in tail position with respect
;;> to the @c{cond}. No further @nonterm{clause}s are evaluated.
;;>
;;> @example{
;;>   @code{
;;>     (cond ((> 3 2) 'greater)
;;>           ((< 3 2) 'less)) @evaluates-to{greater}
;;>   }
;;> }
;;>
;;> If a @nonterm{test} of a @nonterm{clause} evaluates to a true value but
;;> there are no @nonterm{expr}s, then @c{cond} evaluates to the result of the
;;> @nonterm{test} expression.
;;>
;;> @example{
;;>   @code{
;;>     (define (f x)
;;>       (cond ((car x))
;;>             ((not (car x))
;;>              (cdr x))))
;;>
;;>     (f (cons 2 3))  @evaluates-to{2}
;;>     (f (cons #f 3)) @evaluates-to{3}
;;>   }
;;> }
;;>
;;> If a @nonterm{clause} uses the alternate @term{=>} form and its
;;> @nonterm{test} expression evaluates to a true value, its @nonterm{proc-expr}
;;> is evaluated and its result is applied to the result of the @nonterm{test}
;;> expression. If @nonterm{proc-expr} evaluates to an object that cannot be
;;> called with a single value, an exception is raised.
;;>
;;> @example{
;;>   @code{
;;>     (let ((alist '((1 . one) (2 . two))))
;;>       (cond ((assq 1 alist) => cdr)))
;;>     @evaluates-to{one}
;;>   }
;;> }
;;>
;;> Finally, if no @nonterm{test} expression evaluates to a true value, and
;;> an @nonterm{else-clause} is present, its @nonterm{expr}s are evaluated and
;;> the @c{cond} expression evaluates to the result of the last @nonterm{expr},
;;> which is in tail position with respect to the @c{cond}.
;;>
;;> @example{
;;>   @code{
;;>     (let ((x 2))
;;>       (cond ((pair? x)   'pair)
;;>             ((string? x) 'string)
;;>             (else        'mystery)))
;;>     @evaluates-to{mystery}
;;>   }
;;> }
;;>
;;> If no @nonterm{test} expression evaluates to a true value and there is no
;;> @nonterm{else-clause}, the @c{cond} expression evaluates to @c{#void}.
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

;;> @syntax{key-expr @repeated{clause} @optional{else-clause}}
;;> Each clause has the following syntax:
;;> @nonterminal-def[clause]{
;;>   @term{(}@term{(}@repeated{datum}@term{)}
;;>   expr@_{1} @repeated{expr@_{2}}@term{)}
;;> }
;;> @nonterminal-def[clause]{
;;>   @term{(}@term{(}@repeated{datum}@term{)}
;;>   @term{=>}
;;>   proc-expr@term{)}
;;> }
;;> @nonterminal-def[else-clause]{
;;>   @term{(}@term{else}
;;>   expr@_{1} @repeated{expr@_{2}}@term{)}
;;> }
;;> @nonterminal-def[else-clause]{
;;>   @term{(}@term{else} @term{=>} proc-expr@term{)}
;;> }
;;>
;;> First, @nonterm{key-expr} is evaluated once and its result is compared
;;> against each @nonterm{clause}'s @nonterm{datum}s using @c{eqv?}, in order.
;;> If a clause contains the result of @nonterm{key-expr} in its
;;> @nonterm{datum}s, its @nonterm{expr}s are evaluated and the @c{cond}
;;> expression evaluates to the result of the last @nonterm{expr}. This last
;;> @nonterm{expr} is in tail position with respect to the @c{cond}.
;;>
;;> @example{
;;>   @code{
;;>     (case (* 2 3)
;;>       ((2 3 5 7) 'prime)
;;>       ((1 4 6 8 9) 'composite))
;;>     @evaluates-to{composite}
;;>   }
;;> }
;;>
;;> If the selected @nonterm{clause} uses the alternate @term{=>} form, then
;;> @nonterm{proc-expr} is evaluated and its result is applied to the previously
;;> computed result of @nonterm{key-expr}. If @nonterm{proc-expr} evaluates to
;;> an object that cannot be called with a single argument, an exception is
;;> raised.
;;>
;;> @example{
;;>   @code{
;;>     (case (* 2 3)
;;>       ((2 4 6 8) => (lambda (x) (/ x 2)))
;;>       ((1 3 5 7 9) => (lambda (x) (+ (* x 3) 1))))
;;>     @evaluates-to{3}
;;>   }
;;> }
;;>
;;> If no @nonterm{clause} matches the result of @nonterm{key-expr} and there
;;> is an @nonterm{else-clause}, its @nonterm{expr}s are evaluated and the
;;> result of the last one becomes the result of the whole @c{cond} expression.
;;> If the @nonterm{else-clause} uses the alternate @term{=>} form, the result
;;> of evaluating @nonterm{proc-expr} is evaluated to the result of
;;> @nonterm{key-expr}, like for ordinary @nonterm{clause}s.
;;>
;;> @example{
;;>   @code{
;;>     (case 'c
;;>       ((a e i o u) 'vowel)
;;>       ((w y) 'semivowel)
;;>       (else => (lambda (x) x)))
;;>     @evaluates-to{c}
;;>   }
;;> }
;;>
;;> If no @nonterm{clause} matches and there is no @nonterm{else-clause}, the
;;> @c{cond} expression evaluates to @c{#void}.
(define-syntax case
  (syntax-rules ()
    ((case key clause1 clause2 ...)
     (let ((k key))
       (make-case-clauses k clause1 clause2 ...)))))

;;> @syntax{test-expr expr@_{1} @repeated{expr@_{2}}}
;;> @nonterm{test-expr} is evaluated. If its result is a true value, the
;;> @nonterm{expr}s are evaluated in order, and the @c{when} expression
;;> evaluates to the result of the last one. The last @nonterm{expr} is in tail
;;> position with respect to the @c{when} expression.
;;>
;;> If @nonterm{test-expr} evaluates to @c{#f}, the @c{when} expression
;;> evaluates to @c{#void}.
;;>
;;> @example{
;;>   This snippet prints "Less than five" to the current output port and
;;>   evaluates to the symbol @c{too-small}.
;;>   @code{
;;>     (when (< 2 5)
;;>       (display "Less than five")
;;>       'too-small)
;;>     @evaluates-to{too-small}
;;>   }
;;> }
(define-syntax when
  (syntax-rules ()
    ((when condition body0 body ...)
     (if condition
         (begin body0 body ...)))))

;;> @syntax{test-expr expr@_{1} @repeated{expr@_{2}}}
;;> Similar to @ref[(insider syntax) when]{@c{when}}; @nonterm{test-expr} is
;;> evaluated and if it results in @c{#f}, the @nonterm{expr}s are evaluated and
;;> the @c{unless} expression evaluates to the result of the last once. The last
;;> @nonterm{expr} is in tail position with respect to the @c{unless}
;;> expression.
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
