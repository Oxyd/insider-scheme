(library (insider syntax-rules))
(import (insider internal))
(export syntax-match syntax-rules)

(meta
  (define null?
    (lambda (x)
      (eq? x '()))))

(meta
  (define pair?
    (lambda (x)
      (eq? (type x) 'insider::pair))))

(meta
  (define-syntax or
    (lambda (stx)
      (let ((form (syntax->list stx)))
        (if (null? (cdr form))
            #'#f
            (let ((first (cadr form))
                  (rest (cddr form)))
              (if (null? rest)
                  first
                  #`(let ((e #,first))
                      (if e e (or #,@rest))))))))))

(meta
  (define-syntax and
    (lambda (stx)
      (let ((form (syntax->list stx)))
        (let ((first (cadr form))
              (rest (cddr form)))
          (if (null? rest)
              first
              #`(if #,first (and #,@rest) #f)))))))

(meta
  (define symbol?
    (lambda (x)
      (eq? (type x) 'insider::symbol))))

(meta
  (define syntax?
    (lambda (x)
      (eq? (type x) 'insider::syntax))))

(meta
  (define identifier?
    (lambda (x)
      (if (syntax? x)
          (symbol? (syntax-expression x))
          #f))))

(meta
  (define-syntax define-auxiliary-syntax
    (lambda (stx)
      (let ((name (cadr (syntax->list stx))))
        #`(define-syntax #,name
            (lambda (stx)
              #`(syntax-error "Invalid use of auxiliary syntax" #,stx)))))))

(meta (define-auxiliary-syntax else))
(meta (define-auxiliary-syntax =>))

(meta
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
                    (if (and (pair? body) (identifier? (car body))
                             (free-identifier=? (car body) #'=>))
                        (let ((continuation (cadr body)))
                          #`(let ((test #,check))
                              (if test
                                  (#,continuation test)
                                  (cond #,@rest))))
                        #`(if #,check
                              (begin #,@body)
                              (cond #,@rest)))))))))))

(meta
  (define member
    (lambda (x list . rest)
      (let ((pred (if (null? rest) equal? (car rest))))
        (define loop
          (lambda (lst)
            (if (null? lst)
                #f
                (if (pred x (car lst))
                    lst
                    (loop (cdr lst))))))
        (loop list)))))

(meta
  (define memq
    (lambda (x list)
      (member x list eq?))))

(meta
  (define memv
    (lambda (x list)
      (member x list eqv?))))

(meta
  (define vector?
    (lambda (x)
      (eq? (type x) 'insider::vector))))

(meta
  (define list
    (lambda x x)))

(meta
  (define caar
    (lambda (x)
      (car (car x)))))

(meta
  (define cddddr
    (lambda (x)
      (cdr (cdddr x)))))

(meta
  (define assoc
    (lambda (x alist . rest)
      (let ((pred (if (null? rest) equal? (car rest))))
        (define loop
          (lambda (lst)
            (if (null? lst)
                #f
                (if (pred x (caar lst))
                    (car lst)
                    (loop (cdr lst))))))
        (loop alist)))))

(meta
  (define assq
    (lambda (x alist)
      (assoc x alist eq?))))

(meta
  (define map
    (lambda (f list)
      (define go
        (lambda (p)
          (if (null? p)
              '()
              (cons (f (car p)) (go (cdr p))))))
      (go list))))

(meta
  (define map2
    (lambda (f list1 list2)
      (define go
        (lambda (p q)
          (if (or (null? p) (null? q))
              '()
              (cons (f (car p) (car q)) (go (cdr p) (cdr q))))))
      (go list1 list2))))

(meta
  (define reverse
    (lambda (lst)
      (define loop
        (lambda (lst accum)
          (if (null? lst)
              accum
              (loop (cdr lst) (cons (car lst) accum)))))
      (loop lst '()))))

(meta
  (define syntax-car
    (lambda (x)
      (if (syntax? x)
          (car (syntax-expression x))
          (car x)))))

(meta
  (define syntax-cdr
    (lambda (x)
      (if (syntax? x)
          (cdr (syntax-expression x))
          (cdr x)))))

(meta
  (define syntax-cadr
    (lambda (x)
      (syntax-car (syntax-cdr x)))))

(meta
  (define syntax-cddr
    (lambda (x)
      (syntax-cdr (syntax-cdr x)))))

(meta
  (define syntax-null?
    (lambda (x)
      (or (null? x)
          (and (syntax? x)
               (null? (syntax-expression x)))))))

(meta
  (define reverse-syntax-list
    (lambda (lst)
      (define loop
        (lambda (lst accum)
          (if (syntax-null? lst)
              accum
              (loop (syntax-cdr lst) (cons (syntax-car lst) accum)))))
      (loop lst '()))))

(meta
  (define length
    (lambda (list)
      (define loop
        (lambda (lst accum)
          (if (null? lst)
              accum
              (loop (cdr lst) (+ accum 1)))))
      (loop list 0))))

(meta
  (define error
    (lambda (message . irritants)
      (raise (make-error message irritants)))))

(meta
  (define syntax-pair?
    (lambda (x)
      (or (pair? x)
          (and (syntax? x)
               (pair? (syntax-expression x)))))))

(meta
  (define reverse-improper-syntax-list
    (lambda (lst)
      (define loop
        (lambda (e accum)
          (cond ((syntax-pair? e)
                 (loop (syntax-cdr e) (cons (syntax-car e) accum)))
                (else
                 (cons accum e)))))
      (loop lst '()))))

(meta
  (define syntax-vector?
    (lambda (x)
      (and (syntax? x) (vector? (syntax-expression x))))))

(meta
  (define syntax-cons
    (lambda (stx x y)
      (if (syntax? stx)
          (datum->syntax stx (cons x y))
          (cons x y)))))

(meta
  (define underscore?
    (lambda (id)
      (and (syntax? id)
           (eq? (syntax-expression id) '_)))))

(meta
  (define make-match-context
    (lambda (ellipsis literals)
      (let ((ellipsis-is-literal? (member ellipsis literals free-identifier=?)))
        (vector (if ellipsis-is-literal? #f (syntax->datum ellipsis))
                literals)))))

(meta
  (define context-ellipsis
    (lambda (context)
      (vector-ref context 0))))

(meta
  (define context-literals
    (lambda (context)
      (vector-ref context 1))))

(meta
  (define ellipsis?
    (lambda (context id)
      (let ((ellipsis (context-ellipsis context)))
        (and ellipsis
             (syntax? id)
             (eq? (syntax-expression id) ellipsis))))))

(meta
  (define literal?
    (lambda (context id)
      (let ((literals (context-literals context)))
        (and (identifier? id)
             (member id literals free-identifier=?))))))

(meta
  (define followed-by-ellipsis?
    (lambda (context pattern)
      (and (syntax-pair? (syntax-cdr pattern))
           (ellipsis? context (syntax-cadr pattern))))))

(meta
  (define tail-after-ellipsis
    (lambda (pattern)
      (syntax-cddr pattern))))

(meta
  (define extract-pattern-variables
    (lambda (context pattern)
      (define loop
        (lambda (pattern level)
          (cond ((syntax-null? pattern)
                 '())
                ((literal? context pattern)
                 '())
                ((underscore? pattern)
                 '())
                ((ellipsis? context pattern)
                 '())
                ((identifier? pattern)
                 (list (cons pattern level)))
                ((syntax-pair? pattern)
                 (if (followed-by-ellipsis? context pattern)
                     (append (loop (syntax-car pattern) (+ level 1))
                             (loop (syntax-cddr pattern) level))
                     (append (loop (syntax-car pattern) level)
                             (loop (syntax-cdr pattern) level))))
                ((vector? (syntax-expression pattern))
                 (loop (vector->list (syntax-expression pattern)) level))
                (else
                 '()))))
      (loop pattern 0))))

(meta
  (define extract-pattern-variable-ids
    (lambda (context pattern)
      (map car (extract-pattern-variables context pattern)))))

(meta (define <no-match> (list 'no-match)))

(meta
  (define emit-literal-matcher
    (lambda (literal input body)
      #`(if (and (identifier? #,input) (free-identifier=? #,input #'#,literal))
            #,body
            <no-match>))))

(meta
  (define emit-identifier-matcher
    (lambda (pattern input body)
      #`(let ((#,pattern #,input))
          #,body))))

(meta
  (define emit-null-matcher
    (lambda (input body)
      #`(if (syntax-null? #,input)
            #,body
            <no-match>))))

(meta
  (define gensym
    (let ((counter 0))
      (lambda (name)
        (if (identifier? name)
            (gensym (symbol->string (syntax->datum name)))
            (let ((new-name (string-append name "-" (number->string counter))))
              (set! counter (+ counter 1))
              (datum->syntax #'here (string->symbol new-name))))))))

(meta (define emit-matcher #f))

(meta
  (define emit-plain-pair-matcher
    (lambda (context pattern input body end-matcher)
      (let ((head (gensym "head")) (tail (gensym "tail")))
        #`(if (syntax-pair? #,input)
              (let ((#,head (syntax-car #,input))
                    (#,tail (syntax-cdr #,input)))
                #,(emit-matcher context
                                (syntax-car pattern) head
                                (emit-matcher context (syntax-cdr pattern) tail
                                              body end-matcher)
                                emit-null-matcher))
              <no-match>)))))

(meta
  (define emit-repeated-pattern-matcher
    (lambda (context repeated-pattern input body)
      (let ((vars (extract-pattern-variable-ids context repeated-pattern)))
        (let ((vars* (map gensym vars))
              (input* (gensym "input"))
              (recurse (gensym "recurse")))
          #`(letrec* ((#,recurse
                       (lambda (#,input* #,@vars*)
                         (if (syntax-null? #,input*)
                             (let #,(map2 (lambda (v v*) #`(#,v (reverse #,v*))) vars vars*)
                               #,body)
                             (if (syntax-pair? #,input*)
                                 (let ((elem (syntax-car #,input*)))
                                   #,(emit-matcher context repeated-pattern #'elem
                                                   #`(#,recurse (syntax-cdr #,input*)
                                                                #,@(map2 (lambda (v v*) #`(cons #,v #,v*)) vars vars*))
                                                   emit-null-matcher))
                                 <no-match>)))))
              (#,recurse #,input #,@(map (lambda (_) '()) vars))))))))

(meta
  (define emit-ellipsis-tail-matcher
    (lambda (context pattern input emit-repeated-matcher)
      (if (syntax-null? pattern)
          (emit-repeated-matcher input)
          (let ((reversed-pattern (reverse-improper-syntax-list pattern)))
            #`(let ((reversed-input (reverse-improper-syntax-list #,input)))
                #,(emit-matcher context (cdr reversed-pattern) #'(cdr reversed-input)
                                (emit-matcher context (car reversed-pattern) #'(car reversed-input)
                                              #'(syntax-error "This should not be emitted")
                                              (lambda (input body)
                                                (emit-repeated-matcher #`(reverse-syntax-list #,input))))
                                emit-null-matcher)))))))

(meta
  (define emit-ellipsis-pair-matcher
    (lambda (context pattern input body)
      (let ((repeated-pattern (syntax-car pattern))
            (tail-pattern (syntax-cddr pattern)))
        (emit-ellipsis-tail-matcher context tail-pattern input
                                    (lambda (input)
                                      (emit-repeated-pattern-matcher context repeated-pattern input body)))))))

(meta
  (define emit-pair-matcher
    (lambda (context pattern input body end-matcher)
      (if (followed-by-ellipsis? context pattern)
          (emit-ellipsis-pair-matcher context pattern input body)
          (emit-plain-pair-matcher context pattern input body end-matcher)))))

(meta
  (define emit-vector-matcher
    (lambda (context pattern input body end-matcher)
      (let ((pattern-list (vector->list pattern)))
        #`(if (syntax-vector? #,input)
              (let ((lst (vector->list (syntax-expression #,input))))
                #,(emit-matcher context pattern-list #'lst body end-matcher))
              <no-match>)))))

(meta
  (define emit-constant-matcher
    (lambda (pattern input body)
      #`(if (equal? #,(syntax-expression pattern) (syntax-expression #,input))
            #,body
            <no-match>))))

(meta
  (set! emit-matcher
        (lambda (context pattern input body end-matcher)
          (cond ((literal? context pattern)
                 (emit-literal-matcher pattern input body))
                ((underscore? pattern)
                 body)
                ((identifier? pattern)
                 (emit-identifier-matcher pattern input body))
                ((syntax-null? pattern)
                 (end-matcher input body))
                ((syntax-pair? pattern)
                 (emit-pair-matcher context pattern input body end-matcher))
                ((syntax-vector? pattern)
                 (emit-vector-matcher context (syntax-expression pattern) input
                                      body end-matcher))
                (else
                 (emit-constant-matcher pattern input body))))))

(meta
  (define variable-level
    (lambda (id variable-levels)
      (let ((apair (assoc id variable-levels free-identifier=?)))
        (if apair (cdr apair) #f)))))

(meta
  (define check-level*!
    (lambda (expected-level actual-level id)
      (if (> expected-level actual-level)
          (error "Not enough ellipses following template" id)
          (if (< expected-level actual-level)
              (error "Too many ellipses following template" id))))))

(meta
  (define check-level!
    (lambda (id variable-levels level)
      (let ((expected-level (variable-level id variable-levels)))
        (if expected-level
            (check-level*! expected-level level id))))))

(meta (define quasisyntax* #'quasisyntax))
(meta (define unsyntax* #'unsyntax))
(meta (define unsyntax-splicing* #'unsyntax-splicing))

(meta
  (define make-splice-context
    (lambda (match-context variable-levels)
      (vector (context-ellipsis match-context)
              (context-literals match-context)
              variable-levels
              #t))))

(meta
  (define context-variable-levels
    (lambda (context)
      (vector-ref context 2))))

(meta
  (define context-ellipsis-enabled?
    (lambda (context)
      (vector-ref context 3))))

(meta
  (define make-splice-context-with-disabled-ellipses
    (lambda (context)
      (vector (context-ellipsis context)
              (context-literals context)
              (context-variable-levels context)
              #f))))

(meta
  (define splice-identifier-template
    (lambda (context id level)
      (let ((expected-level (variable-level id (context-variable-levels context))))
        (if expected-level
            (begin
              (check-level*! expected-level level id)
              #`(#,unsyntax* #,id))
            id)))))

(meta
  (define split-on-ellipses
    (lambda (context template)
      (define loop
        (lambda (tail num-ellipses)
          (if (and (context-ellipsis-enabled? context)
                   (syntax-pair? tail)
                   (ellipsis? context (syntax-car tail)))
              (loop (syntax-cdr tail) (+ num-ellipses 1))
              (cons tail num-ellipses))))
      (loop (syntax-cdr template) 0))))

(meta
  (define splice-repeated-simple-template
    (lambda (context template level)
      (check-level! template (context-variable-levels context) level)
      #`(#,unsyntax-splicing* #,template))))

(meta
  (define flatten
    (lambda (lol)
      (define loop
        (lambda (lst result)
          (if (syntax-null? lst)
              result
              (loop (syntax-cdr lst) (append result (syntax->list (syntax-car lst)))))))
      (loop lol '()))))

(meta
  (define extract-template-variables
    (lambda (context template)
      (let ((variable-levels (context-variable-levels context)))
        (define loop
          (lambda (template)
            (cond ((syntax-null? template)
                   '())
                  ((and (identifier? template) (assoc template variable-levels free-identifier=?))
                   (list template))
                  ((syntax-pair? template)
                   (append (loop (syntax-car template)) (loop (syntax-cdr template))))
                  ((syntax-vector? template)
                   (loop (vector->list (syntax-expression template))))
                  (else
                   '()))))
        (loop template)))))

(meta (define splice-template #f))

(meta
  (define splice-repeated-complex-template
    (lambda (context template level)
      (let ((template-variables (extract-template-variables context template))
            (loop (gensym "loop")))
        #`(#,unsyntax-splicing*
           (letrec* ((#,loop (lambda #,template-variables
                               (if (or #,@(map (lambda (v) #`(null? #,v)) template-variables))
                                   '()
                                   (cons (let #,(map (lambda (v) #`(#,v (car #,v))) template-variables)
                                           (#,quasisyntax*
                                            #,(splice-template context template level)))
                                         (#,loop #,@(map (lambda (v) #`(cdr #,v)) template-variables)))))))
             (#,loop #,@template-variables)))))))

(meta
  (define flatten-spliced-template
    (lambda (spliced-template count)
      (if (= count 0)
          spliced-template
          (flatten-spliced-template
           #`(#,unsyntax-splicing*
              (flatten (#,quasisyntax* #,spliced-template)))
           (- count 1))))))

(meta
  (define splice-template-followed-by-multiple-ellipses
    (lambda (context template level num-ellipses)
      (let ((spliced-template
             (splice-repeated-complex-template context template
                                               (+ level num-ellipses))))
        (flatten-spliced-template spliced-template (- num-ellipses 1))))))

(meta
  (define simple-template?
    (lambda (template num-ellipses)
      (and (identifier? template) (= num-ellipses 1)))))

(meta
  (define splice-repeated-template
    (lambda (context template level num-ellipses)
      (if (simple-template? template num-ellipses)
          (splice-repeated-simple-template context template (+ level 1))
          (splice-template-followed-by-multiple-ellipses context template level
                                                         num-ellipses)))))

(meta
  (define splice-list-template
    (lambda (context template level)
      (define loop
        (lambda (template)
          (cond ((null? template)
                 '())
                ((syntax-pair? template)
                 (let ((tail&num-ellipses (split-on-ellipses context template)))
                   (let ((tail (car tail&num-ellipses)) (num-ellipses (cdr tail&num-ellipses)))
                     (syntax-cons template
                                  (if (= num-ellipses 0)
                                      (splice-template context (syntax-car template) level)
                                      (splice-repeated-template context (syntax-car template) level num-ellipses))
                                  (loop tail)))))
                (else
                 (splice-template context template level)))))
      (loop template))))

(meta
  (define handle-list-template
    (lambda (context template level)
      (if (and (context-ellipsis-enabled? context)
               (ellipsis? context (syntax-car template)))
          (splice-template (make-splice-context-with-disabled-ellipses context)
                           (syntax-cadr template) level)
          (splice-list-template context template level)))))

(meta
  (define splice-vector-template
    (lambda (context template level)
      (let ((v (syntax-expression template)))
        #`(#,unsyntax*
           (list->vector
            (syntax->list
             (#,quasisyntax*
              #,(splice-template context (vector->list v) level)))))))))

(meta
  (set! splice-template
        (lambda (context template level)
          (cond ((identifier? template)
                 (splice-identifier-template context template level))
                ((syntax-pair? template)
                 (handle-list-template context template level))
                ((syntax-vector? template)
                 (splice-vector-template context template level))
                (else
                 template)))))

(meta
  (define emit-splicer
    (lambda (context template)
      (splice-template context template 0))))

(define-syntax syntax-match*
  (lambda (stx)
    (let ((form (syntax->list stx)))
      (let ((value-expr (cadr form))
            (ellipsis (caddr form))
            (literals (cadddr form))
            (clauses (cddddr form)))
        (let ((context (make-match-context ellipsis (syntax->list literals))))
          (define emit-matchers
            (lambda (clauses input)
              (if (syntax-null? clauses)
                  #'(error "No matching clause")
                  (let ((clause (syntax-car clauses)))
                    (let ((pattern (syntax-car clause))
                          (body (syntax-cadr clause)))
                      #`(let ((clause-result #,(emit-matcher context pattern input body emit-null-matcher)))
                          (if (eq? clause-result <no-match>)
                              #,(emit-matchers (syntax-cdr clauses) input)
                              clause-result)))))))
          #`(let ((value #,value-expr))
              #,(emit-matchers clauses #'value)))))))

(define-syntax syntax-match
  (lambda (stx)
    (syntax-match* stx ... ()
      ((_ value (literals ...) clauses ...)
       #`(syntax-match* #,value ... #,literals #,@clauses))

      ((_ value ellipsis (literals ...) clauses ...)
       #`(syntax-match* #,value #,ellipsis #,literals #,@clauses)))))

(define-syntax syntax-rules
  (lambda (stx)
    (syntax-match stx ()
      ((_ (literals ...) clauses ...)
       #`(syntax-rules ... #,literals #,@clauses))

      ((_ ellipsis (literals ...) (patterns templates) ...)
       (let ((match-context (make-match-context ellipsis (syntax->list literals))))
         (define make-clauses
           (lambda (patterns templates)
             (if (null? patterns)
                 #'()
                 (let ((pattern (syntax-cdr (car patterns))) (template (car templates)))
                   (let ((context (make-splice-context match-context
                                                       (extract-pattern-variables match-context pattern))))
                     (let ((splicer (emit-splicer context template)))
                       #`((#,pattern (#,quasisyntax* #,splicer))
                          . #,(make-clauses (cdr patterns) (cdr templates)))))))))
         #`(lambda (stx)
             (syntax-match (syntax-cdr stx) #,ellipsis #,literals #,@(make-clauses patterns templates))))))))
