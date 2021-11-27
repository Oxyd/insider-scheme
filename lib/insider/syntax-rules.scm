(library (insider syntax-rules))
(import (insider internal))
(export syntax-match syntax-rules)

(begin-for-syntax
 (begin-for-syntax
  (begin-for-syntax
   (define null?
     (lambda (x)
       (eq? x '())))

   (define pair?
     (lambda (x)
       (eq? (type x) 'insider::pair)))

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
   
   (define symbol?
     (lambda (x)
       (eq? (type x) 'insider::symbol)))

   (define syntax?
     (lambda (x)
       (eq? (type x) 'insider::syntax)))

   (define identifier?
     (lambda (x)
       (if (syntax? x)
           (symbol? (syntax-expression x))
           #f))))
  
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
                              (cond #,@rest)))))))))))

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
       (loop list))))

 (define memq
   (lambda (x list)
     (member x list eq?)))

 (define memv
   (lambda (x list)
     (member x list eqv?)))

 (define vector?
   (lambda (x)
     (eq? (type x) 'insider::vector)))

 (define not
   (lambda (x)
     (eq? x #f)))

 (define list
   (lambda x x))

 (define caar
   (lambda (x)
     (car (car x))))

 (define cddddr
   (lambda (x)
     (cdr (cdddr x))))

 (define assq
   (lambda (x alist)
     (define loop
       (lambda (lst)
         (if (null? lst)
             #f
             (if (eq? x (caar lst))
                 (car lst)
                 (loop (cdr lst))))))
     (loop alist)))

 (define map
   (lambda (f list)
     (define go
       (lambda (p)
         (if (null? p)
             '()
             (cons (f (car p)) (go (cdr p))))))
     (go list)))

 (define reverse
   (lambda (lst)
     (define loop
       (lambda (lst accum)
         (if (null? lst)
             accum
             (loop (cdr lst) (cons (car lst) accum)))))
     (loop lst '())))

 (define length
   (lambda (list)
     (define loop
       (lambda (lst accum)
         (if (null? lst)
             accum
             (loop (cdr lst) (+ accum 1)))))
     (loop list 0)))

 (define filter
   (lambda (pred list)
     (define loop
       (lambda (l accum)
         (if (null? l)
             accum
             (loop (cdr l)
                   (if (pred (car l))
                       (cons (car l) accum)
                       accum)))))
     (reverse (loop list '()))))

 (define any
   (lambda (pred lst)
     (define loop
       (lambda (elem)
         (if (null? elem)
             #f
             (let ((value (pred (car elem))))
               (if value value (loop (cdr elem)))))))
     (loop lst)))

 (define error
   (lambda (message . irritants)
     (raise (make-error message irritants))))

 (define no-binding (list 'no-binding))

 (define empty-binding?
   (lambda (binding)
     (eq? binding no-binding)))

 (define merge-level!
   (lambda (x-alist y-alist)
     (define loop
       (lambda (x)
         (if (null? x)
             y-alist
             (let ((x-binding (car x)))
               (cond ((assq (car x-binding) y-alist)
                      => (lambda (y-binding)
                           ;; When merging the first level, the binding will be a
                           ;; scalar value. In this case we merge scalar value with
                           ;; the empty binding to the scalar value, and disallow
                           ;; merging with non-empty lists.
                           (cond ((and (not (empty-binding? (cdr x-binding)))
                                       (not (empty-binding? (cdr y-binding))))
                                  (set-cdr! y-binding (append (cdr x-binding) (cdr y-binding)))
                                  (loop (cdr x)))
                                 ((and (empty-binding? (cdr x-binding)) (not (empty-binding? (cdr y-binding))))
                                  (loop (cdr x)))
                                 ((and (empty-binding? (cdr y-binding)) (not (empty-binding? (cdr x-binding))))
                                  (set-cdr! y-binding (cdr x-binding))
                                  (loop (cdr x)))
                                 (else
                                  (error "Merging different levels" x-binding y-binding)))))
                     (else
                      (set! y-alist (cons (car x) y-alist))
                      (loop (cdr x))))))))
     (loop x-alist)))

 (define merge-bindings!
   (lambda (xs ys)
     (define loop
       (lambda (x y accum)
         (cond ((and (null? x) (null? y))
                (reverse accum))
               ((null? x)
                (loop x (cdr y) (cons (car y) accum)))
               ((null? y)
                (loop (cdr x) y (cons (car x) accum)))
               (else
                (loop (cdr x) (cdr y) (cons (merge-level! (car x) (car y)) accum))))))
     (loop xs ys '())))

 (define shift-binding
   (lambda (binding)
     (let ((key (car binding)) (value (cdr binding)))
       (if (empty-binding? value)
           `(,key . ())
           `(,key . (,value))))))

 (define shift-level
   (lambda (level)
     (map shift-binding level)))

 (define shift-bindings
   (lambda (bindings)
     (cons '() (map shift-level bindings))))

 (define syntax-null?
   (lambda (x)
     (or (null? x)
         (and (syntax? x)
              (null? (syntax-expression x))))))

 (define syntax-car
   (lambda (x)
     (if (syntax? x)
         (car (syntax-expression x))
         (car x))))

 (define syntax-cdr
   (lambda (x)
     (if (syntax? x)
         (cdr (syntax-expression x))
         (cdr x))))

 (define syntax-cadr
   (lambda (x)
     (syntax-car (syntax-cdr x))))

 (define syntax-cddr
   (lambda (x)
     (syntax-cdr (syntax-cdr x))))

 (define unwrap-syntax
   (lambda (x)
     (if (syntax? x)
         (syntax-expression x)
         x)))

 (define reverse-improper-syntax-list
   (lambda (lst)
     (define loop
       (lambda (e accum)
         (cond ((syntax-pair? e)
                (loop (syntax-cdr e) (cons (syntax-car e) accum)))
               (else
                (cons accum e)))))
     (loop lst '())))

 (define bind
   (lambda (value continuation)
     (if value
         (continuation value)
         #f)))

 (define match-tail/list
   (lambda (pattern expression ellipsis literals)
     (let ((pattern* (reverse-improper-syntax-list pattern))
           (expression* (reverse-improper-syntax-list expression)))
       (bind (match-clause (cdr pattern*) (cdr expression*) ellipsis literals)
             (lambda (tail-match)
               (define loop
                 (lambda (pat expr accum)
                   (cond ((null? pat)
                          (cons accum (length expr)))
                         ((null? expr)
                          #f)
                         (else
                          (bind (match-clause (syntax-car pat) (syntax-car expr) ellipsis literals)
                                (lambda (m)
                                  (loop (syntax-cdr pat) (syntax-cdr expr) (merge-bindings! accum m))))))))
               (loop (car pattern*) (car expression*) tail-match))))))

 (define syntax-pair?
   (lambda (x)
     (or (pair? x)
         (and (syntax? x)
              (pair? (syntax-expression x))))))

 (define syntax-vector?
   (lambda (x)
     (and (syntax? x) (vector? (syntax-expression x)))))

 (define ellipsis?
   (lambda (id ellipsis)
     (and ellipsis
          (syntax? id)
          (eq? (syntax-expression id) ellipsis))))

 (define underscore?
   (lambda (id)
     (and (syntax? id)
          (eq? (syntax-expression id) '_))))

 (define literal?
   (lambda (id literals)
     (and (identifier? id)
          (member id literals free-identifier=?))))

 (define followed-by-ellipsis?
   (lambda (x ellipsis)
     (and (syntax-pair? (syntax-cdr x)) (ellipsis? (syntax-cadr x) ellipsis))))

 (define first-level-pattern-variables
   (lambda (pattern ellipsis)
     (cond ((underscore? pattern)
            '())
           ((identifier? pattern)
            (list (syntax-expression pattern)))
           ((syntax-pair? pattern)
            (if (followed-by-ellipsis? pattern ellipsis)
                (first-level-pattern-variables (syntax-cddr pattern) ellipsis)
                (set-union (first-level-pattern-variables (syntax-car pattern) ellipsis)
                           (first-level-pattern-variables (syntax-cdr pattern) ellipsis)
                           eq?)))
           ((syntax-vector? pattern)
            (let ((v (syntax-expression pattern)))
              (define loop
                (lambda (i accum)
                  (cond ((>= i (vector-length v))
                         accum)
                        ((and (< (+ i 1) (vector-length v))
                              (ellipsis? (vector-ref v (+ i 1)) ellipsis))
                         (loop (+ i 2) accum))
                        (else
                         (loop (+ i 1) (set-union (first-level-pattern-variables (vector-ref v i) ellipsis)
                                                  accum
                                                  eq?))))))
              (loop 0 '())))
           (else
            '()))))

 (define make-empty-bindings
   (lambda (pattern ellipsis)
     (cond ((underscore? pattern)
            '())
           ((identifier? pattern)
            `(((,(syntax-expression pattern) . ,no-binding))))
           ((syntax-pair? pattern)
            (cond ((followed-by-ellipsis? pattern ellipsis)
                   (merge-bindings! (shift-bindings (make-empty-bindings (syntax-car pattern) ellipsis))
                                    (make-empty-bindings (syntax-cddr pattern) ellipsis)))
                  (else
                   (merge-bindings! (make-empty-bindings (syntax-car pattern) ellipsis)
                                    (make-empty-bindings (syntax-cdr pattern) ellipsis)))))
           ((syntax-vector? pattern)
            (let ((v (syntax-expression pattern)))
              (define loop
                (lambda (i accum)
                  (cond ((>= i (vector-length v))
                         accum)
                        ((and (< (+ i 1) (vector-length v))
                              (ellipsis? (vector-ref v (+ i 1)) ellipsis))
                         (loop (+ i 2)
                               (merge-bindings! (shift-bindings (make-empty-bindings (vector-ref v i) ellipsis))
                                                accum)))
                        (else
                         (loop (+ i 1)
                               (merge-bindings! (make-empty-bindings (vector-ref v i) ellipsis)
                                                accum))))))
              (loop 0 '())))
           (else
            '()))))

 (define match-repeatedly/list
   (lambda (pattern expression num-elems ellipsis literals)
     (define loop
       (lambda (e accum i)
         (if (= i 0)
             accum
             (bind (match-clause pattern (syntax-car e) ellipsis literals)
                   (lambda (m)
                     (loop (syntax-cdr e) (merge-bindings! accum (shift-bindings m)) (- i 1)))))))
     (loop expression '() num-elems)))

 (define match-repeatedly/vector
   (lambda (pattern expression begin end ellipsis literals)
     (define loop
       (lambda (i accum)
         (if (= i end)
             accum
             (bind (match-clause pattern (vector-ref expression i) ellipsis literals)
                   (lambda (m)
                     (loop (+ i 1) (merge-bindings! accum (shift-bindings m))))))))
     (loop begin '())))

 (define match-literal
   (lambda (pattern expression)
     (if (and (identifier? expression)
              (free-identifier=? pattern expression))
         '()
         #f)))

 (define match-list-followed-by-ellipsis
   (lambda (pattern expression ellipsis literals)
     (bind (match-tail/list (syntax-cddr pattern) expression ellipsis literals)
           (lambda (tail-result)
             (let ((tail-match (car tail-result))
                   (num-unmatched (cdr tail-result)))
               (bind (match-repeatedly/list (syntax-car pattern) expression num-unmatched ellipsis literals)
                     (lambda (repeated-match)
                       (merge-bindings! repeated-match tail-match))))))))

 (define match-plain-list
   (lambda (pattern expression ellipsis literals)
     (let ((car-match (match-clause (syntax-car pattern) (syntax-car expression) ellipsis literals))
           (cdr-match (match-clause (syntax-cdr pattern) (syntax-cdr expression) ellipsis literals)))
       (if (and car-match cdr-match)
           (merge-bindings! car-match cdr-match)
           #f))))

 (define match-list
   (lambda (pattern expression ellipsis literals)
     (cond ((followed-by-ellipsis? pattern ellipsis)
            (match-list-followed-by-ellipsis pattern expression ellipsis literals))
           ((syntax-pair? expression)
            (match-plain-list pattern expression ellipsis literals))
           (else #f))))

 (define match-vector
   (lambda (pattern expression ellipsis literals)
     (if (syntax-vector? expression)
         (let ((pattern* (unwrap-syntax pattern))
               (expression* (unwrap-syntax expression)))
           (define loop
             (lambda (i j accum)
               (if (= i (vector-length pattern*))
                   accum
                   (let ((next (+ i 1)))
                     (cond ((and (< next (vector-length pattern*)) (ellipsis? (vector-ref pattern* next) ellipsis))
                            (let ((repeated-length (+ 1 (- (vector-length expression*) (vector-length pattern*)))))
                              (let ((repeated-end (+ j repeated-length 1)))
                                (bind (match-repeatedly/vector (vector-ref pattern* i) expression* j repeated-end ellipsis literals)
                                      (lambda (repeated-match)
                                        (loop (+ next 1) repeated-end (merge-bindings! accum repeated-match)))))))
                           ((>= j (vector-length expression*))
                            #f)
                           (else
                            (bind (match-clause (vector-ref pattern* i) (vector-ref expression* j) ellipsis literals)
                                  (lambda (m)
                                    (loop next (+ j 1) (merge-bindings! accum m))))))))))
           (loop 0 0 '()))
         #f)))

 (define match-clause
   (lambda (pattern expression ellipsis literals)
     (cond ((literal? pattern literals)
            (match-literal pattern expression))
           ((underscore? pattern)
            '())
           ((identifier? pattern)
            `(((,(syntax-expression pattern) . ,expression))))
           ((syntax-pair? pattern)
            (match-list pattern expression ellipsis literals))
           ((syntax-vector? pattern)
            (match-vector pattern expression ellipsis literals))
           ((equal? (unwrap-syntax pattern) (unwrap-syntax expression))
            '())
           (else
            #f))))

 (define match-clause*
   (lambda (pattern expression ellipsis-stx literals-stx)
     (let ((ellipsis (syntax->datum ellipsis-stx)))
       (bind (match-clause pattern expression ellipsis (syntax->list literals-stx))
             (lambda (bindings)
               (merge-bindings! bindings (make-empty-bindings pattern ellipsis)))))))

 (define set-union
   (lambda (x y compare)
     (define loop
       (lambda (elem accum)
         (cond ((null? elem) accum)
               ((member (car elem) accum compare)
                (loop (cdr elem) accum))
               (else
                (loop (cdr elem) (cons (car elem) accum))))))
     (loop y x)))

 (define all-pattern-ids
   (lambda (pattern ellipsis literals)
     (cond ((null? pattern)
            '())
           ((literal? pattern literals)
            '())
           ((and (identifier? pattern) (free-identifier=? pattern #'_))
            '())
           ((ellipsis? pattern ellipsis)
            '())
           ((identifier? pattern)
            (list pattern))
           ((syntax-pair? pattern)
            (set-union (all-pattern-ids (syntax-car pattern) ellipsis literals)
                       (all-pattern-ids (syntax-cdr pattern) ellipsis literals)
                       bound-identifier=?))
           ((vector? (syntax-expression pattern))
            (let ((v (syntax-expression pattern)))
              (define loop
                (lambda (i accum)
                  (cond ((>= i (vector-length v))
                         accum)
                        ((ellipsis? (vector-ref v i) ellipsis)
                         (loop (+ i 1) accum))
                        (else
                         (loop (+ i 1) (set-union accum
                                                  (all-pattern-ids (vector-ref v i) ellipsis literals)
                                                  bound-identifier=?))))))
              (loop 0 '())))
           (else
            '()))))

 (define all-template-variables
   (lambda (template ellipsis literals)
     (cond ((null? template)
            '())
           ((literal? template literals)
            '())
           ((ellipsis? template ellipsis)
            '())
           ((identifier? template)
            (list (syntax-expression template)))
           ((syntax-pair? template)
            (set-union (all-template-variables (syntax-car template) ellipsis literals)
                       (all-template-variables (syntax-cdr template) ellipsis literals)
                       eq?))
           ((syntax-vector? template)
            (let ((v (syntax-expression template)))
              (define loop
                (lambda (i accum)
                  (cond ((>= i (vector-length v))
                         accum)
                        ((ellipsis? (vector-ref v i) ellipsis)
                         (loop (+ i 1) accum))
                        (else
                         (loop (+ i 1) (set-union accum
                                                  (all-template-variables (vector-ref v i) ellipsis literals)
                                                  eq?))))))
              (loop 0 '())))
           (else
            '()))))

 (define flatten-bindings
   (lambda (bindings)
     (define loop
       (lambda (accum level)
         (if (null? level)
             accum
             (letrec* ((inner-loop
                        (lambda (accum binding)
                          (if (null? binding)
                              (loop accum (cdr level))
                              (inner-loop (cons (car binding) accum) (cdr binding))))))
               (inner-loop accum (car level))))))
     (loop '() bindings)))

 (define make-bindings
   (lambda (pattern match ellipsis literals)
     (let ((ids (all-pattern-ids pattern (syntax->datum ellipsis) (syntax->list literals))))
       (define loop
         (lambda (accum id)
           (if (null? id)
               accum
               (loop #`((#,(car id) (cdr (assq '#,(syntax-expression (car id)) #,match))) . #,accum)
                     (cdr id)))))
       (loop '() ids))))

 (define filter-match
   (lambda (match variables)
     (map (lambda (level)
            (filter (lambda (binding)
                      (memq (car binding) variables))
                    level))
          match)))

 (define match-level-null?
   (lambda (level)
     (any (lambda (binding)
            (null? (cdr binding)))
          level)))

 (define match-null?
   (lambda (match)
     (any match-level-null? match)))

 (define match-car
   (lambda (match)
     (map (lambda (level)
            (map (lambda (binding)
                   (cons (car binding) (cadr binding)))
                 level))
          match)))

 (define match-cdr
   (lambda (match)
     (map (lambda (level)
            (map (lambda (binding)
                   (cons (car binding) (cddr binding)))
                 level))
          match)))

 (define expand-repeatedly/list
   (lambda (template match current-level variable-levels ellipsis literals)
     (let ((vars (all-template-variables template ellipsis literals)))
       (define loop
         (lambda (m accum)
           (cond ((match-null? m)
                  (reverse accum))
                 (else
                  (loop (match-cdr m)
                        (cons (expand-template template (match-car m) current-level variable-levels ellipsis literals)
                              accum))))))
       (loop (filter-match match vars) '()))))

 (define wrap-expansion
   (lambda (template expansion)
     (if (syntax? template)
         (datum->syntax template expansion)
         expansion)))

 (define car*
   (lambda (x)
     (if (null? x)
         '()
         (car x))))

 (define cdr*
   (lambda (x)
     (if (null? x)
         '()
         (cdr x))))

 (define find-binding-at-current-match-level
   (lambda (identifier match)
     (assq identifier (car* match))))

 (define expand-template
   (lambda (template match current-level variable-levels ellipsis literals)
     (cond ((syntax-pair? template)
            (if (ellipsis? (syntax-car template) ellipsis)
                (expand-template (syntax-cadr template) match variable-levels #f literals)
                (wrap-expansion template
                                (if (followed-by-ellipsis? template ellipsis)
                                    (append (expand-repeatedly/list (syntax-car template)
                                                                    (cdr* match)
                                                                    (+ current-level 1)
                                                                    variable-levels
                                                                    ellipsis
                                                                    literals)
                                            (expand-template (syntax-cddr template)
                                                             match
                                                             current-level
                                                             variable-levels
                                                             ellipsis
                                                             literals))
                                    (cons (expand-template (syntax-car template)
                                                           match
                                                           current-level
                                                           variable-levels
                                                           ellipsis
                                                           literals)
                                          (expand-template (syntax-cdr template)
                                                           match
                                                           current-level
                                                           variable-levels
                                                           ellipsis
                                                           literals))))))
           ((syntax-vector? template)
            (wrap-expansion template
                            (list->vector (expand-template (vector->list (syntax-expression template))
                                                           match
                                                           variable-levels
                                                           ellipsis
                                                           literals))))
           ((identifier? template)
            (let ((id (syntax-expression template)))
              (cond ((assq id variable-levels)
                     => (lambda (variable-level)
                          (if (> (cdr variable-level) current-level)
                            (error "Not enough ellipses following template" id))
                          (if (< (cdr variable-level) current-level)
                            (error "Too many ellipses following template" id))
                          (cdr (assq id (car* match)))))
                    (else template))))
           (else template))))

 (define get-variable-names
   (lambda (level)
     (map car level)))

 (define assign-level-to-variables
   (lambda (level variables)
     (map (lambda (v) (cons v level)) variables)))

 (define make-variable-levels
   (lambda (match)
     (define go
       (lambda (level-index level)
         (if (null? level)
             '()
             (append (assign-level-to-variables level-index (get-variable-names (car level)))
                     (go (+ level-index 1) (cdr level))))))
     (go 0 match)))

 (define expand-template*
   (lambda (template match ellipsis literals-stx)
     (expand-template template
                      match
                      0
                      (make-variable-levels match)
                      (syntax->datum ellipsis)
                      (syntax->list literals-stx)))))

(define-syntax syntax-match*
  (lambda (stx)
    (let ((form (syntax->list stx)))
      (let ((value-expr (cadr form))
            (ellipsis (caddr form))
            (literals (cadddr form))
            (clauses (cddddr form)))
        #`(let ((value #,value-expr))
            (cond
             #,@(map (lambda (clause)
                       (let ((pattern (syntax-car clause))
                             (body (syntax-cdr clause)))
                         #`((match-clause* #'#,pattern value #'#,ellipsis #'#,literals)
                            => (lambda (m)
                                 (let ((bindings (flatten-bindings m)))
                                   (let #,(make-bindings pattern #'bindings ellipsis literals)
                                     #,@body))))))
                     clauses)))))))

(define-syntax syntax-match
  (lambda (stx)
    (syntax-match* stx ... ()
      ((_ value (literals ...) clauses ...)
       #`(syntax-match* #,value ... #,literals
            #,@clauses))
      ((_ value ellipsis (literals ...) clauses ...)
       #`(syntax-match* #,value #,ellipsis #,literals
            #,@clauses)))))

(define-syntax syntax-rules
  (lambda (stx)
    (syntax-match stx ()
      ((_ (literals ...) clauses ...)
       #`(syntax-rules ... #,literals #,@clauses))
      ((_ ellipsis (literals ...) (patterns templates) ...)
       #`(lambda (value)
           (define loop
             (lambda (pattern template)
               (if (syntax-null? pattern)
                   #'(syntax-error "No matching syntax-rules clause")
                   (let ((match (match-clause* (syntax-cdr (syntax-car pattern)) (syntax-cdr value)
                                               #'#,ellipsis #'#,literals)))
                     (if match
                         (expand-template* (syntax-car template) match #'#,ellipsis #'#,literals)
                         (loop (syntax-cdr pattern) (syntax-cdr template)))))))
           (loop #'#,patterns #'#,templates)))
      (_
       #'(syntax-error "Invalid syntax-rules syntax")))))
