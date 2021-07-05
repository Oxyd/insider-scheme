(library (insider syntax))
(import (insider base-scheme))
(export syntax-match syntax-rules)

(begin-for-syntax
 (define (merge-level! x-alist y-alist)
   (let loop ((x x-alist))
     (if (null? x)
         y-alist
         (cond ((assq (caar x) y-alist)
                => (lambda (y-binding)
                     (set-cdr! y-binding (append (cdar x) (cdr y-binding)))
                     (loop (cdr x))))
               (else
                (set! y-alist (cons (car x) y-alist))
                (loop (cdr x)))))))

 (define (merge-bindings! xs ys)
   (let loop ((x xs) (y ys) (accum '()))
     (cond ((and (null? x) (null? y))
            (reverse accum))
           ((null? x)
            (loop x (cdr y) (cons (car y) accum)))
           ((null? y)
            (loop (cdr x) y (cons (car x) accum)))
           (else
            (loop (cdr x) (cdr y) (cons (merge-level! (car x) (car y)) accum))))))

 (define (shift-bindings bindings)
   (cons '()
         (map (lambda (b)
                (map (lambda (binding)
                       (let ((key (car binding)) (value (cdr binding)))
                         `(,key . (,value))))
                     b))
              bindings)))

 (define (reverse-improper-syntax-list lst)
   (let loop ((e lst) (accum '()))
     (cond ((syntax-pair? e)
            (loop (syntax-cdr e) (cons (syntax-car e) accum)))
           (else
            (cons accum e)))))

 (define (bind value continuation)
   (if value
       (continuation value)
       #f))

 (define (match-tail/list pattern expression ellipsis literals)
   (let ((pattern* (reverse-improper-syntax-list pattern))
         (expression* (reverse-improper-syntax-list expression)))
     (bind (match-clause (cdr pattern*) (cdr expression*) ellipsis literals)
           (lambda (tail-match)
             (let loop ((pat (car pattern*)) (expr (car expression*))
                        (accum tail-match))
               (cond ((null? pat)
                      (cons accum (length expr)))
                     ((null? expr)
                      #f)
                     (else
                      (bind (match-clause (syntax-car pat) (syntax-car expr) ellipsis literals)
                            (lambda (m)
                              (loop (syntax-cdr pat) (syntax-cdr expr) (merge-bindings! accum m)))))))))))

 (define (syntax-pair? x)
   (or (pair? x)
       (and (syntax? x)
            (pair? (syntax-expression x)))))

 (define (syntax-vector? x)
   (and (syntax? x) (vector? (syntax-expression x))))

 (define (ellipsis? id ellipsis)
   (and ellipsis
        (syntax? id)
        (eq? (syntax-expression id) ellipsis)))

 (define (underscore? id)
   (and (syntax? id)
        (eq? (syntax-expression id) '_)))

 (define (literal? id literals)
   (and (identifier? id)
        (member id literals free-identifier=?)))

 (define (followed-by-ellipsis? x ellipsis)
   (and (syntax-pair? (syntax-cdr x)) (ellipsis? (syntax-cadr x) ellipsis)))

 (define (first-level-pattern-variables pattern ellipsis)
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
            (let loop ((i 0) (accum '()))
              (cond ((>= i (vector-length v))
                     accum)
                    ((and (< (+ i 1) (vector-length v))
                          (ellipsis? (vector-ref v (+ i 1)) ellipsis))
                     (loop (+ i 2) accum))
                    (else
                     (loop (+ i 1) (set-union (first-level-pattern-variables (vector-ref v i) ellipsis)
                                              accum
                                              eq?)))))))
         (else
          '())))

 (define (make-empty-bindings pattern ellipsis)
   (cons '()
         (list
          (map (lambda (name) `(,name . ()))
               (first-level-pattern-variables pattern ellipsis)))))

 (define (match-repeatedly/list pattern expression num-elems ellipsis literals)
   (let loop ((e expression)
              (accum (make-empty-bindings pattern ellipsis))
              (i num-elems))
     (if (= i 0)
         accum
         (bind (match-clause pattern (syntax-car e) ellipsis literals)
               (lambda (m)
                 (loop (syntax-cdr e) (merge-bindings! accum (shift-bindings m)) (- i 1)))))))

 (define (match-repeatedly/vector pattern expression begin end ellipsis literals)
   (let loop ((i begin)
              (accum (make-empty-bindings pattern ellipsis)))
     (if (= i end)
         accum
         (bind (match-clause pattern (vector-ref expression i) ellipsis literals)
               (lambda (m)
                 (loop (+ i 1) (merge-bindings! accum (shift-bindings m))))))))

 (define (match-clause pattern expression ellipsis literals)
   (cond
    ((literal? pattern literals)
     (if (and (identifier? expression)
              (free-identifier=? pattern expression))
         '()
         #f))
    ((underscore? pattern)
     '())
    ((identifier? pattern)
     `(((,(syntax-expression pattern) . ,expression))))
    ((syntax-pair? pattern)
     (cond
      ((followed-by-ellipsis? pattern ellipsis)
       (bind (match-tail/list (syntax-cddr pattern) expression ellipsis literals)
             (lambda (tail-result)
               (let ((tail-match (car tail-result))
                     (num-unmatched (cdr tail-result)))
                 (bind (match-repeatedly/list (syntax-car pattern) expression num-unmatched ellipsis literals)
                       (lambda (repeated-match)
                         (merge-bindings! repeated-match tail-match)))))))
      ((syntax-pair? expression)
       (let ((car-match (match-clause (syntax-car pattern) (syntax-car expression) ellipsis literals))
             (cdr-match (match-clause (syntax-cdr pattern) (syntax-cdr expression) ellipsis literals)))
         (if (and car-match cdr-match)
             (merge-bindings! car-match cdr-match)
             #f)))
      (else #f)))
    ((and (syntax-vector? pattern) (syntax-vector? expression))
     (let ((pattern* (unwrap-syntax pattern))
           (expression* (unwrap-syntax expression)))
       (let loop ((i 0) (j 0) (accum '()))
         (if (= i (vector-length pattern*))
             accum
             (let ((next (+ i 1)))
               (cond ((and (< next (vector-length pattern*)) (ellipsis? (vector-ref pattern* next) ellipsis))
                      (let* ((repeated-length (+ 1 (- (vector-length expression*) (vector-length pattern*))))
                             (repeated-end (+ j repeated-length 1)))
                        (bind (match-repeatedly/vector (vector-ref pattern* i) expression* j repeated-end ellipsis literals)
                              (lambda (repeated-match)
                                (loop (+ next 1) repeated-end (merge-bindings! accum repeated-match))))))
                     ((>= j (vector-length expression*))
                      #f)
                     (else
                      (bind (match-clause (vector-ref pattern* i) (vector-ref expression* j) ellipsis literals)
                            (lambda (m)
                              (loop next (+ j 1) (merge-bindings! accum m)))))))))))
    ((equal? (unwrap-syntax pattern) (unwrap-syntax expression))
     '())
    (else
     #f)))

 (define (match-clause* pattern expression ellipsis-stx literals-stx)
   (match-clause pattern expression (syntax->datum ellipsis-stx) (syntax->list literals-stx)))

 (define (set-union x y compare)
   (let loop ((elem y) (accum x))
     (cond ((null? elem) accum)
           ((member (car elem) accum compare)
            (loop (cdr elem) accum))
           (else
            (loop (cdr elem) (cons (car elem) accum))))))

 (define (all-pattern-ids pattern ellipsis literals)
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
            (let loop ((i 0) (accum '()))
              (cond ((>= i (vector-length v))
                     accum)
                    ((ellipsis? (vector-ref v i) ellipsis)
                     (loop (+ i 1) accum))
                    (else
                     (loop (+ i 1) (set-union accum
                                              (all-pattern-ids (vector-ref v i) ellipsis literals)
                                              bound-identifier=?)))))))
         (else
          '())))

 (define (all-template-variables template ellipsis literals)
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
            (let loop ((i 0) (accum '()))
              (cond ((>= i (vector-length v))
                     accum)
                    ((ellipsis? (vector-ref v i) ellipsis)
                     (loop (+ i 1) accum))
                    (else
                     (loop (+ i 1) (set-union accum
                                              (all-template-variables (vector-ref v i) ellipsis literals)
                                              eq?)))))))
         (else
          '())))

 (define (flatten-bindings bindings)
   (let loop ((accum '())
              (level bindings))
     (if (null? level)
         accum
         (let inner-loop ((accum accum) (binding (car level)))
           (if (null? binding)
               (loop accum (cdr level))
               (inner-loop (cons (car binding) accum) (cdr binding)))))))

 (define (make-bindings pattern match ellipsis literals)
   (let ((ids (all-pattern-ids pattern (syntax->datum ellipsis) (syntax->list literals))))
     (let loop ((accum '()) (id ids))
       (if (null? id)
           accum
           (loop #`((#,(car id) (cdr (assq '#,(syntax-expression (car id)) #,match))) . #,accum)
                 (cdr id))))))

 (define (filter-match match variables)
   (map (lambda (level)
          (filter (lambda (binding)
                    (memq (car binding) variables))
                  level))
        match))

 (define (match-level-null? level)
   (any (lambda (binding)
          (null? (cdr binding)))
        level))

 (define (match-null? match)
   (any match-level-null? match))

 (define (match-car match)
   (map (lambda (level)
          (map (lambda (binding)
                 (cons (car binding) (cadr binding)))
               level))
        match))

 (define (match-cdr match)
   (map (lambda (level)
          (map (lambda (binding)
                 (cons (car binding) (cddr binding)))
               level))
        match))

 (define (expand-repeatedly/list template match current-level variable-levels ellipsis literals)
   (let ((vars (all-template-variables template ellipsis literals)))
     (let loop ((m (filter-match match vars))
                (accum '()))
       (cond ((match-null? m)
              (reverse accum))
             (else
              (loop (match-cdr m)
                    (cons (expand-template template (match-car m) current-level variable-levels ellipsis literals)
                          accum)))))))

 (define (wrap-expansion template expansion)
   (if (syntax? template)
       (datum->syntax template expansion)
       expansion))

 (define (car* x)
   (if (null? x)
       '()
       (car x)))

 (define (cdr* x)
   (if (null? x)
       '()
       (cdr x)))

 (define (find-binding-at-current-match-level identifier match)
   (assq identifier (car* match)))

 (define (expand-template template match current-level variable-levels ellipsis literals)
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
                        (when (> (cdr variable-level) current-level)
                          (error "Not enough ellipses following template" id))
                        (when (< (cdr variable-level) current-level)
                          (error "Too many ellipses following template" id))
                        (cdr (assq id (car* match)))))
                  (else template))))
         (else template)))

 (define (get-variable-names level)
   (map car level))

 (define (assign-level-to-variables level variables)
   (map (lambda (v) (cons v level)) variables))

 (define (make-variable-levels match)
   (define (go level-index level)
     (if (null? level)
         '()
         (append (assign-level-to-variables level-index (get-variable-names (car level)))
                 (go (+ level-index 1) (cdr level)))))
   (go 0 match))

 (define (expand-template* template match ellipsis literals-stx)
   (expand-template template
                    match
                    0
                    (make-variable-levels match)
                    (syntax->datum ellipsis)
                    (syntax->list literals-stx))))

(define-syntax syntax-match*
  (lambda (stx)
    (let* ((form (syntax->list stx))
           (value-expr (cadr form))
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
                   clauses))))))

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
           (let loop ((pattern #'#,patterns) (template #'#,templates))
             (if (syntax-null? pattern)
                 #'(syntax-error "No matching syntax-rules clause")
                 (let ((match (match-clause* (syntax-cdr (syntax-car pattern)) (syntax-cdr value)
                                             #'#,ellipsis #'#,literals)))
                   (if match
                       (expand-template* (syntax-car template) match #'#,ellipsis #'#,literals)
                       (loop (syntax-cdr pattern) (syntax-cdr template))))))))
      (_
       #'(syntax-error "Invalid syntax-rules syntax")))))
