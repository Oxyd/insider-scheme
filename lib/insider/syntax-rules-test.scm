(library (insider syntax-rules-test))
(import (insider syntax) (insider basic-procedures) (insider test) (insider control)
        (insider list) (insider numeric) (insider vector))
(export test-syntax-transformers)

(define (strip-syntax x)
  (cond ((syntax? x)
         (strip-syntax (syntax->datum x)))
        ((pair? x)
         (cons (strip-syntax (car x)) (strip-syntax (cdr x))))
        ((vector? x)
         (let ((result (make-vector (vector-length x))))
           (do ((i 0 (+ i 1)))
               ((= i (vector-length x)) result)
             (vector-set! result i (strip-syntax (vector-ref x i))))))
        (else x)))

(define-syntax test-syntax
  (syntax-rules ()
    ((test-syntax test-name expected expr)
     (test-equal test-name expected (strip-syntax expr)))

    ((test-syntax expected expr)
     (test-equal expected (strip-syntax expr)))))

(define (syntax-match/return-type)
  (test-group "return type"
    (test-assert
     "return syntax object"
     (syntax? (syntax-match #'() () (_ #'foo))))

    (test-assert
     "return non-syntax object"
     (symbol? (syntax-match #'() () (_ 'foo))))

    (test-assert
     "return #f"
     (eq? #f (syntax-match #'() () (_ #f))))))

(define (syntax-match/constant-matchers)
  (test-group "constant matchers"
    (test-assert
     "integer constants"
     (syntax-match #'(1 2 3) ()
       ((1 2) #f)
       ((1 2 3 4) #f)
       ((1 2 3) #t)
       (_ #f)))

    (test-assert
     "string constants"
     (syntax-match #'("foo" "bar" "baz") ()
       (("foo" "quux" "baz") #f)
       (("foo" "bar") #f)
       (("foo" "bar" "baz") #t)
       (_ #f)))

    (test-assert
     "list of constants"
     (syntax-match #'(1 (2 3) 4) ()
       ((1 2 3 4) #f)
       ((1 (2) 3 4) #f)
       ((1 (2 3) 4) #t)
       (_ #f)))

    (test-assert
     "vector of constants"
     (syntax-match #'#(1 2 3) ()
       ((1 2 3) #f)
       ((#(1 2 3)) #f)
       (#(1 2) #f)
       (#(1 2 3 4) #f)
       (#(1 3 4) #f)
       (#(1 2 3) #t)
       (_ #f)))))

(define (syntax-match/underscore-matcher)
  (test-group "underscore matcher"
    (test-assert
     "underscore matches anything"
     (syntax-match #'(1 2 3) ()
       (_ #t)
       ((1 2 3) #f)))

    (test-assert
     "underscore in list"
     (syntax-match #'(1 2 3) ()
       ((1 2) #f)
       ((1 3 3) #f)
       ((1 _ 3) #t)
       (_ #f)))

    (test-assert
     "underscore matches tail of list"
     (syntax-match #'(1 2 3) ()
       ((1 . _) #t)
       (_ #f)))

    (test-assert
     "underscore in vector"
     (syntax-match #'#(1 2 3) ()
       (#(1 _ 3) #t)
       (_ #f)))))

(define (syntax-match/literal-matcher)
  (test-group "literal matcher"
    (define-syntax literal (lambda (stx) #'(syntax-error "invalid")))
    (define-syntax match-literal
      (lambda (stx)
        (syntax-match stx (literal)
          ((_ 1 literal 2) #''literal-matched)
          ((_ 1 _ 2) #''underscore-matched)
          (_ #''did-not-match))))

    (test-equal
     "match literal"
     'literal-matched
     (match-literal 1 literal 2))

    (test-equal
     "literal hygiene"
     'underscore-matched
     (let ((literal 5))
       (match-literal 1 literal 2)))

    (test-syntax
     "match underscore as a literal"
     '(1 2)
     (syntax-match #'(1 _ 2) (_)
       ((a _ b) (list a b))))))

(define (syntax-match/identifier-matchers)
  (test-group "identifier matchers"
    (test-syntax
     "match elements of list"
     '(1 2 3)
     (syntax-match #'(1 2 3) ()
       ((a b c) (list a b c))))

    (test-syntax
     "match tail of improper list"
     2
     (syntax-match #'(1 . 2) ()
       ((_ . x) x)))

    (test-syntax
     "match in nested list"
     '(1 2 3 4)
     (syntax-match #'(1 (2 3) 4) ()
       ((a (b c) d) (list a b c d))))

    (test-syntax
     "match elements of vector"
     '(1 2 3)
     (syntax-match #'#(1 2 3) ()
       (#(a b c) (list a b c))))))

(define (syntax-match/repeated-matchers)
  (test-group "repeated matchers"
    (test-syntax
     "match repeated elements in list"
     '(1 2 3)
     (syntax-match #'(1 2 3) ()
       ((x ...) x)))

    (test-syntax
     "match repeated elements in middle of list"
     '(2 3)
     (syntax-match #'(1 2 3 4) ()
       ((1 xs ... 4) xs)))

    (test-syntax
     "match front and tail of list"
     '((1 2 3) 4)
     (syntax-match #'(1 2 3 4) ()
       ((a ... b) (list a b))))

    (test-syntax
     "match beginning of a complex list"
     '((a b) c)
     (syntax-match #'((a 0) (b 1) (c 2)) ()
       (((xs _) ... (x _))
        (list xs x))))

    (test-syntax
     "match repeated sublist of multiple elements"
     '(1 3 5 2 4 6)
     (syntax-match #'((1 2) (3 4) (5 6)) ()
       (((a b) ...) (append a b))))

    (test-syntax
     "match repeated elements in improper list"
     '(1 2 3)
     (syntax-match #'(1 2 3 . 4) ()
       ((xs ... . 4) xs)))

    (test-syntax
     "nested repeated matches"
     '((1) (2 3) (4 5 6))
     (syntax-match #'((1) (2 3) (4 5 6)) ()
       (((xs ...) ...) xs)))

    (test-syntax
     "complex repeated match"
     '((1 2 4) (() (3) (5 6)))
     (syntax-match #'((1) (2 3) (4 5 6)) ()
       (((a b ...) ...) (list a b))))

    (test-syntax
     "repeated match in a vector"
     '(1 2 3)
     (syntax-match #'#(1 2 3) ()
       (#(x ...) x)))

    (test-syntax
     "different ellipsis"
     '((1 2 3) 4)
     (syntax-match #'(1 2 3 4) ::: ()
                       ((xs ::: ...) (list xs ...))))))

(define (test-syntax-match)
  (test-group "syntax-match"
    (syntax-match/return-type)
    (syntax-match/constant-matchers)
    (syntax-match/underscore-matcher)
    (syntax-match/literal-matcher)
    (syntax-match/identifier-matchers)
    (syntax-match/repeated-matchers)))

(define (syntax-rules/simple-expansion)
  (test-group "simple expansion"
    (test-equal
     "expand to constant"
     'foo
     (let-syntax ((s (syntax-rules ()
                       ((s 2) 'foo))))
       (s 2)))

    (test-equal
     "expand to bound name"
     4
     (let ((x 4))
       (let-syntax ((s (syntax-rules ()
                         ((s _) x))))
         (s 2))))

    (test-equal
     "expand to matched element"
     2
     (let-syntax ((s (syntax-rules ()
                       ((s x) x))))
       (s 2)))

    (test-equal
     "expand to matched expression"
     5
     (let-syntax ((s (syntax-rules ()
                       ((s expr)
                        expr))))
       (s (+ 2 3))))

    (test-equal
     "expand to improper list"
     '(1 2 . 3)
     (let-syntax ((s (syntax-rules ()
                       ((s a b c)
                        '(a b . c)))))
       (s 1 2 3)))

    (test-equal
     "expand to proper list tail"
     3
     (let-syntax ((s (syntax-rules ()
                       ((s . exprs)
                        (begin . exprs)))))
       (s 2 3)))

    (test-equal
     "expand to constant vector"
     #(1 2 3)
     (let-syntax ((s (syntax-rules ()
                       ((s)
                        #(1 2 3)))))
       (s)))

    (test-equal
     "expand to vector with matched elements"
     #(3 2 1)
     (let-syntax ((s (syntax-rules ()
                       ((s a b c)
                        #(c b a)))))
       (s 1 2 3)))

    (test-equal
     "macro producing macro"
     3
     (let-syntax ((s (syntax-rules ()
                       ((s . body)
                        (let-syntax ((t (syntax-rules ()
                                          ((t . body*)
                                           (begin . body*)))))
                          (t . body))))))
       (s 1 2 3)))))

(define (syntax-rules/repeated-match)
  (test-group "repeated matches"
    (test-equal
     "expand repeated simple match"
     '(1 2 3)
     (let-syntax ((s (syntax-rules ()
                       ((s xs ...)
                        (list xs ...)))))
       (s 1 2 3)))

    (test-equal
     "expand repeated complex match"
     '((1 3 5) (2 4 6))
     (let-syntax ((s (syntax-rules ()
                       ((s (a b) ...)
                        (list (list a ...) (list b ...))))))
       (s (1 2) (3 4) (5 6))))

    (test-equal
     "complex expansion of simple match"
     '((element 1) (element 2) (element 3))
     (let-syntax ((s (syntax-rules ()
                       ((s xs ...)
                        (list (list 'element xs) ...)))))
       (s 1 2 3)))

    (test-equal
     "complex expansion of complex match"
     '((2) (4 5) (7 8 9) 1 3 6)
     (let-syntax ((s (syntax-rules ()
                       ((s (a b ...) ...)
                        '((b ...) ... a ...)))))
       (s (1 2) (3 4 5) (6 7 8 9))))

    (test-equal
     "expand repeated simple match in vector template"
     #(1 2 3)
     (let-syntax ((s (syntax-rules ()
                       ((s xs ...)
                        #(xs ...)))))
       (s 1 2 3)))

    (test-equal
     "complex expansion in vector template"
     #('(element 1) '(element 2) '(element 3))
     (let-syntax ((s (syntax-rules ()
                       ((s xs ...)
                        #('(element xs) ...)))))
       (s 1 2 3)))

    (test-equal
     "multiple consecutive ellipses in template"
     '(1 2 3 4 5 6)
     (let-syntax ((s (syntax-rules ()
                       ((s (xs ...) ...)
                        '(xs ... ...)))))
       (s (1) (2 3) (4 5 6))))

    (test-equal
     "ellipsis escape 1"
     '(2 ...)
     (let-syntax ((s (syntax-rules ()
                       ((s x)
                        '(x (... ...))))))
       (s 2)))

    (test-equal
     "ellipsis escape 2"
     '(2 ...)
     (let-syntax ((s (syntax-rules ()
                       ((s x)
                        (... '(x ...))))))
       (s 2)))

    (test-equal
     "ellipsis escape with renamed ellipsis"
     '(2 :::)
     (let-syntax ((s (syntax-rules ::: ()
                       ((s x)
                        '(x (::: :::))))))
       (s 2)))

    (test-equal
     "ellipsis escaped in subtemplate"
     '((1 ...) 2 3)
     (let-syntax ((s (syntax-rules ()
                       ((s a b ...)
                        '((... (a ...)) b ...)))))
       (s 1 2 3)))

    (test-equal
     "be-like-begin 1"
     3
     (let-syntax ((be-like-begin (syntax-rules ()
                                   ((be-like-begin name)
                                    (define-syntax name
                                      (syntax-rules ()
                                        ((name expr (... ...))
                                         (begin expr (... ...)))))))))
       (be-like-begin sequence)
       (sequence 0 1 2 3)))

    (test-equal
     "be-like-begin 2"
     3
     (let-syntax ((be-like-begin (syntax-rules ()
                                   ((be-like-begin name)
                                    (define-syntax name
                                      (... (syntax-rules ()
                                             ((name expr ...)
                                              (begin expr ...)))))))))
       (be-like-begin sequence)
       (sequence 0 1 2 3)))

    (test-equal
     "literal has priority over ellipsis"
     '(100 ...)
     (let-syntax ((s (syntax-rules ... (...)
                       ((_ x)
                        '(x ...)))))
       (s 100)))

    (test-equal
     "reverse a list backward"
     '(3 2 1)
     (letrec-syntax ((s (syntax-rules ()
                          ((s () (ys ...))
                           (list ys ...))

                          ((s (xs ... x) (ys ...))
                           (s (xs ...) (ys ... x))))))
       (s (1 2 3) ())))

    (test-equal
     "match beginning of a complex list"
     '(a b c)
     (let-syntax ((s (syntax-rules ()
                       ((clause ((xs _) ... (x _)))
                        (list 'xs ... 'x)))))
       (s ((a 0) (b 1) (c 2)))))))

(define (syntax-rules/hygiene)
  (test-group "hygiene"
    (test-equal
     "change meaning of identifier used in macro definition"
     'now
     (let-syntax ((when (syntax-rules ()
                          ((when test stmt1 stmt2 ...)
                           (if test (begin stmt1 stmt2 ...))))))
       (let ((if #t))
         (when if (set! if 'now))
         if)))

    (test-equal
     "change meaning of identifier used in macro output"
     'outer
     (let ((x 'outer))
       (let-syntax ((m (syntax-rules () ((m) x))))
         (let ((x 'inner))
           (m)))))

    (test-equal
     "recursive syntax"
     7
     (letrec-syntax ((my-or (syntax-rules ()
                              ((my-or) #f)
                              ((my-or e) e)
                              ((my-or e1 e2 ...)
                               (let ((temp e1))
                                 (if temp
                                     temp
                                     (my-or e2 ...)))))))
       (let ((x #f)
             (y 7)
             (temp 8)
             (let odd?)
             (if even?))
         (my-or x
                (let temp)
                (if y)
                y))))

    (test-equal
     "expand to introduced definition"
     42
     (let-syntax ((jabberwocky (syntax-rules ()
                                 ((_ hatter)
                                  (begin
                                    (define march-hare 42)
                                    (define-syntax hatter
                                      (syntax-rules ()
                                        ((_) march-hare))))))))
       (jabberwocky mad-hatter)
       (mad-hatter)))

    (test-equal
     "expand to quasiquote referencing a variable defined outside the transformer"
     '0
     (let-syntax ((s (syntax-rules ()
                       ((s)
                        (let ((value 0))
                          (let-syntax ((t (syntax-rules ()
                                            ((t) `,value))))
                            (t)))))))
       (s)))))

(define (test-syntax-rules)
  (test-group "syntax-rules"
    (syntax-rules/simple-expansion)
    (syntax-rules/repeated-match)
    (syntax-rules/hygiene)))

(define (test-syntax-transformers)
  (test-group "syntax transformers"
    (test-syntax-match)
    (test-syntax-rules)))

(when-main-module
 (test-syntax-transformers))

;; Local variables:
;; eval: (put 'syntax-match 'scheme-indent-function 2)
;; eval: (put 'test-group 'scheme-indent-function 1)
;; End:
