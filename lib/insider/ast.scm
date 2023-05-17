(define (expression? x)
  (or (literal-expression? x)
      (local-reference-expression? x)
      (top-level-reference-expression? x)
      (unknown-reference-expression? x)
      (application-expression? x)
      (sequence-expression? x)
      (let-expression? x)
      (local-set!-expression? x)
      (top-level-set!-expression? x)
      (lambda-expression? x)
      (if-expression? x)))

(define (variable? x)
  (or (local-variable? x) (top-level-variable? x)))

(define (indent depth)
  (make-string (* 2 depth) #\space))

(define (print-element depth name . header-elems)
  (display (indent depth))
  (display name)
  (display ": ")
  (display (string-join header-elems " "))
  (newline))

(define (print-subelements depth subelems)
  (for-each (lambda (subelem)
              (let ((name (car subelem)) (expr (cdr subelem)))
                (print-element (+ depth 1) name)
                (pretty-print-expression-or-expression-list (+ depth 2) expr)))
            subelems))

(define (print-literal-expression depth expr)
  (print-element depth "literal"
                 (datum->string (literal-expression-value expr))))

(define (print-local-reference-expression depth expr)
  (print-element depth "local-reference"
                      (local-variable-name
                       (local-reference-expression-variable expr))))

(define (print-top-level-reference-expression depth expr)
  (print-element depth "top-level-reference"
                      (top-level-variable-name
                       (top-level-reference-expression-variable expr))))

(define (print-unknown-reference-expression depth expr)
  (print-element depth "unknown-reference"
                 (datum->string (unknown-reference-expression-name expr))))

(define (print-application-expression depth expr)
  (print-element depth "application")
  (print-subelements
   depth
   `(("target"    . ,(application-expression-target expr))
     ("arguments" . ,(application-expression-arguments expr)))))

(define (print-sequence-expression depth expr)
  (print-element depth "sequence")
  (pretty-print-expression-or-expression-list
   (+ depth 1)
   (sequence-expression-expressions expr)))

(define (print-let-definition-pairs depth dps)
  (for-each (lambda (pair)
              (let ((var (car pair)) (expr (cdr pair)))
                (print-element depth (local-variable-name var))
                (pretty-print-expression (+ depth 1) expr)))
            dps))

(define (print-let-expression depth expr)
  (print-element depth "let")
  (print-element (+ depth 1) "definitions")
  (print-let-definition-pairs (+ depth 2) (let-expression-definitions expr))
  (print-element (+ depth 1) "body")
  (pretty-print-expression (+ depth 2) (let-expression-body expr)))

(define (print-local-set!-expression depth expr)
  (print-element depth "local-set!"
                 (local-variable-name (local-set!-expression-target expr)))
  (pretty-print-expression (+ depth 1) (local-set!-expression-expression expr)))

(define (print-top-level-set!-expression depth expr)
  (print-element depth "top-level-set!"
                 (top-level-variable-name
                  (top-level-set!-expression-target expr)))
  (pretty-print-expression (+ depth 1)
                           (top-level-set!-expression-expression expr)))

(define (print-lambda-expression depth expr)
  (print-element depth "lambda"
                 (lambda-expression-name expr)
                 (string-append
                  "("
                  (string-join (map local-variable-name
                                    (lambda-expression-parameters expr))
                               " ")
                  ")"))
  (pretty-print-expression (+ depth 1) (lambda-expression-body expr)))

(define (print-if-expression depth expr)
  (print-element depth "if")
  (print-subelements depth
                     `(("test"        . ,(if-expression-test expr))
                       ("consequent"  . ,(if-expression-consequent expr))
                       ("alternative" . ,(if-expression-alternative expr)))))

(define expression-printers
  `((,literal-expression?             . ,print-literal-expression)
    (,local-reference-expression?     . ,print-local-reference-expression)
    (,top-level-reference-expression? . ,print-top-level-reference-expression)
    (,unknown-reference-expression?   . ,print-unknown-reference-expression)
    (,application-expression?         . ,print-application-expression)
    (,sequence-expression?            . ,print-sequence-expression)
    (,let-expression?                 . ,print-let-expression)
    (,local-set!-expression?          . ,print-local-set!-expression)
    (,top-level-set!-expression?      . ,print-top-level-set!-expression)
    (,lambda-expression?              . ,print-lambda-expression)
    (,if-expression?                  . ,print-if-expression)))

(define (expression-printer expr)
  (let loop ((p expression-printers))
    (if (null? p)
        (error "Invalid expression type" expr)
        (let ((predicate (caar p)) (printer (cdar p)))
          (if (predicate expr)
              printer
              (loop (cdr p)))))))

(define (pretty-print-expression depth expr)
  ((expression-printer expr) depth expr))

(define (pretty-print-expression-or-expression-list depth expr-or-list)
  (if (expression? expr-or-list)
      (pretty-print-expression depth expr-or-list)
      (for-each (lambda (e) (pretty-print-expression depth e)) expr-or-list)))

(define (pretty-print-ast expr)
  (pretty-print-expression 0 expr))
