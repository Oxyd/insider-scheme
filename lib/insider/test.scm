(library (insider test))
(import (insider syntax) (insider basic-procedures) (insider list)
        (insider record) (insider control) (insider error) (insider io)
        (insider numeric) (insider string) (scheme case-lambda)
        (only (insider internal) string-append datum->string number->string))
(export
 test-runner?
 test-runner-on-test-begin test-runner-on-test-begin!
 test-runner-on-test-end test-runner-on-test-end!
 test-runner-on-group-begin test-runner-on-group-begin!
 test-runner-on-group-end test-runner-on-group-end!
 test-runner-on-bad-count test-runner-on-bad-count!
 test-runner-on-bad-end-name test-runner-on-bad-end-name!
 test-runner-on-final test-runner-on-final!
 test-result-alist
 test-runner-aux-value test-runner-aux-value!
 test-runner-pass-count
 test-runner-fail-count
 test-runner-xpass-count
 test-runner-xfail-count
 test-runner-skip-count
 test-runner-test-name

 test-runner-reset
 test-runner-group-stack
 test-runner-group-path

 test-result-clear test-result-ref test-result-set! test-result-kind

 test-on-test-begin-simple test-on-test-end-simple
 test-on-group-begin-simple test-on-group-end-simple
 test-on-bad-count-simple test-on-bad-end-name-simple
 test-on-final-simple

 test-match-name test-match-nth test-match-any test-match-all
 test-skip test-expect-fail

 test-runner-current test-runner-factory test-runner-get
 test-runner-null test-runner-simple test-runner-create

 test-begin test-end test-group test-group-with-cleanup
 test-assert test-eqv test-equal test-eq test-approximate test-error test-values-equal

 test-apply test-with-runner

 ;; Non-standard extensions:
 test test-false)

(define-record-type <test-runner>
  (make-test-runner on-begin on-end on-group-begin on-group-end on-bad-count on-bad-end-name on-final
                    result-alist current-groups test-name
                    pass-count fail-count xpass-count xfail-count skip-count
                    deinstall-automatically? skip-specifiers expect-fail-specifiers run-specifiers
                    failed-tests)
  test-runner?

  ;; Callbacks:
  (on-begin test-runner-on-test-begin test-runner-on-test-begin!)
  (on-end test-runner-on-test-end test-runner-on-test-end!)
  (on-group-begin test-runner-on-group-begin test-runner-on-group-begin!)
  (on-group-end test-runner-on-group-end test-runner-on-group-end!)
  (on-bad-count test-runner-on-bad-count test-runner-on-bad-count!)
  (on-bad-end-name test-runner-on-bad-end-name test-runner-on-bad-end-name!)
  (on-final test-runner-on-final test-runner-on-final!)

  ;; Runner state.
  (result-alist test-result-alist test-result-alist!)
  (current-groups test-runner-current-groups test-runner-current-groups!)
  (test-name test-runner-test-name test-runner-test-name!)
  (pass-count test-runner-pass-count test-runner-pass-count!)
  (fail-count test-runner-fail-count test-runner-fail-count!)
  (xpass-count test-runner-xpass-count test-runner-xpass-count!)
  (xfail-count test-runner-xfail-count test-runner-xfail-count!)
  (skip-count test-runner-skip-count test-runner-skip-count!)
  (aux test-runner-aux-value test-runner-aux-value!)
  (deinstall-automatically? test-runner-deinstall-automatically? test-runner-deinstall-automatically!)
  (skip-specifiers test-runner-skip-specifiers test-runner-skip-specifiers!)
  (expect-fail-specifiers test-runner-expect-fail-specifiers test-runner-expect-fail-specifiers!)
  (run-specifiers test-runner-run-specifiers test-runner-run-specifiers!)
  (failed-tests test-runner-failed-tests test-runner-failed-tests!))

(define-record-type <test-group>
  (make-test-group name expected-count actual-count skip-specifiers)
  test-group?
  (name test-group-name)
  (expected-count test-group-expected-count)
  (actual-count test-group-actual-count test-group-actual-count!)
  (skip-specifiers test-group-skip-specifiers))

(define (test-runner-get)
  (cond ((test-runner-current) => values)
        (else (error "No test runner installed"))))

(define (no-op . _) #void)

(define (test-runner-null)
  (make-test-runner no-op no-op no-op no-op no-op no-op no-op
                    (list) (list) "" 0 0 0 0 0 #f (list) (list) (list) (list)))

(define (test-runner-simple)
  (make-test-runner test-on-test-begin-simple test-on-test-end-simple
                    test-on-group-begin-simple test-on-group-end-simple
                    test-on-bad-count-simple test-on-bad-end-name-simple test-on-final-simple
                    (list) (list) "" 0 0 0 0 0 #f (list) (list) (list) (list)))

(define test-runner-current (make-parameter #f))
(define test-runner-factory (make-parameter test-runner-simple))

(define (test-runner-create)
  ((test-runner-factory)))

(define (test-runner-install!)
  (test-runner-current (test-runner-create)))

(define (test-runner-deinstall!)
  (test-runner-current #f))

(define (inc-field getter setter)
  (lambda (record)
    (setter record (+ (getter record) 1))))

(define inc-pass-count! (inc-field test-runner-pass-count test-runner-pass-count!))
(define inc-fail-count! (inc-field test-runner-fail-count test-runner-fail-count!))
(define inc-xpass-count! (inc-field test-runner-xpass-count test-runner-xpass-count!))
(define inc-xfail-count! (inc-field test-runner-xfail-count test-runner-xfail-count!))
(define inc-skip-count! (inc-field test-runner-skip-count test-runner-skip-count!))

(define inc-group-run-count! (inc-field test-group-actual-count test-group-actual-count!))

(define (test-name-for-display runner)
  (let ((name (test-runner-test-name runner)))
    (if (equal? name "")
        (datum->string (test-result-ref runner 'source-form))
        name)))

(define (test-on-test-begin-simple runner)
  (display/colour
   'green
   (case (test-result-kind runner)
     ((skip) "[ SKIP     ] ")
     (else   "[ RUN      ] ")))
  (display (test-name-for-display runner))
  (newline))

(define (fail-reason-comparison runner)
  (let ((expected (test-result-ref runner 'expected-value))
        (actual (test-result-ref runner 'actual-value)))
    (if (and expected actual)
        (string-append "Expected: " (datum->string expected)
                       "; actual: " (datum->string actual))
        "Result is false")))

(define (fail-reason-unexpected-exception runner)
  (string-append "Unexpected error: "
                 (let ((e (test-result-ref runner 'actual-error)))
                   (if (error? e)
                       (error-object-message e)
                       (datum->string e)))))

(define (fail-reason runner)
  (case (test-result-ref runner 'result)
    ((#f fail) (fail-reason-comparison runner))
    ((unexpected-exception) (fail-reason-unexpected-exception runner))
    ((incorrect-exception) "Expression resulted in a different exception than expected")
    ((no-exception) "Expression was expected to raise an exception but didn't")
    (else => (lambda (result)
               (error "Unknown test result" (datum->string result))))))

(define (test-on-test-end-simple runner)
  (case (test-result-kind runner)
    ((pass)
     (display/colour 'green "[       OK ]\n"))
    ((fail)
     (display/colour 'red "[     FAIL ] ")
     (display (fail-reason runner))
     (newline))
    ((xpass)
     (display/colour 'red "[     FAIL ] ")
     (display "Test was expected to fail, but it passed\n"))
    ((xfail)
     (display/colour 'green "[  OK FAIL ] ")
     (display "Test was expected to fail, and did\n"))
    ((skip)
     #void)))

(define (test-on-group-begin-simple runner suite-name count)
  (display/colour 'green "[----------] ")
  (display "Begin ")
  (display suite-name)
  (newline))

(define (test-on-group-end-simple runner)
  (let ((group (current-test-group runner)))
    (display/colour 'green "[----------] ")
    (display "End ")
    (display (test-group-name group))
    (display ": ran ")
    (display/plural (test-group-actual-count group) "test" "tests")
    (newline)))

(define (test-on-bad-count-simple runner actual-count expected-count)
  (display/colour 'red "[ BAD COUNT] ")
  (display "Expected group to run ")
  (display/plural expected-count "test" "tests")
  (display ", but actually ran ")
  (display/plural actual-count "test" "tests")
  (newline))

(define (test-on-bad-end-name-simple runner begin-name end-name)
  (display/colour 'red "[ BAD NAME ] ")
  (display (string-append "Name given to test-begin ("
                          (datum->string begin-name)
                          ") does not match name given to test-end ("
                          (datum->string end-name)
                          ")\n")))

(define (display/plural value singular plural)
  (display value)
  (display " ")
  (display (if (= value 1) singular plural)))

(define (test-on-final-simple runner)
  (define (show-optional value name)
    (when (> value 0)
      (display ", ")
      (display/plural value "test" "tests")
      (display " ")
      (display name)))

  (display "Summary: ")
  (display/plural (test-runner-pass-count runner) "test" "tests")
  (display " passed, ")
  (display/plural (test-runner-fail-count runner) "test" "tests")
  (display " failed")
  (show-optional (test-runner-xfail-count runner) "expected to fail")
  (show-optional (test-runner-xpass-count runner) "expected to fail but didn't")
  (show-optional (test-runner-skip-count runner) "skipped")
  (display ".\n")

  (let ((failed (reverse (test-runner-failed-tests runner))))
    (unless (null? failed)
      (display "Failed tests:\n")
      (do ((failed failed (cdr failed)))
          ((null? failed))
        (display " * ")
        (display (car failed))
        (newline)))))

(define (test-runner-reset runner)
  (test-result-clear runner))

(define (test-runner-group-stack runner)
  (map test-group-name (test-runner-current-groups runner)))

(define (test-runner-group-path runner)
  (reverse (test-runner-group-stack runner)))

(define (test-result-clear runner)
  (test-result-alist! runner (list)))

(define test-result-ref
  (case-lambda
   ((runner pname)
    (test-result-ref runner pname #f))
   ((runner pname default)
    (let ((alist (test-result-alist runner)))
      (cond ((assq pname alist) => cdr)
            (else default))))))

(define (test-result-set! runner pname value)
  (cond ((assq pname (test-result-alist runner))
         => (lambda (pair)
              (set-cdr! pair value)))
        (else
         (test-result-alist! runner (cons (cons pname value) (test-result-alist runner))))))

(define (test-result-kind runner)
  (test-result-ref runner 'result-kind))

(define (call f runner . args)
  (apply (f runner) runner args))

(define (push-test-group! runner group-name expected-count)
  (test-runner-current-groups! runner
                               (cons (make-test-group group-name
                                                      expected-count 0
                                                      (test-runner-skip-specifiers runner))
                                     (test-runner-current-groups runner))))

(define (current-test-group runner)
  (let ((stack (test-runner-current-groups runner)))
    (cond ((null? stack) #f)
          (else (car stack)))))

(define (pop-test-group! runner)
  (let ((group-stack (test-runner-current-groups runner)))
    (when (null? group-stack)
      (error "Group stack is empty"))
    (test-runner-current-groups! runner (cdr group-stack))
    (test-runner-skip-specifiers! runner (test-group-skip-specifiers (car group-stack)))))

(define (enter-test-group! runner suite-name count)
  (increase-run-tests-in-group-count! runner)
  (push-test-group! runner suite-name count)
  (test-runner-test-name! runner suite-name)
  (call test-runner-on-group-begin runner suite-name count))

(define test-begin
  (case-lambda
   ((suite-name)
    (test-begin suite-name #f))

   ((suite-name count)
    (unless (test-runner-current)
      (test-runner-install!)
      (test-runner-deinstall-automatically! (test-runner-current) #t))
    (enter-test-group! (test-runner-get) suite-name count))))

(define (check-group-test-count! runner)
  (let ((group (current-test-group runner)))
    (let ((expected (test-group-expected-count group))
          (actual (test-group-actual-count group)))
      (when (and expected (not (= expected actual)))
        (call test-runner-on-bad-count runner actual expected)))))

(define (check-group-end-name! runner end-name)
  (let* ((group (current-test-group runner))
         (begin-name (test-group-name group)))
    (unless (or (not end-name) (equal? begin-name end-name))
      (call test-runner-on-bad-end-name runner begin-name end-name))))

(define (maybe-deinstall-test-runner! runner)
  (when (and (null? (test-runner-current-groups runner))
             (test-runner-deinstall-automatically? runner))
    (call test-runner-on-final runner)
    (test-runner-deinstall!)))

(define test-end
  (case-lambda
   (()
    (test-end #f))
   ((suite-name)
    (let ((runner (test-runner-get)))
      (check-group-test-count! runner)
      (check-group-end-name! runner suite-name)
      (call test-runner-on-group-end runner)
      (pop-test-group! runner)
      (maybe-deinstall-test-runner! runner)))))

(define (skip-test-group! runner)
  (inc-skip-count! runner))

(define (execute-test-group! thunk)
  (let ((runner (test-runner-get)))
    (if (skip? runner)
        (skip-test-group! runner)
        (thunk))))

(define-syntax test-group
  (syntax-rules ()
    ((test-group name expr0 expr ...)
     (dynamic-wind
       (lambda () (test-begin name))
       (lambda () (execute-test-group! (lambda () expr0 expr ...)))
       (lambda () (test-end name))))))

(define-syntax test-group-with-cleanup
  (syntax-rules ()
    ((test-group-with-cleanup name expr0 expr ... cleanup-form)
     (dynamic-wind
       (lambda () (test-begin name))
       (lambda () (execute-test-group! (lambda () expr0 expr ...)))
       (lambda () (test-end name) cleanup-form)))))

(define (test-case-result->result-kind result)
  (case result
    ((#t pass) 'pass)
    ((#f fail unexpected-exception incorrect-exception no-exception) 'fail)
    (else => values)))

(define (invert-result result)
  (case result
    ((#t pass) 'xpass)
    ((skip) 'skip)
    (else 'xfail)))

(define (compare-test-result-with-expected result expected)
  (case expected
    ((pass) result)
    (else (invert-result result))))

(define (test-path->string runner)
  (define (group-path->string groups)
    (cond ((null? groups)
           "")
          (else
           (string-append (car groups) "/" (group-path->string (cdr groups))))))
  (string-append (group-path->string (test-runner-group-path runner))
                 (test-name-for-display runner)))

(define (record-failed-test! runner)
  (test-runner-failed-tests! runner (cons (test-path->string runner)
                                          (test-runner-failed-tests runner))))

(define (record-result! runner result-kind)
  (case result-kind
    ((pass) (inc-pass-count! runner))
    ((fail) (inc-fail-count! runner) (record-failed-test! runner))
    ((xpass) (inc-xpass-count! runner) (record-failed-test! runner))
    ((xfail) (inc-xfail-count! runner))
    ((skip) (inc-skip-count! runner))
    (else (error "Invalid result-kind" result-kind))))

(define (increase-run-tests-in-group-count! runner)
  (cond ((current-test-group runner) => inc-group-run-count!)))

(define (run-test-case/catch-exceptions runner test-case)
  (guard (e (else
             (test-result-set! runner 'actual-error e)
             'unexpected-exception))
    (test-case)))

(define (run-test-case! runner test-case)
  (call test-runner-on-test-begin runner)
  (let* ((expected (expected-result runner))
         (result* (run-test-case/catch-exceptions runner test-case))
         (result (compare-test-result-with-expected result* expected))
         (result-kind (test-case-result->result-kind result)))
    (test-result-set! runner 'result result)
    (test-result-set! runner 'result-kind result-kind)
    (record-result! runner result-kind)
    (call test-runner-on-test-end runner)))

(define (skip-test-case! runner)
  (test-result-set! runner 'result 'skip)
  (test-result-set! runner 'result-kind 'skip)
  (call test-runner-on-test-begin runner)
  (record-result! runner 'skip)
  (call test-runner-on-test-end runner))

(define (execute-test-case! runner test-case name source-form)
  (test-result-clear runner)
  (test-runner-test-name! runner name)
  (test-result-set! runner 'source-form source-form)
  (increase-run-tests-in-group-count! runner)
  (if (skip? runner)
      (skip-test-case! runner)
      (run-test-case! runner test-case)))

(define (make-display-test-name stx expr)
  (let ((loc (syntax-location stx)))
    (let ((file-name (car loc)) (column (cadr loc)))
      (string-append file-name ":" (number->string column) ": " (datum->string expr)))))

(define-syntax test-assert
  (syntax-rules ()
    ((test-assert name expr)
     (execute-test-case! (test-runner-get) (lambda () expr) name 'expr))
    ((test-assert expr)
     (test-assert "" expr))))

(define-syntax test
  (syntax-rules ()
    ((test . args)
     (test-assert . args))))

(define-syntax test-false
  (syntax-rules ()
    ((test-false name expr)
     (test-assert name (not expr)))

    ((test-false expr)
     (test-assert "" (not expr)))))

(define-syntax define-test-syntax
  (syntax-rules ()
    ((define-test-syntax name predicate)
     (define-syntax name
       (syntax-rules ()
         ((name test-name expected expr)
          (execute-test-case! (test-runner-get)
                              (lambda ()
                                (let ((runner (test-runner-get))
                                      (expected-value expected)
                                      (actual-value expr))
                                  (test-result-set! runner 'expected-value expected-value)
                                  (test-result-set! runner 'actual-value actual-value)
                                  (predicate expected-value actual-value)))
                              test-name
                              '(predicate expected expr)))
         ((name expected expr)
          (name "" expected expr)))))))

(define-test-syntax test-eqv eqv?)
(define-test-syntax test-equal equal?)
(define-test-syntax test-eq eq?)

(define (values-equal? x y)
  (call-with-values (lambda () x)
    (lambda x-list
      (call-with-values (lambda () y)
        (lambda y-list
          (equal? x-list y-list))))))

(define-syntax test-values-equal
  (syntax-rules ()
    ((test-values-equal test-name (expected ...) expr)
     (execute-test-case! (test-runner-get)
                         (lambda ()
                           (let ((runner (test-runner-get))
                                 (expected-value (values expected ...))
                                 (actual-value expr))
                             (test-result-set! runner 'expected-value expected-value)
                             (test-result-set! runner 'actual-value actual-value)
                             (values-equal? expected-value actual-value)))
                         test-name
                         '(values-equal? (expected ...) expr)))

    ((test-values-equal (expected ...) expr)
     (test-values-equal "" (expected ...) expr))))

(define-syntax test-approximate
  (syntax-rules ()
    ((test-approximate name expected expr error)
     (let ((value expr))
       (and (>= value (- expected error))
            (<= value (+ expected error)))))

    ((test-approximate expected expr error)
     (test-approximate "" expected expr error))))

(define (expect-exception thunk error-predicate)
  (guard (e
          ((or (eq? error-predicate #t)
               (error-predicate e))
           'pass)
          (else
           'incorrect-exception))
    (thunk)
    'no-exception))

(define-syntax test-error
  (syntax-rules ()
    ((test-error name error-predicate expr)
     (execute-test-case! (test-runner-get)
                         (lambda ()
                           (expect-exception (lambda () expr) error-predicate))
                         name
                         'expr))

    ((test-error error-predicate expr)
     (test-error "" error-predicate expr))

    ((test-error expr)
     (test-error "" #t expr))))

(define (test-match-name name)
  (lambda (runner)
    (equal? (test-runner-test-name runner) name)))

(define test-match-nth
  (case-lambda
   ((n)
    (test-match-nth n 1))

   ((n count)
    (let ((counter 0))
      (lambda (runner)
        (set! counter (+ counter 1))
        (<= n counter (+ n count -1)))))))

(define (evaluate-specifiers runner specifiers)
  (map (lambda (s) (s runner)) specifiers))

(define (test-match-any . specifiers)
  (lambda (runner)
    (any values (evaluate-specifiers runner specifiers))))

(define (test-match-all . specifiers)
  (let ((specifiers* (map coerce-specifier specifiers)))
    (lambda (runner)
      (every values (evaluate-specifiers runner specifiers*)))))

(define (integer->specifier n)
  (test-match-nth 1 n))

(define (string->specifier s)
  (test-match-name s))

(define (coerce-specifier s)
  (cond ((integer? s)
         (integer->specifier s))
        ((string? s)
         (string->specifier s))
        (else s)))

(define (test-skip specifier)
  (let ((runner (test-runner-get)))
    (test-runner-skip-specifiers! runner (cons (coerce-specifier specifier)
                                               (test-runner-skip-specifiers runner)))))

(define (test-expect-fail specifier)
  (let ((runner (test-runner-get)))
    (test-runner-expect-fail-specifiers! runner (cons (coerce-specifier specifier)
                                                      (test-runner-expect-fail-specifiers runner)))))

(define (any-specifier-matches? runner specifiers)
  (any values (evaluate-specifiers runner specifiers)))

(define (in-skip-list? runner)
  (any-specifier-matches? runner (test-runner-skip-specifiers runner)))

(define (in-run-list? runner)
  (let ((list (test-runner-run-specifiers runner)))
    (or (null? list)
        (any-specifier-matches? runner list))))

(define (skip? runner)
  (or (in-skip-list? runner) (not (in-run-list? runner))))

(define (expected-result runner)
  (if (any-specifier-matches? runner (test-runner-expect-fail-specifiers runner))
      'fail
      'pass))

(define (test-runner-get-or-create)
  (unless (test-runner-current)
    (test-runner-install!))
  (test-runner-current))

(define (extract-specifiers-and-procedure args)
  (let loop ((a args) (specifiers '()))
    (cond ((null? a)
           (error "No procedure given to test-apply"))
          ((null? (cdr a))
           (values (map coerce-specifier specifiers) (car a)))
          (else
           (loop (cdr a) (cons (car a) specifiers))))))

(define (test-apply* runner args)
  (let-values (((specifiers proc) (extract-specifiers-and-procedure args)))
    (let ((old-run-specifiers (test-runner-run-specifiers runner)))
      (parameterize ((test-runner-current runner))
        (dynamic-wind
          (lambda () (test-runner-run-specifiers! runner specifiers))
          proc
          (lambda () (test-runner-run-specifiers! runner old-run-specifiers)))))))

(define (test-apply first . rest)
  (cond ((test-runner? first)
         (test-apply* first rest))
        (else
         (test-apply* (test-runner-get-or-create) (cons first rest)))))

(define-syntax test-with-runner
  (syntax-rules ()
    ((test-with-runner runner expr0 expr ...)
     (parameterize ((test-runner-current runner))
       expr0 expr ...))))

(define (colour-escape number)
  (string-append "\x1B;[" (number->string number) "m"))

(define reset-colour 0)
(define red-colour 31)
(define green-colour 32)

(define (colour->escape-parameter colour)
  (case colour
    ((#f) reset-colour)
    ((red) red-colour)
    ((green) green-colour)
    (else "Unsupported colour" colour)))

(define (call-with-colour colour thunk)
  (dynamic-wind
    (lambda () (display (colour-escape (colour->escape-parameter colour))))
    thunk
    (lambda () (display (colour-escape reset-colour)))))

(define (display/colour colour string)
  (call-with-colour colour (lambda () (display string))))

;; Local variables:
;; eval: (put 'dynamic-wind 'scheme-indent-function 0)
;; eval: (put 'guard 'scheme-indent-function 1)
;; End:
