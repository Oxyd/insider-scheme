(define (test-eval)
  (test-group "test-eval"
    (test-equal 7 (test-read-eval-string "(+ 3 4)"))
    (test-error (test-read-eval-string "(+ 3 4"))
    (test-error (test-read-eval-string "(+ 3 4) "))
    (test-equal #\newline (test-read-eval-string "#\\newline"))
    (test-error (test-read-eval-string "#\\newlin"))))

(define (test-test)
  (test-group "test"
    (test-eval)))

(when-main-module
 (test-test))
