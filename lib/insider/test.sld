(define-library (insider test)
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
   test-assert test-eqv test-equal test-eq test-approximate test-error
   test-values-equal

   test-apply test-with-runner

   test-read-eval-string

   ;; Non-standard extensions:
   test test-false)
  (include "test.scm"))
