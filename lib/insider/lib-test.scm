(library (insider lib-test))
(import (insider syntax) (insider control) (insider test)
        (insider basic-procedures-test) (insider numeric-test)
        (insider opt-lambda-test) (insider string-test)
        (insider syntax-rules-test) (insider syntax-test))
(export test-lib)

(define (test-lib)
  (test-group "library"
    (test-basic-procedures)
    (test-numeric)
    (test-opt-lambda)
    (test-string)
    (test-syntax-transformers)
    (test-syntax)))

(when-main-module
 (test-lib))
