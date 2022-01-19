(library (insider lib-test))
(import (insider syntax) (insider control) (insider test)
        (insider basic-procedures-test) (insider bytevector-test)
        (insider char-test) (insider list-test) (insider numeric-test)
        (insider opt-lambda-test) (insider string-test)
        (insider syntax-rules-test) (insider syntax-test)
        (insider vector-test))
(export test-lib)

(define (test-lib)
  (test-group "library"
    (test-basic-procedures)
    (test-bytevector)
    (test-char)
    (test-list)
    (test-numeric)
    (test-opt-lambda)
    (test-string)
    (test-syntax-transformers)
    (test-syntax)
    (test-vector)))

(when-main-module
 (test-lib))
