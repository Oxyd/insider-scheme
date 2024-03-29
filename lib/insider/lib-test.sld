(define-library (insider lib-test)
  (import (insider basic-procedures-test)
          (insider bytevector-test)
          (insider char-test)
          (insider control)
          (insider control-test)
          (insider format-test)
          (insider include-test)
          (insider io-test)
          (insider list-test)
          (insider numeric-test)
          (insider scribble-test)
          (insider string-test)
          (insider struct-test)
          (insider syntax)
          (insider syntax-rules-test)
          (insider syntax-test)
          (insider test)
          (insider test-test)
          (insider vector-test))
  (export test-lib)

  (begin
    (define (test-lib)
      (test-group "library"
        (test-basic-procedures)
        (test-bytevector)
        (test-char)
        (test-control)
        (test-format)
        (test-include)
        (test-io)
        (test-list)
        (test-numeric)
        (run-scribble-tests)
        (test-string)
        (test-struct)
        (test-syntax)
        (test-syntax-transformers)
        (test-test)
        (test-vector)))

    (when-main-module
     (test-lib))))
