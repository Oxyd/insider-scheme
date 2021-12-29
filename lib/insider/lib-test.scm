(library (insider lib-test))
(import (insider syntax) (insider control) (insider test)
        (insider basic-procedures-test) (insider syntax-rules-test) (insider syntax-test))
(export test-lib)

(define (test-lib)
  (test-group "library"
    (test-basic-procedures)
    (test-syntax-transformers)
    (test-syntax)))

(when-main-module
 (test-lib))
