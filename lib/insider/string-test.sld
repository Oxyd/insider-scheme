(define-library (insider string-test)
  (import (insider syntax) (insider string) (insider test) (insider control)
          (insider char) (insider numeric) (insider list)
          (insider basic-procedures))
  (export test-string)
  (include "string-test.scm"))
