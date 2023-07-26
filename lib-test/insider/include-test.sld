(define-library (insider include-test)
  (import (insider syntax) (insider include) (insider test) (insider control))
  (export test-include)
  (include "include-test.scm"))
