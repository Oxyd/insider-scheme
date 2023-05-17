(define-library (scheme lazy)
  (import (insider syntax) (insider record) (insider basic-procedures))
  (export delay-force delay force make-promise promise?)
  (include "lazy.scm"))
