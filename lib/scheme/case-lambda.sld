(define-library (scheme case-lambda)
  (import (insider syntax) (insider basic-procedures) (insider error)
          (insider control) (insider list)
          (only (insider internal) =))
  (export case-lambda)
  (include "case-lambda.scm"))
