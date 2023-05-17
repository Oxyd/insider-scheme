(define-library (insider include)
  (import (insider io)
          (insider syntax)
          (insider error)
          (insider control)
          (insider string)
          (insider basic-procedures)
          (insider list)
          (insider numeric)
          (insider vector))
  (export include include-ci)
  (include "include.scm"))
