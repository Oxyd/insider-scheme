(define-library (insider format)
  (import (scheme complex) (insider string) (insider char) (scheme inexact)
          (except (scheme base) read-char)
          (except (insider io) read-char))
  (export print-formatted format)
  (include "format.scm"))