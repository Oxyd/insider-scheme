(define-library (scheme load)
  (import (scheme base) (scheme read) (scheme eval) (scheme repl) (scheme file))
  (export load)
  (include "load.scm"))
