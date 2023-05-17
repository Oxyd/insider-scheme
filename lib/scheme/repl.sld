(define-library (scheme repl)
  (import (insider syntax) (insider control))
  (export interaction-environment)
  (begin
    (define (interaction-environment)
      (interactive-environment (interaction-environment-specifier)))))
