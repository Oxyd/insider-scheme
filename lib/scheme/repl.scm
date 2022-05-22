(library (scheme repl))
(import (insider syntax) (insider control))
(export interaction-environment)

(define (interaction-environment)
  (interactive-environment (interaction-environment-specifier)))
