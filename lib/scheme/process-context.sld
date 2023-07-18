(define-library (scheme process-context)
  (import (rename (only (insider internal)
                        command-line get-environment-variable
                        get-environment-variables exit emergency-exit)
                  (exit %exit)
                  (emergency-exit %emergency-exit))
          (scheme base))
  (export command-line get-environment-variable get-environment-variables
          exit emergency-exit)
  (begin
    (define (exit (result #t))
      (%exit result))

    (define (emergency-exit (result #t))
      (%emergency-exit result))))
