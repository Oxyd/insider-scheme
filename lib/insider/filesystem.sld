(define-library (insider filesystem)
  (import (only (insider internal)
                file-exists? delete-file current-working-directory
                set-current-working-directory!))
  (export file-exists? delete-file current-working-directory
          set-current-working-directory!))
