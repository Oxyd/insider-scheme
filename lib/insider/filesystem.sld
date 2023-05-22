(define-library (insider filesystem)
  (import (only (insider internal)
                file-exists? delete-file current-path set-current-path!))
  (export file-exists? delete-file current-path set-current-path!))
